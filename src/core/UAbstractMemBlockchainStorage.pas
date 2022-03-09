unit UAbstractMemBlockchainStorage;

{ Copyright (c) 2022 by Albert Molina

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

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

{$I ./../config.inc}

uses
  Classes, SysUtils, UBlockchain, UThread, UCrypto, math, UAccounts, ULog,
  SyncObjs,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF},
  UCommon,
  UBaseTypes, UPCDataTypes,
  UAbstractMem, UFileMem, UCacheMem,
  UAbstractMemBTree, UOrderedList,
  UFileStorage,
  UConst;

type
  EAbstractMemBlockchainStorage = Class(Exception);

  TBlockchainStorageStats = record
    blockInformationCount : Int64;
    operationRawDataCount : Int64;
    affectedAccountCount : Int64;
    startTC : TTickCount;
    procedure Clear;
    function ToString : String;
    function ThroughputPerSecond : Double;
    procedure AddTo(var ADest : TBlockchainStorageStats);
  end;
  PBlockchainStorageStats = ^TBlockchainStorageStats;

  { TAbstractMemBlockchainStorage }

  TAbstractMemBlockchainStorage = Class(TStorage)
  private
    FFileMem : TFileMem;
    FStorageLock : TCriticalSection;
    FAutoFlushCache: Boolean;
    FUseMultithread: Boolean;
    FSaveStorageStats : TBlockchainStorageStats;
    FPSaveStorageStats: PBlockchainStorageStats;
    procedure SetUseMultithread(const Value: Boolean);
    type
      TOrphanInformation = record
        orphan : string;
        regsCounter : Integer;
        procedure Clear;
        procedure CopyFrom(const ASource : TOrphanInformation);
      end;
      TAMBTreeOrphanInformationByOrphan = Class(TAbstractMemBTreeData<TOrphanInformation>)
      protected
        function LoadData(const APosition : TAbstractMemPosition) : TOrphanInformation; override;
        function SaveData(const AData : TOrphanInformation) : TAMZone; override;
      public
        procedure Update(const AOrphan : String; AIncrement : Integer);
        function GetRegsCountByOrphan(const AOrphan : String) : Integer;
      End;

      TBlockInformation = record
        operationBlock : TOperationBlock;
        orphan : String;
        operationsCount : Integer;
        volume : Int64;
        rawDataPosition : TAbstractMemPosition;
        procedure Clear;
        procedure SetToFindByBlock(ABlock : Integer);
        function ToSerialized : TBytes;
        function FromSerialized(ABytes : TBytes) : Boolean;
        function GetRawData(AAbstractMem : TAbstractMem; var ARawData : TBytes) : Boolean;
        function CreateTPCOperationsComp(AAbstractMem : TAbstractMem; ABank: TPCBank) : TPCOperationsComp;
        procedure ReadTPCOperationsComp(AAbstractMem : TAbstractMem; AOperationsComp : TPCOperationsComp);
        function IsOrphan(const AOrphan : String) : Boolean;
        procedure CopyFrom(const ASource : TBlockInformation);
      end;
      TAMBTreeOperationBlockInformationByOrphanBlock = Class(TAbstractMemBTreeData<TBlockInformation>)
      protected
        function LoadData(const APosition : TAbstractMemPosition) : TBlockInformation; override;
        function SaveData(const AData : TBlockInformation) : TAMZone; override;
        procedure DeletedData(const AData: TBlockInformation); override;
      public
        function GetBlockInformationByBlock(ABlock : Integer) :  TBlockInformation;
      End;

      TOperationRawData = record
        rightOpHash : TBytes;
        account : Integer;
        n_operation : Integer;
        block : Integer;
        opblock : Integer;
        opType : Integer;
        opSavedProtocol : Integer;
        rawData : TBytes;
        procedure Clear;
        procedure SetToFindByRightOpHash(const ARightOpHash : TBytes);
        procedure SetToFindByBlockOpblock(ABlock, AOpblock : Integer);
        function ToSerialized : TBytes;
        function FromSerialized(ABytes : TBytes) : Boolean;
        procedure CopyFrom(const ASource : TOperationRawData);
        function CreateTPCOperation(AAbstractMem : TAbstractMem) : TPCOperation; overload;
        function CreateTPCOperation(AAbstractMem : TAbstractMem; out APCOperation : TPCOperation) : Boolean; overload;
      end;
      TAMBTreeTOperationRawDataByRightOpHash = Class(TAbstractMemBTreeData<TOperationRawData>)
      protected
        function LoadData(const APosition : TAbstractMemPosition) : TOperationRawData; override;
        function SaveData(const AData : TOperationRawData) : TAMZone; override;
      End;
      TAMBTreeTOperationRawDataByBlockOpBlock_Index = Class(TAbstractMemBTreeDataIndex<TOperationRawData>)
      End;

      TAffectedAccount = record
        account : Integer;
        n_operation : Integer;
        block : Integer;
        opblock : Integer;
        procedure Clear;
        procedure SetToFindByAccount(AAccount : Integer);
        procedure SetToFindByAccountBlockOpblock(AAccount, ABlock, AOpblock : Integer);
        function ToSerialized : TBytes;
        function FromSerialized(ABytes : TBytes) : Boolean;
        procedure CopyFrom(const ASource : TAffectedAccount);
        function ToString:String;
      end;
      TAMBTreeTAffectedAccountByAccountBlockOpBlock = Class(TAbstractMemBTreeData<TAffectedAccount>)
      protected
        function LoadData(const APosition : TAbstractMemPosition) : TAffectedAccount; override;
        function SaveData(const AData : TAffectedAccount) : TAMZone; override;
      End;

      TPendingData = record
        operation : TOperationRawData;
        affectedAccounts : Array of TAffectedAccount;
        procedure Clear;
      end;

      TPendingToSaveThread = Class;

      TPendingToSave = Class
      private
        FAMStorage : TAbstractMemBlockchainStorage;
        FMaxThreads : Integer;
        FPending : TThreadList<TPendingData>;
        FThreads : TThreadList<TPendingToSaveThread>;
        FOperationsRawData_By_RightOpHash : TAMBTreeTOperationRawDataByRightOpHash;
        FAffectedAccounts_By_Account_Block_OpBlock : TAMBTreeTAffectedAccountByAccountBlockOpBlock;
        FTotal: Integer;
        FMaxPendingsCount: Integer;
        FLastLogTC : TTickCount;
        procedure SetMaxThreads(const Value: Integer);
      protected
        procedure ThreadHasFinishedCurrentJob;
      public
        procedure AddPendingData(const APendingData : TPendingData);
        constructor Create(AStorage : TAbstractMemBlockchainStorage; AAMBTreeTOperationRawDataByRightOpHash : TAMBTreeTOperationRawDataByRightOpHash;
          AAMBTreeTAffectedAccountByAccountBlockOpBlock : TAMBTreeTAffectedAccountByAccountBlockOpBlock);
        destructor Destroy; override;
        property MaxThreads : Integer read FMaxThreads write SetMaxThreads;
        function PendingsCount : Integer;
        property Total : Integer read FTotal write FTotal;
        property MaxPendingsCount : Integer read FMaxPendingsCount write FMaxPendingsCount;
      End;

      TPendingToSaveThread = Class(TPCThread)
      private
        FPendingToSave : TPendingToSave;
        FBusy: Boolean;
      protected
        procedure BCExecute; override;
      public
        Constructor Create(APendingToSave : TPendingToSave);
        property Busy : Boolean read FBusy write FBusy;
      End;

    var
    FOrphansInformation_By_Orphan : TAMBTreeOrphanInformationByOrphan;
    FBlocksInformation_By_OrphanBlock : TAMBTreeOperationBlockInformationByOrphanBlock;
    FOperationsRawData_By_RightOpHash : TAMBTreeTOperationRawDataByRightOpHash;
    FOperationsRawData_By_Block_OpBlock_Index : TAMBTreeTOperationRawDataByBlockOpBlock_Index;
    FAffectedAccounts_By_Account_Block_OpBlock : TAMBTreeTAffectedAccountByAccountBlockOpBlock;

    FPendingToSave : TPendingToSave;
    FCheckingConsistency : Boolean;
    FCheckingConsistencyProgress : String;
    FCheckingConsistencyStats : TBlockchainStorageStats;
    FLogSaveActivity : Boolean;
    FInBlockNotFound : Boolean;
    FInBlockSaving : Integer;

    function GetFirstBlockNumberByOrphan(const AOrphan : String): Int64;
    function GetLastBlockNumberByOrphan(const AOrphan : String): Int64;
    Function DoBlockExistsByOrphan(ABlock : Integer; const AOrphan : String; var LBlockInformation : TBlockInformation) : Boolean;

    function DeleteBlockChainBlockExt(ABlock : Integer; const AOrphan : String) : Boolean;
    Function DoSaveBlockChainExt(Operations : TPCOperationsComp; const AOrphan : String; var AStats : TBlockchainStorageStats) : Boolean;
    Function DoLoadBlockChainExt(Operations : TPCOperationsComp; Block : Cardinal; const AOrphan : String) : Boolean;
    procedure AddMessage(AMessages : TStrings; const AMessage : String; ARaiseAnException : Boolean);
  protected
    procedure SetReadOnly(const Value: Boolean); override;
    Function DoGetBlockInformation(const ABlock : Integer; var AOperationBlock : TOperationBlock; var AOperationsCount : Integer; var AVolume : Int64) : Boolean; override;

    Function DoLoadBlockChain(Operations : TPCOperationsComp; Block : Cardinal) : Boolean; override;
    Function DoSaveBlockChain(Operations : TPCOperationsComp) : Boolean; override;
    Function DoMoveBlockChain(StartBlock : Cardinal; Const DestOrphan : TOrphan) : Boolean; override;
    Procedure DoDeleteBlockChainBlocks(StartingDeleteBlock : Cardinal); override;
    Function DoBlockExists(Block : Cardinal) : Boolean; override;
    function GetFirstBlockNumber: Int64; override;
    function GetLastBlockNumber: Int64; override;
    function DoInitialize : Boolean; override;
    Procedure DoEraseStorage; override;
    function CheckBlockConsistency(ARaiseOnError : Boolean; AMessages : TStrings; const ABlockInformation : TBlockInformation; out AOperationsCount, AAffectedAccountsCount : Integer; AThread : TPCThread) : Boolean;

    procedure DoBlockNotFound(ABlock : Integer); virtual;
    procedure BlockNotFound(ABlock : Integer);

    Function DoGetBlockOperations(ABlock, AOpBlockStartIndex, AMaxOperations : Integer; var AOperationBlock : TOperationBlock; var AOperationsCount : Integer; var AVolume : Int64; const AOperationsResumeList:TOperationsResumeList) : Boolean; override;
    Function DoGetAccountOperations(AAccount : Integer; AMaxDepth, AStartOperation, AMaxOperations, ASearchBackwardsStartingAtBlock: Integer; const AOperationsResumeList:TOperationsResumeList): Boolean; override;
    function DoFindOperation(const AOpHash : TBytes; var AOperationResume : TOperationResume) : TSearchOpHashResult; override;
    Function DoGetOperation(const ABlock, AOpBlock : Integer; const AOperations : TOperationsHashTree) : Boolean;
  public
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
    procedure FinalizedUpdating;
    procedure CheckConsistency(ARaiseOnError: Boolean; AMessages: TStrings; AThread : TPCThread); overload;
    function CheckConsistency(const AOrphan : String; AFromBlock , AToBlock : Integer; ARaiseOnError : Boolean; AMessages : TStrings; out ABlocksFound, AOperationsFound, AAffectedAccountsFound : Integer; AThread : TPCThread) : Boolean; overload;
    property FileMem : TFileMem read FFileMem;
    property AutoFlushCache : Boolean read FAutoFlushCache write FAutoFlushCache;

    Procedure FillInfo(AStrings : TStrings);
    class function OrphanCompare(const ALeft, ARight : String) : Integer; inline;
    function PendingToSave : Integer;
    procedure AbortPendingToSave;
    property UseMultithread : Boolean read FUseMultithread write SetUseMultithread;

    property CheckingConsistency : Boolean read FCheckingConsistency;
    property CheckingConsistencyProgress : String read FCheckingConsistencyProgress;
    property CheckingConsistencyStats : TBlockchainStorageStats read FCheckingConsistencyStats;
    property SaveStorageStats : PBlockchainStorageStats read FPSaveStorageStats;
    property LogSaveActivity : Boolean read FLogSaveActivity write FLogSaveActivity;
  End;


  TAbstractMemBlockchainStorageSecondary = Class(TAbstractMemBlockchainStorage)
  private
    FAuxStorage : TStorage;
  protected
    procedure SetReadOnly(const Value: Boolean); override;
    Function DoSaveBlockChain(Operations : TPCOperationsComp) : Boolean; override;
    Function DoMoveBlockChain(StartBlock : Cardinal; Const DestOrphan : TOrphan) : Boolean; override;
    Procedure DoDeleteBlockChainBlocks(StartingDeleteBlock : Cardinal); override;
    function DoInitialize : Boolean; override;
    procedure DoBlockNotFound(ABlock : Integer); override;
  public
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
    property AuxStorage : TStorage read FAuxStorage;
  End;


implementation


function Comparer_TOrphanInformation_By_Orphan(const ALeft, ARight : TAbstractMemBlockchainStorage.TOrphanInformation) : Integer;
begin
  Result := TAbstractMemBlockchainStorage.OrphanCompare(ALeft.orphan,ARight.orphan);
end;

function Comparer_TBlockInformation_By_OrphanBlock(const ALeft, ARight : TAbstractMemBlockchainStorage.TBlockInformation) : Integer;
begin
  Result := TAbstractMemBlockchainStorage.OrphanCompare(ALeft.orphan,ARight.orphan);
  if Result=0 then begin
    if ALeft.operationBlock.block<ARight.operationBlock.block then Result := -1
    else if ALeft.operationBlock.block>ARight.operationBlock.block then Result := 1
    else Result := 0;
  end;
end;

function Comparer_TOperationRawData_By_RightOpHash(const ALeft, ARight : TAbstractMemBlockchainStorage.TOperationRawData) : Integer;
begin
  Result := BytesCompare(ALeft.rightOpHash,ARight.rightOpHash);
end;

function Comparer_TOperationRawData_By_Block_OpBlock(const ALeft, ARight : TAbstractMemBlockchainStorage.TOperationRawData) : Integer;
begin
  Result := ALeft.block - ARight.block;
  if Result=0 then Result := ALeft.opblock - ARight.opblock;
end;

function Comparer_TAffectedAccount_By_Account_Block_OpBlock(const ALeft, ARight : TAbstractMemBlockchainStorage.TAffectedAccount) : Integer;
begin
  Result := ALeft.account - ARight.account;
  if Result=0 then Result := ALeft.block - ARight.block;
  if Result=0 then Result := ALeft.opblock - ARight.opblock;
end;


{ TAbstractMemBlockchainStorage }

procedure TAbstractMemBlockchainStorage.AbortPendingToSave;
begin
  FreeAndNil(FPendingToSave);
end;

procedure TAbstractMemBlockchainStorage.AddMessage(AMessages: TStrings; const AMessage: String; ARaiseAnException : Boolean);
begin
  if Assigned(AMessages) then AMessages.Add(AMessage);
  if ARaiseAnException then raise EAbstractMemBlockchainStorage.Create(AMessage);
  TLog.NewLog(ltinfo,ClassName,AMessage);
  FCheckingConsistencyProgress := AMessage;
end;

procedure TAbstractMemBlockchainStorage.BlockNotFound(ABlock: Integer);
begin
  if FInBlockNotFound then begin
    TLog.NewLog(ltdebug,ClassName,Format('BlockNotFound cannot save Block:%d because saving block:%d',[ABlock,FInBlockSaving]));
    Exit;
  end;
  FInBlockNotFound := True;
  try
    FInBlockSaving := ABlock;
    DoBlockNotFound(ABlock);
  finally
    FInBlockNotFound := False;
  end;
end;

function TAbstractMemBlockchainStorage.CheckBlockConsistency(
  ARaiseOnError: Boolean; AMessages: TStrings; const ABlockInformation : TBlockInformation; out AOperationsCount, AAffectedAccountsCount : Integer;
  AThread : TPCThread): Boolean;
var LErrorsCount : Integer;
  procedure AddInfo(AIsError : Boolean; const AMessage : String);
  Var LTxt : String;
  begin
    if AIsError then begin
      Result := False;
      inc(LErrorsCount);
      LTxt := 'ERROR: '+AMessage;
    end else LTxt := AMessage;
    AddMessage(AMessages,LTxt,(AIsError and ARaiseOnError));
  end;
var
  LOperationsCounter : Integer;
  LOperationRawDataToSearch, LOperationRawData,
  LOperationRawDataAux, LOperationRawDataAuxFound : TOperationRawData;
  LOperationsComp : TPCOperationsComp;
  LOperation : TPCOperation;
  LRawData : TBytes;
  LError : String;
  LPos : TAbstractMemPosition;
  LAffectedAccountsList : TOrderedList<Cardinal>;
  LAffectedAccount, LAffectedAccountFound : TAffectedAccount;
  i : Integer;
begin
  Result := True;
  LErrorsCount := 0;
  AAffectedAccountsCount := 0;
  AOperationsCount := 0;
  if Not ABlockInformation.GetRawData(FFileMem,LRawData) then begin
    AddInfo(True,Format('Cannot obtain raw data from block %d at pos %d',[AblockInformation.operationBlock.block,ABlockInformation.rawDataPosition]));
  end;
  LOperationsComp := ABlockInformation.CreateTPCOperationsComp(FFileMem,Nil);
  Try
    //
    if LOperationsComp.Count<>ABlockInformation.operationsCount then begin
      AddInfo(True,Format('Block %d operations count not equal %d <> %d',[AblockInformation.operationBlock.block,AblockInformation.operationsCount,LOperationsComp.Count]));
    end;
    if LOperationsComp.OperationsHashTree.TotalAmount<>ABlockInformation.volume then begin
      AddInfo(True,Format('Block %d volume not equal %d <> %d',[AblockInformation.operationBlock.block,AblockInformation.volume,LOperationsComp.OperationsHashTree.TotalAmount]));
    end;
    //
    LOperationsCounter := 0;
    LOperationRawDataToSearch.Clear;
    LOperationRawDataToSearch.block := ABlockInformation.operationBlock.block;
    LOperationRawDataToSearch.opblock := 0;
    if FOperationsRawData_By_Block_OpBlock_Index.FindData(LOperationRawDataToSearch,LPos,LOperationRawData) then begin
      repeat
        if Assigned(AThread) and AThread.Terminated then  Break;

        Inc(FCheckingConsistencyStats.operationRawDataCount);
        FCheckingConsistencyProgress := Format('Orphan "%s" Block %d Operation %d/%d',[ABlockInformation.orphan,ABlockInformation.operationBlock.block,LOperationRawData.opblock+1,ABlockInformation.operationsCount]);

        LOperationRawDataAux.CopyFrom(LOperationRawData);
        if not FOperationsRawData_By_RightOpHash.FindData(LOperationRawDataAux,LPos,LOperationRawDataAuxFound) then begin
          AddInfo(True,Format('Block %d operation %d not found by searching by OpHash',[LOperationRawData.block,LOperationRawData.opblock]));
        end;
        //
        LOperation := LOperationRawData.CreateTPCOperation(FFileMem);
        Try
          if (Not BytesEqual(LOperationRawData.rightOpHash,LOperation.RipeMD160)) then raise EAbstractMemBlockchainStorage.Create('ERR 20211116-1');
          if LOperationRawData.account<>LOperation.SignerAccount then raise EAbstractMemBlockchainStorage.Create('ERR 20211116-2');
          if LOperationRawData.n_operation<>LOperation.N_Operation then raise EAbstractMemBlockchainStorage.Create('ERR 20211116-3');
          //
          if (Not BytesEqual(LOperation.RipeMD160,LOperationsComp.Operation[LOperationRawData.opblock].RipeMD160)) then raise EAbstractMemBlockchainStorage.Create('ERR 20211116-4');
          // Check affected accounts:
          LAffectedAccountsList := TOrderedList<Cardinal>.Create(False,TComparison_Cardinal);
          Try
            LOperation.AffectedAccounts(LAffectedAccountsList);
            for i := 0 to LAffectedAccountsList.Count-1 do begin
              LAffectedAccount.Clear;
              LAffectedAccount.account := LAffectedAccountsList.Get(i);
              LAffectedAccount.n_operation := LOperation.GetAccountN_Operation(LAffectedAccount.account);
              LAffectedAccount.block := ABlockInformation.operationBlock.block;
              LAffectedAccount.opblock := LOperationRawData.opblock;

              Inc(FCheckingConsistencyStats.affectedAccountCount);
              FCheckingConsistencyProgress := Format('Orphan "%s" Block %d Operation %d/%d Account %d/%d',
                 [ABlockInformation.orphan,ABlockInformation.operationBlock.block,LOperationRawData.opblock+1,ABlockInformation.operationsCount,
                  i+1,LAffectedAccountsList.Count]);

              if Not FAffectedAccounts_By_Account_Block_OpBlock.FindData(LAffectedAccount,LPos,LAffectedAccountFound) then begin
                AddInfo(True,Format('Affected account %d (%d/%d) for block %d opblock %d/%d not found',[LAffectedAccount.account,
                  i+1,LAffectedAccountsList.Count, LAffectedAccount.block, LAffectedAccount.opblock+1, ABlockInformation.operationsCount]));
              end else begin
                if LAffectedAccountFound.n_operation <> LOperation.GetAccountN_Operation(LAffectedAccount.account) then begin
                  AddInfo(True,Format('Invalid n_operation %d for account %d (%d/%d) for block %d opblock %d/%d',[LAffectedAccountFound.n_operation, LAffectedAccount.account,
                    i+1,LAffectedAccountsList.Count, LAffectedAccount.block, LAffectedAccount.opblock+1, ABlockInformation.operationsCount]));
                end;
              end;
            end;
            inc(AAffectedAccountsCount, LAffectedAccountsList.Count);
          Finally
            LAffectedAccountsList.Free;
          End;
        Finally
          LOperation.Free;
        End;
        //
        inc(LOperationsCounter);
        Inc(LOperationRawDataToSearch.opblock);
      until Not (FOperationsRawData_By_Block_OpBlock_Index.FindData(LOperationRawDataToSearch,LPos,LOperationRawData));
    end;
    if LOperationsCounter<>ABlockInformation.operationsCount then begin
      AddInfo(True,Format('Block %d has %d operations but %d was found',[ABlockInformation.operationBlock.block,ABlockInformation.operationsCount,LOperationsCounter]));
    end;
    AOperationsCount := LOperationsCounter;
  Finally
    LOperationsComp.Free;
  End;
end;

procedure TAbstractMemBlockchainStorage.CheckConsistency(ARaiseOnError: Boolean; AMessages: TStrings; AThread : TPCThread);
var LOrphans, LSearch : TOrphanInformation;
  LOrphansCount, LBlocksFound, LOperationsFound, LAffectedAccountsFound,
  Ltemp1, Ltemp2, Ltemp3 : Integer;
  LMyOrphanFound : Boolean;
begin
  if FCheckingConsistency then begin
    if ARaiseOnError then raise EAbstractMemBlockchainStorage.Create('Checking consistency in process...');
    Exit;
  end;
  LMyOrphanFound := False;
  LBlocksFound := 0;
  LOperationsFound := 0;
  LAffectedAccountsFound := 0;
  LOrphansCount := 0;
  LOrphans.Clear;
  AddMessage(AMessages,Format('Start CheckConsistency process for My Orphan "%s"',[Orphan]),False);
  if FOrphansInformation_By_Orphan.FindDataLowest(LOrphans) then begin
    repeat
      if Assigned(AThread) and (AThread.Terminated) then Break;
      inc(LOrphansCount);
      AddMessage(AMessages,Format('Start analyzing orphan "%s" with %d registers',[LOrphans.orphan,LOrphans.regsCounter]),False);
      if not CheckConsistency(LOrphans.orphan,-1,-1,ARaiseOnError,AMessages,Ltemp1,Ltemp2,Ltemp3,AThread) then begin
        AddMessage(AMessages,Format('Errors analyzing orphan "%s"',[LOrphans.orphan]),ARaiseOnError);
      end;
      inc(LBlocksFound,Ltemp1);
      inc(LOperationsFound,Ltemp2);
      inc(LAffectedAccountsFound,Ltemp3);
      LMyOrphanFound := LMyOrphanFound or (OrphanCompare(LOrphans.orphan,Orphan)=0);
      LSearch := LOrphans;
    until Not (FOrphansInformation_By_Orphan.FindDataSuccessor(LSearch,LOrphans));
  end;
  if not LMyOrphanFound then begin
    AddMessage(AMessages,Format('Warning: My orphan "%s" not found in list!',[Self.Orphan]),False);
    //
    if Not CheckConsistency(Self.Orphan,-1,-1,ARaiseOnError,AMessages,Ltemp1,Ltemp2,Ltemp3,AThread) then begin
      AddMessage(AMessages,Format('Errors analyzing My orphan "%s"',[Self.Orphan]),ARaiseOnError);
    end;
    inc(LBlocksFound,Ltemp1);
    inc(LOperationsFound,Ltemp2);
    inc(LAffectedAccountsFound,Ltemp3);
  end;
  if (LBlocksFound<>FBlocksInformation_By_OrphanBlock.Count) then begin
    AddMessage(AMessages,Format('Error: Found %d blocks but expected %d',[LBlocksFound,FBlocksInformation_By_OrphanBlock.Count]),ARaiseOnError);
  end;
  if (LOperationsFound<>FOperationsRawData_By_RightOpHash.Count) then begin
    AddMessage(AMessages,Format('Error: Found %d operations but expected %d',[LOperationsFound,FOperationsRawData_By_RightOpHash.Count]),ARaiseOnError);
  end;
  if (LAffectedAccountsFound<>FAffectedAccounts_By_Account_Block_OpBlock.Count) then begin
    AddMessage(AMessages,Format('Error: Found %d accounts but expected %d',[LAffectedAccountsFound,FAffectedAccounts_By_Account_Block_OpBlock.Count]),ARaiseOnError);
  end;
  if (FOperationsRawData_By_RightOpHash.Count<>FOperationsRawData_By_Block_OpBlock_Index.Count) then begin
    AddMessage(AMessages,Format('Error: Indexes for operations %d not %d',[FOperationsRawData_By_RightOpHash.Count,FOperationsRawData_By_Block_OpBlock_Index.Count]),ARaiseOnError);
  end;


  AddMessage(AMessages,Format('Finalized analyzing orphans with %d orphans %d blocks %d operations and %d accounts',
    [LOrphansCount,LBlocksFound,LOperationsFound,LAffectedAccountsFound]),False);
end;

function TAbstractMemBlockchainStorage.CheckConsistency(const AOrphan: String;
  AFromBlock, AToBlock: Integer;
  ARaiseOnError: Boolean; AMessages: TStrings;
  out ABlocksFound, AOperationsFound, AAffectedAccountsFound : Integer; AThread : TPCThread): Boolean;
var LMessages : TStringList;
  LErrorsCount : Integer;
  procedure AddInfo(AIsError : Boolean; const AMessage : String);
  Var LTxt : String;
  begin
    if AIsError then begin
      Result := False;
      inc(LErrorsCount);
      LTxt := 'ERROR: '+AMessage;
      if ARaiseOnError then raise Exception.Create(Self.ClassName+' not consistent: '+LTxt);
    end else LTxt := 'INFO: '+AMessage;
    AddMessage(AMessages,Ltxt,AIsError and ARaiseOnError);
  end;
var LBlockInformation, LPreviousBlockInformation : TBlockInformation;
  LBlocksInformationMin,LBlocksInformationMax : Integer;
  LOperationRawData,LOperationRawDataAux,LOperationRawDataAuxFound : TOperationRawData;
  LAffectedAccount : TAffectedAccount;
  LPos : TAbstractMemPosition;

  LTempOperationsCounter, LTempAccountsCounter : Integer;
  LContinue : Boolean;
  LOrphanInformation,LOrphanInformationFound : TOrphanInformation;
  LTC,LStartTC : TTickCount;
begin
  Assert((AToBlock<0) or (AToBlock>=AFromBlock),Format('Invalid from %d to %d values',[AFromBlock,AToBlock]));
  Result := True;

  LBlocksInformationMin := 0;
  LBlocksInformationMax := 0;

  ABlocksFound := 0;
  AOperationsFound := 0;
  AAffectedAccountsFound := 0;

  if FCheckingConsistency then begin
    if ARaiseOnError then raise EAbstractMemBlockchainStorage.Create('Checking consistency in process...');
    Exit;
  end;
  FCheckingConsistencyStats.Clear;
  FCheckingConsistency := True;
  Try

  LPreviousBlockInformation.Clear;
  if (AFromBlock<0) then begin
    AFromBlock := GetFirstBlockNumberByOrphan(AOrphan);
  end;
  LTC := TPlatform.GetTickCount;
  LStartTC := LTC;
  LContinue := ((AToBlock<0) or (AFromBlock<=AToBlock)) and  (DoBlockExistsByOrphan(AFromBlock,AOrphan,LBlockInformation));
  if (LContinue) then begin
    inc(ABlocksFound);
    LPreviousBlockInformation.CopyFrom(LBlockInformation);
    // Initialize
    LBlocksInformationMin := LBlockInformation.operationBlock.block;
    LBlocksInformationMax := LBlockInformation.operationBlock.block;
    // Check operations count
    if (OrphanCompare(AOrphan,Self.Orphan)=0) then begin
      CheckBlockConsistency(ARaiseOnError,AMessages,LBlockInformation,LTempOperationsCounter,LTempAccountsCounter,AThread);
      inc(AOperationsFound,LTempOperationsCounter);
      inc(AAffectedAccountsFound,LTempAccountsCounter);
    end;
    //
    while (FBlocksInformation_By_OrphanBlock.FindDataSuccessor(LPreviousBlockInformation,LBlockInformation)) do begin
      Inc(FCheckingConsistencyStats.blockInformationCount);
      FCheckingConsistencyProgress := Format('Orphan "%s" Block %d Operations %d',[LBlockInformation.orphan,LBlockInformation.operationBlock.block,LBlockInformation.operationsCount]);
      if Assigned(AThread) and (AThread.Terminated)  then Break;

      if ((AToBlock>=0) and (AToBlock<LBlockInformation.operationBlock.block)) then break;
      if (Not LBlockInformation.IsOrphan(AOrphan)) then Break;

      if (LPreviousBlockInformation.operationBlock.block >= LBlockInformation.operationBlock.block) then begin
        AddInfo(True,Format('Previous block %d >= current block %d (DUPLICATE OR INVALID ORDER!)',[LPreviousBlockInformation.operationBlock.block,LBlockInformation.operationBlock.block]));
      end else if LPreviousBlockInformation.operationBlock.block+1 <> LBlockInformation.operationBlock.block then begin
        AddInfo(False,Format('Previous block %d+1 Not current block %d',[LPreviousBlockInformation.operationBlock.block,LBlockInformation.operationBlock.block]));
      end;

      if LBlocksInformationMax < LBlockInformation.operationBlock.block then LBlocksInformationMax := LBlockInformation.operationBlock.block;
      //
      // Check operations count
      if (OrphanCompare(AOrphan,Self.Orphan)=0) then begin
        CheckBlockConsistency(ARaiseOnError,AMessages,LBlockInformation,LTempOperationsCounter,LTempAccountsCounter,AThread);
        inc(AOperationsFound,LTempOperationsCounter);
        inc(AAffectedAccountsFound,LTempAccountsCounter);
      end;
      //
      inc(ABlocksFound);
      LPreviousBlockInformation.CopyFrom(LBlockInformation);
      if (TPlatform.GetElapsedMilliseconds(LTC)>6000) then begin
        TLog.NewLog(ltdebug,ClassName,Format('Consistency checking %d/%d elapsed %s seconds',[LBlockInformation.operationBlock.block,AToBlock,FormatFloat('0.00',TPlatform.GetElapsedMilliseconds(LStartTC)/1000)]));
        LTC := TPlatform.GetTickCount;
      end;
    end;
  end;
  if (AToBlock<0) and (GetLastBlockNumberByOrphan(AOrphan)>0) and (GetLastBlockNumberByOrphan(AOrphan)<>LBlocksInformationMax) then begin
    AddInfo(True,Format('Last block found %d not what expected %d',[LBlocksInformationMax,GetLastBlockNumberByOrphan(AOrphan)]));
  end;

  if (OrphanCompare(AOrphan,Self.Orphan)=0) then begin
    if (FOperationsRawData_By_RightOpHash.Count<>AOperationsFound) then begin
      AddInfo(True,Format('Found %d operations but stored %d operations',[AOperationsFound,FOperationsRawData_By_RightOpHash.Count]));
    end;
    if FAffectedAccounts_By_Account_Block_OpBlock.Count<>AAffectedAccountsFound then begin
      AddInfo(True,Format('Stored %d affected accounts but only %d on blocks',[FAffectedAccounts_By_Account_Block_OpBlock.Count,AAffectedAccountsFound]));
    end;
  end;
  LOrphanInformation.Clear;
  LOrphanInformation.orphan := AOrphan;
  if FOrphansInformation_By_Orphan.FindData(LOrphanInformation,LPos,LOrphanInformationFound) then begin
    if LOrphanInformationFound.regsCounter<>ABlocksFound then begin
      AddInfo((AToBlock<0),Format('Orphan information counter expected %d found %d for orphan "%s"',[LOrphanInformationFound.regsCounter,ABlocksFound,AOrphan]));
    end;
  end else begin
    AddInfo(True,Format('Not found information for Orphan "%s"',[AOrphan]));
  end;

  AddInfo(False,Format('Analyzed from block %d to %d on orphan "%s" (expected %d/%d pending %d) Operations %d Accounts %d',[
    LBlocksInformationMin,LBlocksInformationMax,
    AOrphan,
    ABlocksFound, (LBlocksInformationMax - LBlocksInformationMin + 1), (LBlocksInformationMax - LBlocksInformationMin + 1)- ABlocksFound,

    AOperationsFound,AAffectedAccountsFound]));

  Finally
    FCheckingConsistency := False;
    FCheckingConsistencyProgress := '';
  End;
end;

constructor TAbstractMemBlockchainStorage.Create(AOwner: TComponent);
begin
  inherited;
  FInBlockNotFound := False;
  FInBlockSaving := 0;
  FLogSaveActivity := True;
  FSaveStorageStats.Clear;
  FPSaveStorageStats := @FSaveStorageStats;
  FPSaveStorageStats^.clear;
  FCheckingConsistency := False;
  FCheckingConsistencyProgress := '';
  FCheckingConsistencyStats.Clear;
  FUseMultithread := True;
  FFileMem := Nil;
  FOrphansInformation_By_Orphan := Nil;
  FBlocksInformation_By_OrphanBlock := Nil;
  FOperationsRawData_By_RightOpHash := Nil;
  FOperationsRawData_By_Block_OpBlock_Index := Nil;
  FAffectedAccounts_By_Account_Block_OpBlock := Nil;
  FAutoFlushCache := True;
  FStorageLock := TCriticalSection.Create;
  FPendingToSave := Nil;
end;

function TAbstractMemBlockchainStorage.DeleteBlockChainBlockExt(ABlock : Integer; const AOrphan : String) : Boolean;
var LBlock : TBlockInformation;
  LOperationRawDataSearch,LOperationRawDataFound : TOperationRawData;
  LPos : TAbstractMemPosition;
  LOperation : TPCOperation;
  LAffectedAccountSearch, LAffectedAccountFound, LAffectedAccount : TAffectedAccount;
  LAffectedAccounts : TOrderedList<Cardinal>;
  i : Integer;
begin
  Result := False;
  LBlock.Clear;
  LBlock.orphan := AOrphan;
  LBlock.operationBlock.block := ABlock;
  if Not (FBlocksInformation_By_OrphanBlock.DeleteData(LBlock)) then Exit;

  FOrphansInformation_By_Orphan.Update(AOrphan,-1);

  Result := True;

  if OrphanCompare(AOrphan,Orphan)<>0 then Exit;

  // Try to delete all operations and affected accounts:
  LOperationRawDataSearch.Clear;
  LOperationRawDataSearch.block := LBlock.operationBlock.block;
  LOperationRawDataSearch.opblock := MAXINT; // Will search BACKWARDS
  FOperationsRawData_By_Block_OpBlock_Index.FindData(LOperationRawDataSearch,LOperationRawDataFound);
  while (LOperationRawDataSearch.opblock>=0) and (LOperationRawDataFound.block = LOperationRawDataSearch.block) do begin
    // Delete affected accounts

    if LOperationRawDataFound.CreateTPCOperation(FFileMem,LOperation) then
    try
      LAffectedAccounts := TOrderedList<Cardinal>.Create(False,TComparison_Cardinal);
      Try
        LOperation.AffectedAccounts(LAffectedAccounts);
        for i := 0 to LAffectedAccounts.Count-1 do begin
          LAffectedAccount.Clear;
          LAffectedAccount.account := LAffectedAccounts.Items[i];
          LAffectedAccount.block := ABlock;
          LAffectedAccount.opblock := LOperationRawDataFound.opblock;
          //
          if Not FAffectedAccounts_By_Account_Block_OpBlock.DeleteData(LAffectedAccount) then begin
            TLog.NewLog(lterror,ClassName,Format('ERR 20211117-01 Affected account %d %d %d not found',[LAffectedAccount.account,LAffectedAccount.block,LAffectedAccount.opblock+1]));
          end;

        end;
      Finally
        LAffectedAccounts.Free;
      end;
    Finally
      LOperation.Free;
    end;
    if not FOperationsRawData_By_RightOpHash.DeleteData(LOperationRawDataFound) then begin
      // Found
      raise EAbstractMemBlockchainStorage.Create('ERR 20211117-02');
    end;
    // Go backward
    LOperationRawDataSearch.opblock := LOperationRawDataFound.opblock-1;
    FOperationsRawData_By_Block_OpBlock_Index.FindData(LOperationRawDataSearch,LOperationRawDataFound);
  end;
end;

destructor TAbstractMemBlockchainStorage.Destroy;
begin
  UseMultithread := False;

  FreeAndNil(FPendingToSave);

  FreeAndNil(FFileMem);

  FreeAndNil(FOrphansInformation_By_Orphan);
  FreeAndNil(FBlocksInformation_By_OrphanBlock);
  FreeAndNil(FOperationsRawData_By_RightOpHash);
  FreeAndNil(FOperationsRawData_By_Block_OpBlock_Index);
  FreeAndNil(FAffectedAccounts_By_Account_Block_OpBlock);

  FreeAndNil(FStorageLock);

  inherited;
end;

function TAbstractMemBlockchainStorage.DoBlockExists(Block: Cardinal): Boolean;
var LFoundBlock : TBlockInformation;
begin
  Result := DoBlockExistsByOrphan(Block,Orphan,LFoundBlock);
  if Not Result then BlockNotFound(Block);
end;

function TAbstractMemBlockchainStorage.DoBlockExistsByOrphan(ABlock: Integer;
  const AOrphan: String; var LBlockInformation: TBlockInformation): Boolean;
var LSearch : TBlockInformation;
  LDataPos : TAbstractMemPosition;
begin
  LSearch.Clear;
  LSearch.orphan := AOrphan;
  LSearch.operationBlock.block := ABlock;
  Result := FBlocksInformation_By_OrphanBlock.FindData(LSearch,LDataPos,LBlockInformation);
end;

procedure TAbstractMemBlockchainStorage.DoBlockNotFound(ABlock: Integer);
begin
  // Nothing to do here
end;

procedure TAbstractMemBlockchainStorage.DoDeleteBlockChainBlocks(StartingDeleteBlock: Cardinal);
begin
  FStorageLock.Acquire;
  try
    while DeleteBlockChainBlockExt(StartingDeleteBlock,Orphan) do inc(StartingDeleteBlock);
    FinalizedUpdating;
  finally
    FStorageLock.Release;
  end;
end;

procedure TAbstractMemBlockchainStorage.DoEraseStorage;
begin
  FStorageLock.Acquire;
  Try
    FFileMem.ClearContent(FFileMem.Is64Bits,FFileMem.MemUnitsSize);
    FreeAndNil(FFileMem);
    DoInitialize;
    FinalizedUpdating;
  Finally
    FStorageLock.Release;
  End;
end;

function TAbstractMemBlockchainStorage.DoFindOperation(const AOpHash: TBytes; var AOperationResume: TOperationResume): TSearchOpHashResult;
var LSearch, LFound : TOperationRawData;
  LPos : TAbstractMemPosition;
  LOperation : TPCOperation;
  LMD160Hash : TBytes;
  LBlock, LAccount, LN_Operation : Cardinal;
begin
  Result := OpHash_invalid_params;
  if not (TPCOperation.DecodeOperationHash(AOpHash,LBlock,LAccount,LN_Operation,LMD160Hash)) then Exit;

  if Not BlockExists(LBlock) then Exit;

  LSearch.Clear;
  LSearch.rightOpHash := Copy(AOpHash,12,20);
  if (FOperationsRawData_By_RightOpHash.FindData(LSearch,LPos,LFound)) then begin
    if LFound.CreateTPCOperation(FFileMem,LOperation) then
    Try
      if not TPCOperation.OperationToOperationResume(LFound.block,LOperation,True,LAccount,AOperationResume) then Exit;
      AOperationResume.NOpInsideBlock := LFound.opblock;
      AOperationResume.Balance := -1;
      Result := OpHash_found;
    Finally
      LOperation.Free;
    End;
  end else Result := OpHash_block_not_found;
end;


function TAbstractMemBlockchainStorage.DoGetAccountOperations(AAccount,
  AMaxDepth, AStartOperation, AMaxOperations, ASearchBackwardsStartingAtBlock: Integer;
  const AOperationsResumeList: TOperationsResumeList): Boolean;
var LSearch,LFound : TAffectedAccount;
  LOperation : TPCOperation;
  LOPR : TOperationResume;
  LPreviousBlock : Integer;
  LOperationsHashTree : TOperationsHashTree;
  LLastBalance : Int64;
  LAcc : TAccount;
  LHasFound : Boolean;
begin
  if AMaxOperations=0 then Exit(False);
  if AStartOperation<0 then Exit(False);
  Result := True;
  LSearch.Clear;
  LSearch.account := AAccount;
  LAcc := Bank.SafeBox.Account(AAccount);

  if Not BlockExists(LAcc.GetLastUpdatedBlock) then Exit(False);

  if ASearchBackwardsStartingAtBlock>0 then begin
    LSearch.block := ASearchBackwardsStartingAtBlock;
  end else begin
    LSearch.block := MAXINT;
  end;
  LSearch.opblock := MAXINT;
  LFound.Clear;
  if Not FAffectedAccounts_By_Account_Block_OpBlock.FindData(LSearch,LFound) then begin
    if (LFound.account <> AAccount)  then Exit(False);
  end;
  if (LFound.block = LAcc.GetLastUpdatedBlock) then begin
    LLastBalance := LAcc.balance;
  end else LLastBalance := -1;
  LPreviousBlock := LFound.block;
  repeat
    // Process back
    if (LFound.account<>AAccount) then Break;
    if (LFound.block<>LPreviousBlock) then begin
      Dec(AMaxDepth);
      LPreviousBlock := LFound.block;
      if (AMAxDepth=0) then Break;
      if Not BlockExists(LFound.block) then Break;
    end;
    if (AStartOperation>0) then Dec(AStartOperation)
    else begin
      LOperationsHashTree := TOperationsHashTree.Create;
      Try
        if DoGetOperation(LFound.block,LFound.opblock,LOperationsHashTree) then begin
          if LOperationsHashTree.OperationsCount=1 then begin
            LOperation := LOperationsHashTree.GetOperation(0);
            if TPCOperation.OperationToOperationResume(LFound.block,LOperation,True,AAccount,LOPR) then begin

              LOPR.NOpInsideBlock := LFound.opblock;
              LOPR.time := Bank.SafeBox.GetBlockInfo(LFound.block).timestamp;
              LOPR.Block := LFound.block;
              If LLastBalance>=0 then begin
                LOPR.Balance := LLastBalance;
                LLastBalance := LLastBalance - ( LOPR.Amount + LOPR.Fee );
              end else LOPR.Balance := 0; // Undetermined

              AOperationsResumeList.Add(LOPR);
            end;
          end;
        end;
      Finally
        LOperationsHashTree.Free;
      End;
      Dec(AMaxOperations);
    end;
    LSearch.CopyFrom(LFound);
    LHasFound := FAffectedAccounts_By_Account_Block_OpBlock.FindDataPrecessor(LSearch,LFound);
  until (AMaxDepth=0) or (AMaxOperations=0) or (Not LHasFound);

end;

function TAbstractMemBlockchainStorage.DoGetBlockInformation(const ABlock : Integer;
  var AOperationBlock: TOperationBlock; var AOperationsCount: Integer;
  var AVolume: Int64): Boolean;
var LBlock,LFoundBlock : TBlockInformation;
  LDataPos : TAbstractMemPosition;
begin
  if Not BlockExists(ABlock) then Exit(False);

  LBlock.Clear;
  LBlock.orphan := Orphan;
  LBlock.operationBlock.block := ABlock;
  if FBlocksInformation_By_OrphanBlock.FindData(LBlock,LDataPos,LFoundBlock) then begin
    AOperationBlock := LFoundBlock.operationBlock;
    AOperationsCount := LFoundBlock.operationsCount;
    AVolume := LFoundBlock.volume;
    Result := True;
  end else Result := False;
end;

function TAbstractMemBlockchainStorage.DoGetBlockOperations(ABlock,
  AOpBlockStartIndex, AMaxOperations: Integer;
  var AOperationBlock: TOperationBlock; var AOperationsCount: Integer;
  var AVolume: Int64;
  const AOperationsResumeList: TOperationsResumeList): Boolean;
var LFound,LSearch : TOperationRawData;
  LOperation : TPCOperation;
  LOPR : TOperationResume;
begin
  //
  if AMaxOperations=0 then Exit(False);

  if Not BlockExists(ABlock) then Exit(False);

  Result := True;
  LSearch.Clear;
  LSearch.block := ABlock;
  LSearch.opblock := AOpBlockStartIndex;
  LFound.Clear;
  if not FOperationsRawData_By_Block_OpBlock_Index.FindData(LSearch,LFound) then begin
    if LFound.block<>ABlock then Exit(False);
    LSearch := LFound;
    if Not FOperationsRawData_By_Block_OpBlock_Index.FindDataSuccessor(LSearch,LFound) then Exit(False);
    if LFound.block<>ABlock then Exit(False);
  end;
  repeat
    if LFound.block<>ABlock then Exit(True);

    if LFound.CreateTPCOperation(FFileMem,LOperation) then
    Try
      if not TPCOperation.OperationToOperationResume(ABlock,LOperation,True,LOperation.SignerAccount,LOPR) then break;
      LOPR.NOpInsideBlock := LFound.opblock;
      LOPR.Balance := -1;
      AOperationsResumeList.Add(LOPR);
    Finally
      LOperation.Free;
    End;
    Dec(AMaxOperations);
    LSearch := LFound;
  until (AMaxOperations=0) or (Not FOperationsRawData_By_Block_OpBlock_Index.FindDataSuccessor(LSearch,LFound));
end;

function TAbstractMemBlockchainStorage.DoGetOperation(const ABlock, AOpBlock: Integer; const AOperations: TOperationsHashTree): Boolean;
var LSearch,LFound : TOperationRawData;
  LOp : TPCOperation;
begin
  if Not BlockExists(ABlock) then Exit(False);

  LSearch.Clear;
  LSearch.SetToFindByBlockOpblock(ABlock,AOpBlock);
  LFound.Clear;
  if Not FOperationsRawData_By_Block_OpBlock_Index.FindData(LSearch,LFound) then Exit(False);
  Result := True;
  if LFound.CreateTPCOperation(FFileMem,LOp) then
  try
    AOperations.AddOperationToHashTree( LOp );
  finally
    LOp.Free;
  end;
end;

function TAbstractMemBlockchainStorage.DoInitialize: Boolean;
const CT_HEADER = 'AMBlockchain'; // 12 chars
      CT_VERSION : Integer = $00000003;   // 4 bytes
var LfdZone : TAMZone;
  LfdBytes : TBytes;
  LExpectedHeader,
  LHeader : TBytes;
  LZoneOrphansInformation,
  LZoneBlocksInformation_By_Block,
  LZoneBlocksRawData_By_BlockOrphan,
  LZoneOperationsRawData_By_RightOpHash,
  LZoneOperationsRawData_By_Block_OpBlock,
  LZoneAffectedAccounts_By_Account_Block_OpBlock : TAMZone;
  LFileName  : String;
  i : Integer;
  LCacheMem : TCacheMem;
begin
  Result := False;
  if Not Assigned(FFileMem) then begin
    if (FStorageFilename='') then begin
      FStorageFilename := Bank.GetStorageFolder(Bank.Orphan)+PathDelim+'BlockChainStream.am_blocks';
    end;

    FFileMem := TFileMem.Create(FStorageFilename,ReadOnly);
    FFileMem.IncreaseFileBytes := 1 * 1024*1024; // 1Mb each increase

    LCacheMem := FFileMem.LockCache;
    try
      LCacheMem.GridCache := False;
      LCacheMem.DefaultCacheDataBlocksSize := 1024;
      LCacheMem.MaxCacheSize := 300 * 1024 * 1024; // 300Mb
      LCacheMem.MaxCacheDataBlocks := 150000;
    finally
      FFileMem.UnlockCache;
    end;

  end;
  if Not FFileMem.HeaderInitialized then begin
    if ReadOnly then Exit(False);
    if not FFileMem.Initialize(True,4) then Exit(False);
  end;
  FreeAndNil(FPendingToSave);
  FreeAndNil(FOrphansInformation_By_Orphan);
  FreeAndNil(FBlocksInformation_By_OrphanBlock);
  FreeAndNil(FOperationsRawData_By_RightOpHash);
  FreeAndNil(FOperationsRawData_By_Block_OpBlock_Index);
  FreeAndNil(FAffectedAccounts_By_Account_Block_OpBlock);

  LZoneOrphansInformation.Clear;
  LZoneBlocksInformation_By_Block.Clear;
  LZoneBlocksRawData_By_BlockOrphan.Clear;
  LZoneOperationsRawData_By_RightOpHash.Clear;
  LZoneOperationsRawData_By_Block_OpBlock.Clear;
  LZoneAffectedAccounts_By_Account_Block_OpBlock.Clear;

  LExpectedHeader.FromString(CT_HEADER);
  Assert(Length(LExpectedHeader)=12,'CT_HEADER Header is not 12 bytes');
  SetLength(LExpectedHeader,16);
  i := CT_VERSION;
  Move(i,LExpectedHeader[12],4);

  SetLength(LHeader,Length(LExpectedHeader)); // 16

  if (FFileMem.ReadFirstData(LfdZone,LfdBytes))
    and (LfdZone.size>=100) then begin
    Move(LfdBytes[0],LHeader[0],16);
    //
    Move(LfdBytes[16],LZoneBlocksInformation_By_Block.position,8);
    Move(LfdBytes[24],LZoneBlocksRawData_By_BlockOrphan,8);
    Move(LfdBytes[32],LZoneOperationsRawData_By_RightOpHash,8);
    Move(LfdBytes[40],LZoneOperationsRawData_By_Block_OpBlock,8);
    Move(LfdBytes[48],LZoneAffectedAccounts_By_Account_Block_OpBlock,8);
    Move(LfdBytes[56],LZoneOrphansInformation,8);
  end;

  if (Not CompareMem(@LExpectedHeader[0],@LHeader[0],Length(LExpectedHeader))) or
    (Not FFileMem.Is64Bits) or
    (LZoneOrphansInformation.position=0) or
    (LZoneBlocksInformation_By_Block.position=0) or
    (LZoneBlocksRawData_By_BlockOrphan.position=0) or
    (LZoneOperationsRawData_By_RightOpHash.position=0) or
    (LZoneOperationsRawData_By_Block_OpBlock.position=0) or
    (LZoneAffectedAccounts_By_Account_Block_OpBlock.position=0) then begin
    FFileMem.ClearContent(True,4);

    //
    SetLength(LfdBytes,100);
    FillChar(LfdBytes[0],Length(LfdBytes),0);
    LfdZone := FFileMem.New(Length(LfdBytes));

    // Create
    LZoneOrphansInformation := FFileMem.New(TAbstractMemBTree.MinAbstractMemInitialPositionSize(FFileMem));
    LZoneBlocksInformation_By_Block := FFileMem.New(TAbstractMemBTree.MinAbstractMemInitialPositionSize(FFileMem));
    LZoneBlocksRawData_By_BlockOrphan := FFileMem.New(TAbstractMemBTree.MinAbstractMemInitialPositionSize(FFileMem));
    LZoneOperationsRawData_By_RightOpHash := FFileMem.New(TAbstractMemBTree.MinAbstractMemInitialPositionSize(FFileMem));
    LZoneOperationsRawData_By_Block_OpBlock := FFileMem.New(TAbstractMemBTree.MinAbstractMemInitialPositionSize(FFileMem));
    LZoneAffectedAccounts_By_Account_Block_OpBlock := FFileMem.New(TAbstractMemBTree.MinAbstractMemInitialPositionSize(FFileMem));
    //
    Move(LExpectedHeader[0],LfdBytes[0],16);
    //
    Move(LZoneBlocksInformation_By_Block.position             ,LfdBytes[16],8);
    Move(LZoneBlocksRawData_By_BlockOrphan.position           ,LfdBytes[24],8);
    Move(LZoneOperationsRawData_By_RightOpHash.position       ,LfdBytes[32],8);
    Move(LZoneOperationsRawData_By_Block_OpBlock.position     ,LfdBytes[40],8);
    Move(LZoneAffectedAccounts_By_Account_Block_OpBlock.position,LfdBytes[48],8);
    Move(LZoneOrphansInformation.position                     ,LfdBytes[56],8);

    FFileMem.Write(LfdZone.position,LfdBytes[0],Length(LfdBytes));
  end;
  //
  //
  FOrphansInformation_By_Orphan               := TAMBTreeOrphanInformationByOrphan.Create(
    FFileMem,LZoneOrphansInformation,
    False,29,Comparer_TOrphanInformation_By_Orphan);
  FBlocksInformation_By_OrphanBlock           := TAMBTreeOperationBlockInformationByOrphanBlock.Create(
    FFileMem,LZoneBlocksInformation_By_Block,
    False,509,Comparer_TBlockInformation_By_OrphanBlock);
  FOperationsRawData_By_RightOpHash           := TAMBTreeTOperationRawDataByRightOpHash.Create(
    FFileMem,LZoneOperationsRawData_By_RightOpHash,
    False,509,Comparer_TOperationRawData_By_RightOpHash);
  FOperationsRawData_By_Block_OpBlock_Index   := TAMBTreeTOperationRawDataByBlockOpBlock_Index.Create(
    FOperationsRawData_By_RightOpHash,LZoneOperationsRawData_By_Block_OpBlock,
    TRUE,509,Comparer_TOperationRawData_By_Block_OpBlock);
  FAffectedAccounts_By_Account_Block_OpBlock  := TAMBTreeTAffectedAccountByAccountBlockOpBlock.Create(
    FFileMem,LZoneAffectedAccounts_By_Account_Block_OpBlock,
    False,509,Comparer_TAffectedAccount_By_Account_Block_OpBlock);

  if FUseMultithread then begin
    FPendingToSave := TPendingToSave.Create(Self,FOperationsRawData_By_RightOpHash,FAffectedAccounts_By_Account_Block_OpBlock);
  end;

  Result := True;
end;

function TAbstractMemBlockchainStorage.DoLoadBlockChain(Operations: TPCOperationsComp; Block: Cardinal): Boolean;
begin
  Result := DoLoadBlockChainExt(Operations,Block,Orphan);
  if Not Result then BlockNotFound(Block);
end;

function TAbstractMemBlockchainStorage.DoLoadBlockChainExt(
  Operations: TPCOperationsComp; Block: Cardinal;
  const AOrphan: String): Boolean;
var LBlock,LFoundBlock : TBlockInformation;
  LDataPos : TAbstractMemPosition;
begin
  LBlock.Clear;
  LBlock.orphan := AOrphan;
  LBlock.operationBlock.block := Block;
  if FBlocksInformation_By_OrphanBlock.FindData(LBlock,LDataPos,LFoundBlock) then begin
    LFoundBlock.ReadTPCOperationsComp(FBlocksInformation_By_OrphanBlock.AbstractMem,Operations);
    Result := True;
  end else Result := False;
end;

function TAbstractMemBlockchainStorage.DoMoveBlockChain(StartBlock : Cardinal; Const DestOrphan : TOrphan) : Boolean;
var LPCOperationsComp : TPCOperationsComp;
begin
  Assert(Orphan<>DestOrphan,'Orphan and Destorphan are equals');
  FStorageLock.Acquire;
  try
    LPCOperationsComp := TPCOperationsComp.Create(Nil);
    try
      while LoadBlockChainBlock(LPCOperationsComp,StartBlock) do begin
        if Not DeleteBlockChainBlockExt(StartBlock,Orphan) then raise EAbstractMemBlockchainStorage.Create('ERR 20211117-03');
        if Not DoSaveBlockChainExt(LPCOperationsComp,DestOrphan,FSaveStorageStats) then raise EAbstractMemBlockchainStorage.Create('ERR 20211117-04');
        inc(StartBlock);
      end;
    finally
      LPCOperationsComp.Free;
    end;
    Result := True;
    FinalizedUpdating;
  finally
    FStorageLock.Release;
  end;
end;

function TAbstractMemBlockchainStorage.DoSaveBlockChain(Operations: TPCOperationsComp): Boolean;
begin
  FStorageLock.Acquire;
  try
    Result := DoSaveBlockChainExt(Operations,Orphan,FSaveStorageStats);
    FinalizedUpdating;
  finally
    FStorageLock.Release;
  end;
end;

function TAbstractMemBlockchainStorage.DoSaveBlockChainExt(Operations: TPCOperationsComp; const AOrphan: String; var AStats: TBlockchainStorageStats): Boolean;
var LBlockInformation : TBlockInformation;
    LOperationRawData :  TOperationRawData;
    LAffectedAccount : TAffectedAccount;
  LMemStream : TMemoryStream;
  iOperation, iOpAccount : Integer;
  LOp : TPCOperation;
  Laccounts : TOrderedList<Cardinal>;
  LRawData : TBytes;
  LAMZone : TAMZone;

  LPendingData : TPendingData;
  LTC : TTickCount;
begin
  Result := True;
  LBlockInformation.Clear;
  LOperationRawData.Clear;
  LAffectedAccount.Clear;

  // Add
  LBlockInformation.Clear;
  LBlockInformation.operationBlock := Operations.OperationBlock;
  LBlockInformation.orphan := AOrphan;
  LBlockInformation.operationsCount := Operations.Count;
  LBlockInformation.volume := Operations.OperationsHashTree.TotalAmount;

  // DELETE PREVIOUS:
  DeleteBlockChainBlockExt(Operations.OperationBlock.block,AOrphan);

  LMemStream := TMemoryStream.Create;
  Try
    Operations.SaveBlockToStorage(LMemStream);
    SetLength(LRawData,LMemStream.Size);
    Move(LMemStream.Memory^,LRawData[0],LMemStream.Size);
  Finally
    LMemStream.Free;
  End;
  LAMZone := FFileMem.New(Length(LRawData));
  FFileMem.Write(LAMZone.position,LRawData[0],Length(LRawData));

  LBlockInformation.rawDataPosition := LAMZone.position;

  FBlocksInformation_By_OrphanBlock.AddData(LBlockInformation);
  Inc(AStats.blockInformationCount);

  // Save increment:
  FOrphansInformation_By_Orphan.Update(AOrphan,+1);

  if OrphanCompare(AOrphan,Orphan)<>0 then Exit;
  LTC := TPlatform.GetTickCount;

  for iOperation := 0 to Operations.count-1 do begin
    LOp := Operations.Operation[iOperation];

    LOperationRawData.Clear;
    LOperationRawData.rightOpHash := Copy(LOp.RipeMD160,0,20);
    LOperationRawData.account := LOp.SignerAccount;
    LOperationRawData.n_operation := LOp.N_Operation;

    LOperationRawData.block := Operations.OperationBlock.block;
    LOperationRawData.opblock := iOperation;
    LOperationRawData.opType := LOp.OpType;
    LOperationRawData.opSavedProtocol := LOp.ProtocolVersion;

    LMemStream := TMemoryStream.Create;
    Try
      LOp.SaveToStorage(LMemStream);
      LOperationRawData.rawData := TStreamOp.SaveStreamToRaw(LMemStream);
    Finally
      LMemStream.Free;
    End;
    LPendingData.Clear;
    LPendingData.operation.CopyFrom(LOperationRawData);
    if Not Assigned(FPendingToSave) then begin
      // Add Operation
      if not FOperationsRawData_By_RightOpHash.AddData(LOperationRawData) then
        raise EAbstractMemBlockchainStorage.Create(Format('Cannot add operation %d/%d of block %d - %s',[iOperation+1,Operations.Count,Operations.OperationBlock.block, LOp.ToString]));
      Inc(AStats.operationRawDataCount);
    end;
    if (FLogSaveActivity) and (TPlatform.GetElapsedMilliseconds(LTC)>10000) then begin
      LTC := TPlatform.GetTickCount;
      TLog.NewLog(ltdebug,ClassName,Format('Saving block %d operation %d/%d - %s',[Operations.OperationBlock.block,iOperation+1,Operations.Count,FSaveStorageStats.ToString]));
    end;

    // Affected accounts:
    Laccounts := TOrderedList<Cardinal>.Create(False,TComparison_Cardinal);
    try
      LOp.AffectedAccounts(Laccounts);
      SetLength(LPendingData.affectedAccounts,LAccounts.Count);
      for iOpAccount:=0 to Laccounts.Count-1 do begin
        //
        LAffectedAccount.Clear;
        LAffectedAccount.account := Laccounts.Items[iOpAccount];
        LAffectedAccount.n_operation := LOp.GetAccountN_Operation(Laccounts.Items[iOpAccount]);
        LAffectedAccount.block := Operations.OperationBlock.block;
        LAffectedAccount.opblock := iOperation;

        LPendingData.affectedAccounts[iOpAccount].CopyFrom( LAffectedAccount );
        if Not Assigned(FPendingToSave) then begin
          // Add affected account
          if not FAffectedAccounts_By_Account_Block_OpBlock.AddData(LAffectedAccount) then begin
            raise EAbstractMemBlockchainStorage.Create(Format('Cannot add affected account %d/%d in operation %d/%d of block %d - %s',
              [iOpAccount+1,LAccounts.Count,iOperation+1,Operations.Count,Operations.OperationBlock.block,
              LOp.ToString]));
          end;
          Inc(AStats.affectedAccountCount);
        end;
      end;
    finally
      Laccounts.Free;
    end;
    if Assigned(FPendingToSave) then begin
      FPendingToSave.AddPendingData(LPendingData);
    end;
  end;
  //
end;

procedure TAbstractMemBlockchainStorage.FillInfo(AStrings: TStrings);
var LOrphans, LSearch : TOrphanInformation;
begin
  AStrings.Add(Format('Orphan "%s" from %d to %d with Orphans: %d',[
    Orphan,FirstBlock,LastBlock,
    FOrphansInformation_By_Orphan.Count]));
  if FOrphansInformation_By_Orphan.FindDataLowest(LOrphans) then begin
    repeat
      AStrings.Add(Format('- Orphan "%s" regs: %d',[LOrphans.orphan,LOrphans.regsCounter]));
      LSearch.CopyFrom(LOrphans)
    until (Not (FOrphansInformation_By_Orphan.FindDataSuccessor(LSearch,LOrphans)));
  end else AStrings.Add('No orphans');

end;

procedure TAbstractMemBlockchainStorage.FinalizedUpdating;
var LTC : TTickCount;
begin
  if FAutoFlushCache then begin
    LTC := TPlatform.GetTickCount;
    FileMem.FlushCache;
    TLog.NewLog(ltdebug,ClassName,Format('Flushed Cache after finalized updating blockchain in %d millis',[TPlatform.GetElapsedMilliseconds(LTC)]));
  end;
end;

function TAbstractMemBlockchainStorage.GetFirstBlockNumber: Int64;
begin
  Result := GetFirstBlockNumberByOrphan(Orphan);
end;

function TAbstractMemBlockchainStorage.GetFirstBlockNumberByOrphan(
  const AOrphan: String): Int64;
var LBlockInformation,LBlockInformationFound : TBlockInformation;
  LPos : TAbstractMemPosition;
begin
  Result := -1;
  LBlockInformation.Clear;
  LBlockInformation.orphan := AOrphan;
  LBlockInformation.operationBlock.block := 0;
  if Not FBlocksInformation_By_OrphanBlock.FindData(LBlockInformation,LPos,LBlockInformationFound) then begin
    if FBlocksInformation_By_OrphanBlock.Count<=0 then Exit(-1);
    if LBlockInformationFound.IsOrphan(AOrphan) then Exit(LBlockInformationFound.operationBlock.block);
    LBlockInformation := LBlockInformationFound;
    if FBlocksInformation_By_OrphanBlock.FindDataSuccessor(LBlockInformation,LBlockInformationFound) then begin
      if LBlockInformationFound.IsOrphan(AOrphan) then Exit(LBlockInformationFound.operationBlock.block);
    end;
  end else Result := LBlockInformationFound.operationBlock.block;
end;

function TAbstractMemBlockchainStorage.GetLastBlockNumber: Int64;
begin
  Result := GetLastBlockNumberByOrphan(Orphan);
end;

function TAbstractMemBlockchainStorage.GetLastBlockNumberByOrphan(const AOrphan: String): Int64;
var LBlockInformation,LBlockInformationFound : TBlockInformation;
  LPos : TAbstractMemPosition;
begin
  Result := -1;
  LBlockInformation.Clear;
  LBlockInformation.orphan := AOrphan;
  LBlockInformation.operationBlock.block := MAXINT;
  if Not FBlocksInformation_By_OrphanBlock.FindData(LBlockInformation,LPos,LBlockInformationFound) then begin
    if FBlocksInformation_By_OrphanBlock.Count<=0 then Exit(-1);
    if LBlockInformationFound.IsOrphan(AOrphan) then Exit(LBlockInformationFound.operationBlock.block);
  end else Result := LBlockInformationFound.operationBlock.block;
end;

class function TAbstractMemBlockchainStorage.OrphanCompare(const ALeft, ARight: String): Integer;
begin
  Result := BinStrComp(ALeft,ARight);
end;

function TAbstractMemBlockchainStorage.PendingToSave: Integer;
begin
  if Assigned(FPendingToSave) then begin
    Result := FPendingToSave.PendingsCount;
  end else Result := 0;
end;

procedure TAbstractMemBlockchainStorage.SetReadOnly(const Value: Boolean);
begin
  if ReadOnly=Value then Exit;
  inherited;
  //
  if Assigned(FFileMem) then begin
    FreeAndNil(FPendingToSave);
    FreeAndNil(FFileMem);
    Initialize;
  end;
end;

procedure TAbstractMemBlockchainStorage.SetUseMultithread(const Value: Boolean);
var
  i : Integer;
begin
  if FUseMultithread=Value then Exit;
  FStorageLock.Acquire;
  Try
    if Assigned(FPendingToSave) then begin
      i := FPendingToSave.PendingsCount;
      if i>0 then begin
        TLog.NewLog(ltinfo,ClassName,Format('Finalizing use of multitrheads with %d pending jobs',[i]));
      end;
      while (FPendingToSave.PendingsCount>0) do begin
        sleep(1);
      end;
      if i>0 then begin
        TLog.NewLog(ltinfo,ClassName,Format('Finalized use of multitrheads with %d pending jobs',[i]));
      end;
    end;
    FreeAndNil(FPendingToSave);
    FUseMultithread := Value;
    if FUseMultithread then begin
      FPendingToSave := TPendingToSave.Create(Self,FOperationsRawData_By_RightOpHash,FAffectedAccounts_By_Account_Block_OpBlock);
    end;
  Finally
    FStorageLock.Release;
  End;
end;

{ TAbstractMemBlockchainStorage.TAMBTreeOperationBlockInformationByBlock }

procedure TAbstractMemBlockchainStorage.TAMBTreeOperationBlockInformationByOrphanBlock.DeletedData(
  const AData: TBlockInformation);
begin
  inherited;
  if AData.rawDataPosition>0 then begin
    AbstractMem.Dispose(AData.rawDataPosition);
  end;
end;

function TAbstractMemBlockchainStorage.TAMBTreeOperationBlockInformationByOrphanBlock.GetBlockInformationByBlock(
  ABlock: Integer): TBlockInformation;
var LPos : TAbstractMemPosition;
  LBlockSearch : TBlockInformation;
begin
  LBlockSearch.Clear;
  LBlockSearch.operationBlock.block := ABlock;
  if Not FindData(LBlockSearch,LPos,Result) then Result.Clear;
end;

function TAbstractMemBlockchainStorage.TAMBTreeOperationBlockInformationByOrphanBlock.LoadData(
  const APosition: TAbstractMemPosition): TBlockInformation;
var LZone : TAMZone;
  LBytes : TBytes;
begin
  if APosition=0 then begin
    Result.Clear;
    Exit;
  end;
  if Not AbstractMem.GetUsedZoneInfo( APosition, False, LZone) then
    raise EAbstractMemBTree.Create(Format('%s.LoadData Inconsistency error used zone info not found at pos %d',[Self.ClassName,APosition]));
  SetLength(LBytes,LZone.size);
  if AbstractMem.Read(LZone.position, LBytes[0], Length(LBytes) )<>Length(LBytes) then
    raise EAbstractMemBTree.Create(Format('%s.LoadData Inconsistency error cannot read %d bytes at pos %d',[Self.ClassName,LZone.size,APosition]));
  Result.Clear;
  if not Result.FromSerialized(LBytes) then
    raise EAbstractMemBTree.Create(Format('%s.LoadData Invalid FromSerialized call with %d bytes at pos %d',[Self.ClassName,LZone.size,APosition]));
end;

function TAbstractMemBlockchainStorage.TAMBTreeOperationBlockInformationByOrphanBlock.SaveData(
  const AData: TBlockInformation): TAMZone;
var LBytes : TBytes;
begin
  LBytes := AData.ToSerialized;
  Result := AbstractMem.New(Length(LBytes));
  AbstractMem.Write(Result.position,LBytes[0],Length(LBytes));
end;

{ TAbstractMemBlockchainStorage.TAMBTreeTOperationRawDataByRightOpHash }

function TAbstractMemBlockchainStorage.TAMBTreeTOperationRawDataByRightOpHash.LoadData(
  const APosition: TAbstractMemPosition): TOperationRawData;
var LZone : TAMZone;
  LBytes : TBytes;
begin
  if APosition=0 then begin
    Result.Clear;
    Exit;
  end;
  if Not AbstractMem.GetUsedZoneInfo( APosition, False, LZone) then
    raise EAbstractMemBTree.Create(Format('%s.LoadData Inconsistency error used zone info not found at pos %d',[Self.ClassName,APosition]));
  SetLength(LBytes,LZone.size);
  if AbstractMem.Read(LZone.position, LBytes[0], Length(LBytes) )<>Length(LBytes) then
    raise EAbstractMemBTree.Create(Format('%s.LoadData Inconsistency error cannot read %d bytes at pos %d',[Self.ClassName,LZone.size,APosition]));
  Result.Clear;
  if not Result.FromSerialized(LBytes) then
    raise EAbstractMemBTree.Create(Format('%s.LoadData Invalid FromSerialized call with %d bytes at pos %d',[Self.ClassName,LZone.size,APosition]));
end;

function TAbstractMemBlockchainStorage.TAMBTreeTOperationRawDataByRightOpHash.SaveData(
  const AData: TOperationRawData): TAMZone;
var LBytes : TBytes;
begin
  LBytes := AData.ToSerialized;
  Result := AbstractMem.New(Length(LBytes));
  AbstractMem.Write(Result.position,LBytes[0],Length(LBytes));
end;

{ TAbstractMemBlockchainStorage.TAMBTreeTAffectedAccountByAccountBlockOpBlock }

function TAbstractMemBlockchainStorage.TAMBTreeTAffectedAccountByAccountBlockOpBlock.LoadData(
  const APosition: TAbstractMemPosition): TAffectedAccount;
var LZone : TAMZone;
  LBytes : TBytes;
begin
  if APosition=0 then begin
    Result.Clear;
    Exit;
  end;
  if Not AbstractMem.GetUsedZoneInfo( APosition, False, LZone) then
    raise EAbstractMemBTree.Create(Format('%s.LoadData Inconsistency error used zone info not found at pos %d',[Self.ClassName,APosition]));
  SetLength(LBytes,LZone.size);
  if AbstractMem.Read(LZone.position, LBytes[0], Length(LBytes) )<>Length(LBytes) then
    raise EAbstractMemBTree.Create(Format('%s.LoadData Inconsistency error cannot read %d bytes at pos %d',[Self.ClassName,LZone.size,APosition]));
  Result.Clear;
  if not Result.FromSerialized(LBytes) then
    raise EAbstractMemBTree.Create(Format('%s.LoadData Invalid FromSerialized call with %d bytes at pos %d',[Self.ClassName,LZone.size,APosition]));
end;

function TAbstractMemBlockchainStorage.TAMBTreeTAffectedAccountByAccountBlockOpBlock.SaveData(
  const AData: TAffectedAccount): TAMZone;
var LBytes : TBytes;
begin
  LBytes := AData.ToSerialized;
  Result := AbstractMem.New(Length(LBytes));
  AbstractMem.Write(Result.position,LBytes[0],Length(LBytes));
end;

{ TAbstractMemBlockchainStorage.TBlockInformation }

procedure TAbstractMemBlockchainStorage.TBlockInformation.Clear;
begin
  Self.operationBlock := CT_OperationBlock_NUL;
  Self.orphan := '';
  Self.operationsCount := 0;
  Self.volume := 0;
  Self.rawDataPosition := 0;
end;

procedure TAbstractMemBlockchainStorage.TBlockInformation.CopyFrom(const ASource: TBlockInformation);
begin
  Self.operationBlock := ASource.operationBlock.GetCopy;
  Self.orphan := ASource.orphan;
  Self.operationsCount := ASource.operationsCount;
  Self.volume := ASource.volume;
  Self.rawDataPosition := ASource.rawDataPosition;
end;

function TAbstractMemBlockchainStorage.TBlockInformation.CreateTPCOperationsComp(
  AAbstractMem : TAbstractMem; ABank: TPCBank): TPCOperationsComp;
begin
  Result := TPCOperationsComp.Create(ABank);
  try
    ReadTPCOperationsComp(AAbstractMem,Result);
  Except
    On E:Exception do begin
      Result.Free;
      Raise;
    end;
  end;
end;

function TAbstractMemBlockchainStorage.TBlockInformation.FromSerialized(ABytes: TBytes): Boolean;
var LStream : TStream;
  Lsoob : Byte;
  LBuild : Word;
begin
  Clear;
  LStream := TMemoryStream.Create;
  Try
    TStreamOp.LoadStreamFromRaw(LStream,ABytes);
    LStream.Position := 0;
    LStream.Read(LBuild,2);
    if LBuild>CT_PROTOCOL_5 then Exit(False);
    Result := TPCOperationsComp.LoadOperationBlockFromStream(LStream,Lsoob,Self.operationBlock);
    TStreamOp.ReadString(LStream,Self.orphan);
    LStream.Read(Self.operationsCount,4);
    LStream.Read(Self.volume,8);
    LStream.Read(Self.rawDataPosition,8);
  Finally
    LStream.Free;
  End;
  Result := True;
end;

function TAbstractMemBlockchainStorage.TBlockInformation.GetRawData(
  AAbstractMem: TAbstractMem; var ARawData: TBytes): Boolean;
var LZone : TAMZone;
begin
  if Self.rawDataPosition<=0 then begin
    Exit(False);
  end;
  if Not AAbstractMem.GetUsedZoneInfo( Self.rawDataPosition, False, LZone) then
    raise EAbstractMemBlockchainStorage.Create(Format('TAbstractMemBlockchainStorage.TBlockInformation.GetRawData Inconsistency error used zone info not found at pos %d',[Self.rawDataPosition]));
  SetLength(ARawData,LZone.size);
  if AAbstractMem.Read(LZone.position, ARawData[0], Length(ARawData) )<>Length(ARawData) then
    raise EAbstractMemBlockchainStorage.Create(Format('TAbstractMemBlockchainStorage.TBlockInformation.GetRawData Inconsistency error cannot read %d bytes at pos %d',[LZone.size,Self.rawDataPosition]));
  Result := True;
end;

function TAbstractMemBlockchainStorage.TBlockInformation.IsOrphan(const AOrphan: String): Boolean;
begin
  Result := TAbstractMemBlockchainStorage.OrphanCompare(Self.orphan,AOrphan)=0;
end;

procedure TAbstractMemBlockchainStorage.TBlockInformation.ReadTPCOperationsComp(
  AAbstractMem: TAbstractMem; AOperationsComp: TPCOperationsComp);
var LStream : TStream;
  LRaw : TBytes;
  LErrors : String;
begin
  LStream := TMemoryStream.Create;
  Try
    Self.GetRawData(AAbstractMem,LRaw);
    LStream.Write(LRaw[0],Length(LRaw));
    LStream.Position := 0;
    if Not AOperationsComp.LoadBlockFromStorage(LStream,Lerrors) then raise EAbstractMemBlockchainStorage.Create(
      Format('Cannot read ReadTPCOperationsComp %d from storage: %s',[Self.operationBlock.block,LErrors]));
  Finally
    LStream.Free;
  End;
end;

procedure TAbstractMemBlockchainStorage.TBlockInformation.SetToFindByBlock(ABlock: Integer);
begin
  Self.Clear;
  Self.operationBlock.block := ABlock;
end;

function TAbstractMemBlockchainStorage.TBlockInformation.ToSerialized: TBytes;
var LStream : TStream;
  LBuild : Word;
begin
  LStream := TMemoryStream.Create;
  Try
    LBuild := CT_BUILD_PROTOCOL;
    LStream.Write(LBuild,2);
    TPCOperationsComp.SaveOperationBlockToStream(Self.operationBlock,LStream);
    TStreamOp.WriteString(LStream,Self.orphan);
    LStream.Write(Self.operationsCount,4);
    LStream.Write(Self.volume,8);
    LStream.Write(Self.rawDataPosition,8);
    Result := TStreamOp.SaveStreamToRaw(LStream);
  Finally
    LStream.Free;
  End;
end;

{ TAbstractMemBlockchainStorage.TOperationRawData }

procedure TAbstractMemBlockchainStorage.TOperationRawData.Clear;
begin
  Self.rightOpHash := Nil;
  Self.account := 0;
  Self.n_operation := 0;
  Self.block := 0;
  Self.opblock := 0;
  Self.opType := 0;
  Self.opSavedProtocol := 0;
  Self.rawData := Nil;
end;

procedure TAbstractMemBlockchainStorage.TOperationRawData.CopyFrom(const
  ASource: TOperationRawData);
begin
  Self.rightOpHash := Copy(ASource.rightOpHash);
  Self.account := ASource.account;
  Self.n_operation := ASource.n_operation;
  Self.block := ASource.block;
  Self.opblock := ASource.opblock;
  Self.opType := ASource.opType;
  Self.opSavedProtocol := ASource.opSavedProtocol;
  Self.rawData := Copy(ASource.rawData);
end;

function TAbstractMemBlockchainStorage.TOperationRawData.CreateTPCOperation(
  AAbstractMem: TAbstractMem; out APCOperation: TPCOperation): Boolean;
var LOpClass: TPCOperationClass;
  LStream : TStream;
begin
  Result := false;
  APCOperation := Nil;
  LOpClass := TPCOperationsComp.GetOperationClassByOpType(Self.opType);
  if Not Assigned(LOpClass) then Exit;
  APCOperation := LOpClass.Create(Self.opSavedProtocol);
  Try
    LStream := TMemoryStream.Create;
    Try
      TStreamOp.LoadStreamFromRaw(LStream,Self.rawData);
      LStream.Position := 0;
      Result := APCOperation.LoadFromStorage(LStream,CT_BUILD_PROTOCOL,Nil);
    Finally
      LStream.Free;
    End;
  Finally
    if not Result then FreeAndNil(APCOperation);
  End;
end;

function TAbstractMemBlockchainStorage.TOperationRawData.CreateTPCOperation(AAbstractMem: TAbstractMem): TPCOperation;
var LOpClass: TPCOperationClass;
  LStream : TStream;
begin

  LOpClass := TPCOperationsComp.GetOperationClassByOpType(Self.opType);
  if Not Assigned(LOpClass) then raise EAbstractMemBlockchainStorage.Create(Format('Class for OpType %d not found ',[Self.opType]));
  Result := LOpClass.Create(Self.opSavedProtocol);
  Try
    LStream := TMemoryStream.Create;
    Try
      TStreamOp.LoadStreamFromRaw(LStream,Self.rawData);
      LStream.Position := 0;
      if not Result.LoadFromStorage(LStream,CT_BUILD_PROTOCOL,Nil) then raise EAbstractMemBlockchainStorage.Create(
        Format('Cannot load TPCOperation type %s from stream ',[Result.ClassName]));
    Finally
      LStream.Free;
    End;
  Except
    On E: Exception do begin
      Result.Free;
      raise;
    end;
  End;
end;

function TAbstractMemBlockchainStorage.TOperationRawData.FromSerialized(
  ABytes: TBytes): Boolean;
var LStream : TStream;
  LBuild : Word;
begin
  Self.Clear;
  LStream := TMemoryStream.Create;
  Try
    TStreamOp.LoadStreamFromRaw(LStream,ABytes);
    LStream.Position := 0;
    LStream.Read(LBuild,2);
    if LBuild>CT_PROTOCOL_5 then Exit(False);
    TStreamOp.ReadAnsiString(LStream,Self.rightOpHash);
    LStream.Read(Self.account,4);
    LStream.Read(Self.n_operation,4);
    LStream.Read(Self.block,4);
    LStream.Read(Self.opblock,4);
    LStream.Read(Self.opType,2);
    LStream.Read(Self.opSavedProtocol,2);
    TStreamOp.ReadAnsiString(LStream,Self.rawData);
  Finally
    LStream.Free;
  End;
  Result := True;
end;

procedure TAbstractMemBlockchainStorage.TOperationRawData.SetToFindByBlockOpblock(
  ABlock, AOpblock: Integer);
begin
  Self.Clear;
  Self.block := Ablock;
  Self.opblock := AOpblock;
end;

procedure TAbstractMemBlockchainStorage.TOperationRawData.SetToFindByRightOpHash(
  const ARightOpHash: TBytes);
begin
  Self.Clear;
  Self.rightOpHash := Copy(ARightOpHash);
end;

function TAbstractMemBlockchainStorage.TOperationRawData.ToSerialized: TBytes;
var LStream : TStream;
  Lraw : TRawBytes;
  LBuild : Word;
begin
  LStream := TMemoryStream.Create;
  Try
    LBuild := CT_BUILD_PROTOCOL;
    LStream.Write(LBuild,2);
    TStreamOp.WriteAnsiString(LStream,Self.rightOpHash);
    LStream.Write(Self.account,4);
    LStream.Write(Self.n_operation,4);
    LStream.Write(Self.block,4);
    LStream.Write(Self.opblock,4);
    LStream.Write(Self.opType,2);
    LStream.Write(Self.opSavedProtocol,2);
    TStreamOp.WriteAnsiString(LStream,Self.rawData);
    Result := TStreamOp.SaveStreamToRaw(LStream);
  Finally
    LStream.Free;
  End;
end;

{ TAbstractMemBlockchainStorage.TAffectedAccount }

procedure TAbstractMemBlockchainStorage.TAffectedAccount.Clear;
begin
  Self.account := -1; // -1 = No account
  Self.n_operation := 0;
  Self.block := -1;  // -1 = No block
  Self.opblock := -1; // -1 = No opblock
end;

procedure TAbstractMemBlockchainStorage.TAffectedAccount.CopyFrom(
  const ASource: TAffectedAccount);
begin
  Self.account := ASource.account;
  Self.n_operation := ASource.n_operation;
  Self.block := ASource.block;
  Self.opblock := ASource.opblock;
end;

function TAbstractMemBlockchainStorage.TAffectedAccount.FromSerialized(
  ABytes: TBytes): Boolean;
var LStream : TStream;
  LBuild : Word;
begin
  LStream := TMemoryStream.Create;
  Try
    TStreamOp.LoadStreamFromRaw(LStream,ABytes);
    LStream.Position := 0;
    LStream.Read(LBuild,2);
    if LBuild>CT_PROTOCOL_5 then Exit(False);
    LStream.Read(Self.account,4);
    LStream.Read(Self.n_operation,4);
    LStream.Read(Self.block,4);
    LStream.Read(Self.opblock,4);
  Finally
    LStream.Free;
  End;
  Result := True;
end;

procedure TAbstractMemBlockchainStorage.TAffectedAccount.SetToFindByAccount(
  AAccount: Integer);
begin
  Self.Clear;
  Self.account := AAccount;
end;

procedure TAbstractMemBlockchainStorage.TAffectedAccount.SetToFindByAccountBlockOpblock(
  AAccount, ABlock, AOpblock: Integer);
begin
  Self.Clear;
  Self.account := AAccount;
  Self.block := ABlock;
  Self.opblock := AOpblock;
end;

function TAbstractMemBlockchainStorage.TAffectedAccount.ToSerialized: TBytes;
var LStream : TStream;
  Lraw : TRawBytes;
  LBuild : Word;
begin
  LStream := TMemoryStream.Create;
  Try
    LBuild := CT_BUILD_PROTOCOL;
    LStream.Write(LBuild,2);
    LStream.Write(Self.account,4);
    LStream.Write(Self.n_operation,4);
    LStream.Write(Self.block,4);
    LStream.Write(Self.opblock,4);
    Result := TStreamOp.SaveStreamToRaw(LStream);
  Finally
    LStream.Free;
  End;
end;

function TAbstractMemBlockchainStorage.TAffectedAccount.ToString: String;
begin
  Result := Format('Account %s (n_operation %d) on Block %d opBlock %d',[TAccountComp.AccountNumberToAccountTxtNumber(Self.account),Self.n_operation,Self.block,Self.opblock]);
end;

{ TAbstractMemBlockchainStorage.TAMBTreeOrphanInformationByOrphan }

function TAbstractMemBlockchainStorage.TAMBTreeOrphanInformationByOrphan.GetRegsCountByOrphan(const AOrphan: String): Integer;
var LSearch, LFound : TOrphanInformation;
  LPos : TAbstractMemPosition;
begin
  LSearch.Clear;
  LSearch.orphan := AOrphan;
  if FindData(LSearch,LPos, LFound) then begin
    Result := LFound.regsCounter;
  end else Result := 0;
end;

function TAbstractMemBlockchainStorage.TAMBTreeOrphanInformationByOrphan.LoadData(
  const APosition: TAbstractMemPosition): TOrphanInformation;
var LZone : TAMZone;
  LBytes : TBytes;
  LStream : TStream;
begin
  Result.Clear;
  if Not AbstractMem.GetUsedZoneInfo( APosition, False, LZone) then
    raise EAbstractMemBTree.Create(Format('%s.LoadData Inconsistency error used zone info not found at pos %d',[Self.ClassName,APosition]));
  SetLength(LBytes,LZone.size);
  if AbstractMem.Read(LZone.position, LBytes[0], Length(LBytes) )<>Length(LBytes) then
    raise EAbstractMemBTree.Create(Format('%s.LoadData Inconsistency error cannot read %d bytes at pos %d',[Self.ClassName,LZone.size,APosition]));
  LStream := TMemoryStream.Create;
  Try
    LStream.Write(LBytes[0],Length(LBytes));
    LStream.Position := 0;
    TStreamOp.ReadString(LStream,Result.orphan);
    LStream.Read(Result.regsCounter,4);
  Finally
    LStream.Free;
  End;
end;

function TAbstractMemBlockchainStorage.TAMBTreeOrphanInformationByOrphan.SaveData(
  const AData: TOrphanInformation): TAMZone;
var
  LBytes : TBytes;
  LStream : TStream;
begin
  LStream := TMemoryStream.Create;
  Try
    TStreamOp.WriteString(LStream,AData.orphan);
    LStream.Write(AData.regsCounter,4);
    SetLength(LBytes,LStream.Size);
    LBytes := TStreamOp.SaveStreamToRaw(LStream);
  Finally
    LStream.Free;
  End;
  Result := AbstractMem.New(Length(LBytes));
  AbstractMem.Write(Result.position,LBytes[0],Length(LBytes));
end;

procedure TAbstractMemBlockchainStorage.TAMBTreeOrphanInformationByOrphan.Update(
  const AOrphan: String; AIncrement: Integer);
var LUpdate, LUpdateFound : TOrphanInformation;
  LPos : TAbstractMemPosition;
begin
  LUpdate.Clear;
  LUpdate.orphan := AOrphan;
  if FindData(LUpdate,LPos, LUpdateFound) then begin
    LUpdate.regsCounter := LUpdateFound.regsCounter + AIncrement;
    DeleteData(LUpdateFound);
  end else begin
    Assert(AIncrement>0,'Creating an orphan with increment<=0 '+AIncrement.ToString);
    LUpdate.regsCounter := AIncrement;
  end;
  LUpdate.orphan := AOrphan;
  if not AddData(LUpdate) then raise EAbstractMemBlockchainStorage.Create(Format('Cannot update Orphan information for %s inc %d to %d',[AOrphan,AIncrement,LUpdate.regsCounter]));
end;

{ TAbstractMemBlockchainStorage.TOrphanInformation }

procedure TAbstractMemBlockchainStorage.TOrphanInformation.Clear;
begin
  Self.orphan:='';
  Self.regsCounter := 0;
end;

procedure TAbstractMemBlockchainStorage.TOrphanInformation.CopyFrom(
  const ASource: TOrphanInformation);
begin
  Self.orphan := ASource.orphan;
  Self.regsCounter := ASource.regsCounter;
end;

{ TAbstractMemBlockchainStorageSecondary }

constructor TAbstractMemBlockchainStorageSecondary.Create(AOwner: TComponent);
begin
  inherited;
  FAuxStorage := Nil;
end;

destructor TAbstractMemBlockchainStorageSecondary.Destroy;
begin
  FreeAndNil(FAuxStorage);
  inherited;
end;

procedure TAbstractMemBlockchainStorageSecondary.DoBlockNotFound(ABlock: Integer);
var LOperationsComp : TPCOperationsComp;
begin
  inherited;
  if (Assigned(FAuxStorage)) then begin
    LOperationsComp := TPCOperationsComp.Create(Nil);
    Try
      if FAuxStorage.LoadBlockChainBlock(LOperationsComp,ABlock) then begin
        TLog.NewLog(ltdebug,ClassName,Format('BlockNotFound Migrating block %d with %d operations',[ABlock,LOperationsComp.Count]));
        inherited DoSaveBlockChain(LOperationsComp);
      end;
    Finally
      LOperationsComp.Free;
    End;
  end;
end;

procedure TAbstractMemBlockchainStorageSecondary.DoDeleteBlockChainBlocks(
  StartingDeleteBlock: Cardinal);
begin
  inherited;
  if Assigned(FAuxStorage) then begin
    FAuxStorage.DeleteBlockChainBlocks(StartingDeleteBlock);
  end;
end;

function TAbstractMemBlockchainStorageSecondary.DoInitialize: Boolean;
begin
  Result := inherited DoInitialize;
  if (Result) And (Not Assigned(FAuxStorage)) then begin
    FAuxStorage := TFileStorage.Create(Self);
    FAuxStorage.Bank := Self.Bank;
    FAuxStorage.ReadOnly := Self.ReadOnly;
    Result := FAuxStorage.Initialize;
  end;
end;

function TAbstractMemBlockchainStorageSecondary.DoMoveBlockChain(
  StartBlock: Cardinal; const DestOrphan: TOrphan): Boolean;
begin
  Result := inherited;
  if (Result) and (Assigned(FAuxStorage)) then begin
    FAuxStorage.DeleteBlockChainBlocks(StartBlock);
  end;
end;

function TAbstractMemBlockchainStorageSecondary.DoSaveBlockChain(
  Operations: TPCOperationsComp): Boolean;
begin
  Result := inherited;
  if (Result) and (Assigned(FAuxStorage)) then begin
    Result := FAuxStorage.SaveBlockChainBlock(Operations);
  end;
end;

procedure TAbstractMemBlockchainStorageSecondary.SetReadOnly(
  const Value: Boolean);
begin
  inherited;
  if (Assigned(FAuxStorage)) then begin
    FAuxStorage.ReadOnly := Value;
    FAuxStorage.StorageFilename := '';
  end;
end;

{ TAbstractMemBlockchainStorage.TPendingToSaveThread }

procedure TAbstractMemBlockchainStorage.TPendingToSaveThread.BCExecute;
var LPendingList : TList<TPendingData>;
  LPending, LZero : TPendingData;
  i, nLastBatch : Integer;
begin
  //
  nLastBatch := 0;
  while (Not Terminated) do begin
    LPendingList := FPendingToSave.FPending.LockList;
    try
      if LPendingList.Count>0 then begin
        LZero := LPendingList.Items[0];
        LPending := LPendingList.Items[LPendingList.Count-1];
        LPendingList.Delete(LPendingList.Count-1);
        FBusy := True;
        if (FPendingToSave.FAMStorage.LogSaveActivity) and (TPlatform.GetElapsedMilliseconds(FPendingToSave.FLastLogTC)>10000) then begin

          TLog.NewLog(ltdebug,ClassName,Format('Pendings %d (%d/%d..%d/%d) - %s',
            [LPendingList.Count+1, LZero.operation.block, LZero.operation.opblock,
              LPending.operation.block, LPending.operation.opblock, FPendingToSave.FAMStorage.FSaveStorageStats.ToString]));
          FPendingToSave.FLastLogTC := TPlatform.GetTickCount;
          FPendingToSave.FAMStorage.FSaveStorageStats.Clear;
        end;
      end else FBusy := False;
    finally
      FPendingToSave.FPending.UnlockList;
    end;
    if (FBusy) then begin
      inc(nLastBatch);
      // Here will not terminate until finished job (or raised exception)
      DebugStep := Format('Block %d opBlock %d',[LPending.operation.block,LPending.operation.opblock]);
      if not FPendingToSave.FOperationsRawData_By_RightOpHash.AddData(LPending.operation) then
        raise EAbstractMemBlockchainStorage.Create(Format('Cannot add operation block %d opBlock %d',[LPending.operation.block,LPending.operation.opblock]));
      Inc(FPendingToSave.FAMStorage.FSaveStorageStats.operationRawDataCount);
      //
      for i := 0 to High(LPending.affectedAccounts) do begin
        DebugStep := Format('Block %d opBlock %d Account %d %d/%d',[LPending.operation.block,LPending.operation.opblock,LPending.affectedAccounts[i].account,i+1,Length(LPending.affectedAccounts)]);
        if not FPendingToSave.FAffectedAccounts_By_Account_Block_OpBlock.AddData(LPending.affectedAccounts[i]) then begin
          raise EAbstractMemBlockchainStorage.Create(Format('Cannot add affected account %d %d/%d in block %d opBlock %d',
            [LPending.affectedAccounts[i].account,
             i+1,Length(LPending.affectedAccounts),
             LPending.affectedAccounts[i].block,LPending.affectedAccounts[i].opblock]));
        end;
        Inc(FPendingToSave.FAMStorage.FSaveStorageStats.affectedAccountCount);
      end;
    end else begin
      if nLastBatch>0 then begin
        TLog.NewLog(ltdebug,ClassName,Format('Finished %d operations... waiting for more - %s',
            [nLastBatch, FPendingToSave.FAMStorage.FSaveStorageStats.ToString]));
        nLastBatch := 0;
        FPendingToSave.FAMStorage.FSaveStorageStats.Clear;
        FPendingToSave.FLastLogTC := FPendingToSave.FAMStorage.FSaveStorageStats.startTC;
        FPendingToSave.ThreadHasFinishedCurrentJob; // Notify in order to flush when all threads terminated
      end;
      Sleep(10);
    end;
  end;
end;

constructor TAbstractMemBlockchainStorage.TPendingToSaveThread.Create(
  APendingToSave: TPendingToSave);
begin
  FBusy := True;
  FPendingToSave := APendingToSave;
  inherited Create(True);
  FreeOnTerminate := False;
  Resume;
end;

{ TAbstractMemBlockchainStorage.TPendingToSave }

procedure TAbstractMemBlockchainStorage.TPendingToSave.AddPendingData(const APendingData: TPendingData);
var LPendings : TList<TPendingData>;
  LCount : Integer;
begin
  LPendings := FPending.LockList;
  Try
    LPendings.Add(APendingData);
    LCount := LPendings.Count;
    inc(FTotal);
  Finally
    FPending.UnlockList;
  End;
  if MaxThreads<1 then begin
    SetMaxThreads(1);
    repeat
      sleep(1);
    until PendingsCount=0;
    SetMaxThreads(0);
  end else SetMaxThreads( FMaxThreads );

  if (MaxPendingsCount>0) And (LCount>=MaxPendingsCount) then begin
    while (PendingsCount>=MaxPendingsCount) and (MaxPendingsCount>0) do begin
      Sleep(10);
    end;
  end;
end;

constructor TAbstractMemBlockchainStorage.TPendingToSave.Create(
  AStorage : TAbstractMemBlockchainStorage;
  AAMBTreeTOperationRawDataByRightOpHash: TAMBTreeTOperationRawDataByRightOpHash;
  AAMBTreeTAffectedAccountByAccountBlockOpBlock: TAMBTreeTAffectedAccountByAccountBlockOpBlock);
begin
  FAMStorage := AStorage;
  FTotal := 0;
  FMaxPendingsCount := 5000;
  FLastLogTC := TPlatform.GetTickCount;
  FOperationsRawData_By_RightOpHash := AAMBTreeTOperationRawDataByRightOpHash;
  FAffectedAccounts_By_Account_Block_OpBlock := AAMBTreeTAffectedAccountByAccountBlockOpBlock;
  FPending := TThreadList<TPendingData>.Create;
  FThreads := TThreadList<TPendingToSaveThread>.Create;
  SetMaxThreads( TCPUTool.GetLogicalCPUCount );
end;

destructor TAbstractMemBlockchainStorage.TPendingToSave.Destroy;
var i : Integer;
begin
  SetMaxThreads(0);
  i := PendingsCount;
  if i>0 then begin
    TLog.NewLog(lterror,ClassName,Format('ERROR: Finalizing Pending to save with %d pending operations!',[i]));
  end;
  if FTotal>0 then begin
    TLog.NewLog(ltdebug,ClassName,Format('Finalizing Pending to save with %d operations saved',[FTotal]));
  end;
  FreeAndNil(FPending);
  FreeAndNil(FThreads);
  inherited;
end;

function TAbstractMemBlockchainStorage.TPendingToSave.PendingsCount: Integer;
var  LPendings : TList<TPendingData>;
begin
  LPendings := FPending.LockList;
  Try
    Result := LPendings.Count;
  Finally
    FPending.UnlockList;
  End;
end;

procedure TAbstractMemBlockchainStorage.TPendingToSave.SetMaxThreads(const Value: Integer);
var i : Integer;
  ListTh : TList<TPendingToSaveThread>;
begin
  {$IFDEF HIGHLOG}
  if Value<>FMaxThreads then begin
    TLog.NewLog(ltdebug,ClassName,Format('Setting muxThreads from %d to %d',[FMaxThreads,Value]));
  end;
  {$ENDIF}
  if Value<0 then FMaxThreads := 0
  else if Value>16 then FMaxThreads := 16
  else FMaxThreads := Value;

  ListTh := FThreads.LockList;
  try
    // Clean terminateds...
    for i := ListTh.Count-1 downto 0 do begin
      if ListTh.Items[i].Terminated then begin
        ListTh.Items[i].Free;
        ListTh.Delete(i);
      end;
    end;
    // CREATE
    while ListTh.Count<FMaxThreads do begin
      ListTh.Add( TPendingToSaveThread.Create(Self) );
    end;
    // REMOVE
    for i := FMaxThreads to ListTh.Count-1 do begin
      ListTh.Items[i].Terminate;
    end;
    for i := ListTh.Count-1 downto FMaxThreads do begin
      ListTh.Items[i].WaitFor;
      ListTh.Items[i].Free;
      ListTh.Delete(i);
    end;
  finally
    FThreads.UnlockList;
  end;
end;

procedure TAbstractMemBlockchainStorage.TPendingToSave.ThreadHasFinishedCurrentJob;
var i : Integer;
  ListTh : TList<TPendingToSaveThread>;
begin
  if PendingsCount>0 then Exit;
  ListTh := FThreads.LockList;
  try
    for i := ListTh.Count-1 downto 0 do begin
      if ListTh.Items[i].Busy then Exit;  // Still working
    end;
  Finally
    FThreads.UnlockList;
  end;
  //
  FAMStorage.FinalizedUpdating;
end;

{ TAbstractMemBlockchainStorage.TPendingData }

procedure TAbstractMemBlockchainStorage.TPendingData.Clear;
begin
  Self.operation.Clear;
  SetLength(Self.affectedAccounts,0);
end;

{ TBlockchainStorageStats }

procedure TBlockchainStorageStats.AddTo(var ADest: TBlockchainStorageStats);
begin
  Inc(ADest.blockInformationCount,Self.blockInformationCount);
  Inc(ADest.operationRawDataCount,Self.operationRawDataCount);
  Inc(ADest.affectedAccountCount,Self.affectedAccountCount);
end;

procedure TBlockchainStorageStats.Clear;
begin
  Self.blockInformationCount := 0;
  Self.operationRawDataCount := 0;
  Self.affectedAccountCount := 0;
  Self.startTC := TPlatform.GetTickCount;
end;

function TBlockchainStorageStats.ThroughputPerSecond: Double;
var Lmilis : Int64;
  LRend : Double;
begin
  Lmilis := TPlatform.GetElapsedMilliseconds(Self.startTC);
  if LMilis>0 then begin
    Result := ((Self.blockInformationCount + Self.operationRawDataCount + Self.affectedAccountCount) / Lmilis)*1000;
  end else Result := 0;
end;

function TBlockchainStorageStats.ToString: String;
begin
  Result := format('Blocks:%d Operations:%d Accounts:%d secs:%.2f TPS:%.2f',
    [Self.blockInformationCount,Self.operationRawDataCount,Self.affectedAccountCount,
    TPlatform.GetElapsedMilliseconds(Self.startTC)/1000,Self.ThroughputPerSecond]);
end;

initialization
end.

