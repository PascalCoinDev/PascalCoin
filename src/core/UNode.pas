unit UNode;

{ Copyright (c) 2016 by Albert Molina

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

{ UNode contains the basic structure to operate
  - An app can only contains 1 node.
  - A node contains:
    - 1 Bank
    - 1 NetServer  (Accepting incoming connections)
    - 1 Operations (Operations has actual BlockChain with Operations and SafeBankTransaction to operate with the Bank)
    - 0..x NetClients
    - 0..x Miners
 }

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}


interface

uses
  Classes, SysUtils,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF}, UPCDataTypes, UEncoding,
  UBlockChain, UNetProtocol, UAccounts, UCrypto, UEPasa, UThread, SyncObjs, ULog, UBaseTypes, UPCOrderedLists;

{$I ./../config.inc}

Type

  { TNode }

  TNodeNotifyEvents = Class;

  TNode = Class;

  TSaveMempoolOperationsThread = Class(TPCThread)
  private
    FNode : TNode;
    FPendingToSave : Boolean;
  protected
    procedure BCExecute; override;
  public
    procedure Touch;
  End;

  TNode = Class(TComponent)
  private
    FNodeLog : TLog;
    FLockMempool : TPCCriticalSection;
    FOperationSequenceLock : TPCCriticalSection;
    FNotifyList : TList<TNodeNotifyEvents>;
    FBank : TPCBank;
    FMemPoolOperationsComp : TPCOperationsComp;
    FMemPoolAddingOperationsList : TOrderedRawList;
    FNetServer : TNetServer;
    FBCBankNotify : TPCBankNotify;
    FPeerCache : String;
    FDisabledsNewBlocksCount : Integer;
    FBroadcastData : Boolean;
    FUpdateBlockchain: Boolean;
    FMaxPayToKeyPurchasePrice: Int64;
    FSaveMempoolOperationsThread : TSaveMempoolOperationsThread;
    {$IFDEF BufferOfFutureOperations}
    FBufferAuxWaitingOperations : TOperationsHashTree;
    {$ENDIF}
    Procedure OnBankNewBlock(Sender : TObject);
    procedure SetNodeLogFilename(const Value: String);
    function GetNodeLogFilename: String;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    Class Function Node : TNode;
    Class Function NodeExists : Boolean;
    Class Procedure DecodeIpStringToNodeServerAddressArray(Const Ips : String; Var NodeServerAddressArray : TNodeServerAddressArray);
    Class Function EncodeNodeServerAddressArrayToIpString(Const NodeServerAddressArray : TNodeServerAddressArray) : String;
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Property Bank : TPCBank read FBank;
    Function NetServer : TNetServer;
    Procedure NotifyNetClientMessage(Sender : TNetConnection; Const TheMessage : String);

    // Return Operations count in the Mempool
    function MempoolOperationsCount : Integer;
    // Return Account based on current state (Safebox + Mempool operations)
    function GetMempoolAccount(AAccountNumber : Cardinal) : TAccount;
    // Locking methods to access to the Mempool
    function LockMempoolRead : TPCOperationsComp;
    procedure UnlockMempoolRead;
    function LockMempoolWrite : TPCOperationsComp;
    procedure UnlockMempoolWrite;
    //
    Function AddNewBlockChain(SenderConnection : TNetConnection; NewBlockOperations: TPCOperationsComp; var errors: String): Boolean;
    Function AddOperations(SenderConnection : TNetConnection; AOperationsHashTreeToAdd : TOperationsHashTree; OperationsResult : TOperationsResumeList; var errors: String): Integer;
    Function AddOperation(SenderConnection : TNetConnection; Operation : TPCOperation; var errors: String): Boolean;
    Function SendNodeMessage(Target : TNetConnection; const TheMessage : String; var errors : String) : Boolean;
    //
    Procedure NotifyBlocksChanged;
    //
    Function FindOperation(Const AOperationHash : TRawBytes; var AOperationResume : TOperationResume) : TSearchOpHashResult;
    Function FindNOperation(block, account, n_operation : Cardinal; var OpResume : TOperationResume) : TSearchOpHashResult;
    Function FindNOperations(account, start_block : Cardinal; allow_search_previous : Boolean; n_operation_low, n_operation_high : Cardinal; OpResumeList : TOperationsResumeList) : TSearchOpHashResult;
    //
    Procedure InitSafeboxAndOperations(max_block_to_read : Cardinal = $FFFFFFFF; restoreProgressNotify : TProgressNotify = Nil);
    Procedure AutoDiscoverNodes(Const ips : String);
    Function IsBlockChainValid(var WhyNot : String) : Boolean;
    Function IsReady(Var CurrentProcess : String) : Boolean;
    Property PeerCache : String read FPeerCache write FPeerCache;
    Procedure DisableNewBlocks;
    Procedure EnableNewBlocks;
    Property NodeLogFilename : String read GetNodeLogFilename write SetNodeLogFilename;
    Property OperationSequenceLock : TPCCriticalSection read FOperationSequenceLock;
    function TryLockNode(MaxWaitMilliseconds : Cardinal) : Boolean;
    procedure UnlockNode;
    //
    function GetAccountsAvailableByPublicKey(const APubKeys : TList<TAccountKey>; out AOnSafebox, AOnMempool : Integer) : Integer; overload;
    function GetAccountsAvailableByPublicKey(const APubKey : TAccountKey; out AOnSafebox, AOnMempool : Integer) : Integer; overload;
    //
    Property BroadcastData : Boolean read FBroadcastData write FBroadcastData;
    Property UpdateBlockchain : Boolean read FUpdateBlockchain write FUpdateBlockchain;
    procedure MarkVerifiedECDSASignaturesFromMemPool(newOperationsToValidate : TPCOperationsComp);
    class function NodeVersion : String;
    class function GetPascalCoinDataFolder : String;
    class procedure SetPascalCoinDataFolder(const ANewDataFolder : String);
    //
    function TryFindAccountByKey(const APubKey : TAccountKey; out AAccountNumber : Cardinal) : Boolean;
    function TryFindPublicSaleAccount(AMaximumPrice : Int64; APreventRaceCondition : Boolean; out AAccountNumber : Cardinal) : Boolean;
    Function TryResolveEPASA(const AEPasa : TEPasa; out AResolvedAccount: Cardinal): Boolean; overload;
    Function TryResolveEPASA(const AEPasa : TEPasa; out AResolvedAccount: Cardinal; out AErrorMessage: String): Boolean; overload;
    Function TryResolveEPASA(const AEPasa : TEPasa; out AResolvedAccount: Cardinal; out AResolvedKey : TAccountKey; out ARequiresPurchase : boolean): Boolean; overload;
    Function TryResolveEPASA(const AEPasa : TEPasa; out AResolvedAccount: Cardinal; out AResolvedKey : TAccountKey; out ARequiresPurchase : boolean; out AErrorMessage: String): Boolean; overload;

    Property MaxPayToKeyPurchasePrice: Int64 read FMaxPayToKeyPurchasePrice write FMaxPayToKeyPurchasePrice;
  End;

  TThreadSafeNodeNotifyEvent = Class(TPCThread)
    FNodeNotifyEvents : TNodeNotifyEvents;
    FNotifyBlocksChanged : Boolean;
    FNotifyOperationsChanged : Boolean;
    Procedure SynchronizedProcess;
  protected
    procedure BCExecute; override;
  public
    Constructor Create(ANodeNotifyEvents : TNodeNotifyEvents);
  End;

  { TNodeMessage Event }

  TNodeMessageEvent = Procedure(NetConnection : TNetConnection; MessageData : String) of object;

  { TNodeMessageManyEvent }

  TNodeMessageManyEvent = TArray<TNodeMessageEvent>;

  { TNodeMessageManyEventHelper }

  TNodeMessageManyEventHelper = record helper for TNodeMessageManyEvent
    procedure Add(listener : TNodeMessageEvent);
    procedure Remove(listener : TNodeMessageEvent);
    procedure Invoke(NetConnection : TNetConnection; MessageData : String);
  end;

  { TNodeNotifyEvents is ThreadSafe and will only notify in the main thread }
  TNodeNotifyEvents = Class(TComponent)
  private
    FNode: TNode;
    FOnKeyActivity: TNotifyEvent;
    FPendingNotificationsList : TPCThreadList<Pointer>;
    FThreadSafeNodeNotifyEvent : TThreadSafeNodeNotifyEvent;
    FOnBlocksChanged: TNotifyEvent;
    FOnOperationsChanged: TNotifyEvent;
    FMessages : TStringList;
    FOnNodeMessageEvent: TNodeMessageEvent;
    FWatchKeys: TOrderedAccountKeysList;
    procedure SetNode(const Value: TNode);
    Procedure NotifyBlocksChanged;
    Procedure NotifyOperationsChanged;
    procedure SetWatchKeys(AValue: TOrderedAccountKeysList);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Property Node : TNode read FNode write SetNode;
    Property OnBlocksChanged : TNotifyEvent read FOnBlocksChanged write FOnBlocksChanged;
    Property OnOperationsChanged : TNotifyEvent read FOnOperationsChanged write FOnOperationsChanged;
    Property OnNodeMessageEvent : TNodeMessageEvent read FOnNodeMessageEvent write FOnNodeMessageEvent;
    Property WatchKeys : TOrderedAccountKeysList read FWatchKeys write SetWatchKeys;
    Property OnKeyActivity : TNotifyEvent read FOnKeyActivity write FOnKeyActivity;
  End;

  TThreadNodeNotifyNewBlock = Class(TPCThread)
    FNetConnection : TNetConnection;
    FSanitizedOperationsHashTree : TOperationsHashTree;
    FNewBlockOperations : TPCOperationsComp;
  protected
    procedure BCExecute; override;
  public
    Constructor Create(NetConnection : TNetConnection; MakeACopyOfNewBlockOperations: TPCOperationsComp; MakeACopyOfSanitizedOperationsHashTree : TOperationsHashTree);
    destructor Destroy; override;
  End;

  TThreadNodeNotifyOperations = Class(TPCThread)
    FNetConnection : TNetConnection;
  protected
    procedure BCExecute; override;
  public
    Constructor Create(NetConnection : TNetConnection; MakeACopyOfOperationsHashTree : TOperationsHashTree);
    destructor Destroy; override;
  End;

implementation

Uses UOpTransaction, UConst, UTime, UCommon, UPCOperationsSignatureValidator,
  UFolderHelper, USettings;

var _Node : TNode;
  _PascalCoinDataFolder : String;

{ TNode }

function TNode.AddNewBlockChain(SenderConnection: TNetConnection; NewBlockOperations: TPCOperationsComp; var errors: String): Boolean;
Var i,j,maxResend : Integer;
  nc : TNetConnection;
  s,sClientRemoteAddr : String;
  OpBlock : TOperationBlock;
  opsht : TOperationsHashTree;
  minBlockResend : Cardinal;
  resendOp : TPCOperation;
  LLockedMempool : TPCOperationsComp;
begin
  Result := false;
  errors := '';
  if Assigned(SenderConnection) then sClientRemoteAddr := SenderConnection.ClientRemoteAddr
  else sClientRemoteAddr:='(SELF)';
  if FDisabledsNewBlocksCount>0 then begin
    TLog.NewLog(lterror,Classname,Format('Cannot Add new BlockChain due is adding disabled - Connection:%s NewBlock:%s',[
    sClientRemoteAddr,TPCOperationsComp.OperationBlockToText(NewBlockOperations.OperationBlock)]));
    errors := 'Adding blocks is disabled';
    exit;
  end;
  NewBlockOperations.Lock; // New protection
  Try
    If NewBlockOperations.OperationBlock.block<>Bank.BlocksCount then begin
      errors := 'New block number ('+IntToStr(NewBlockOperations.OperationBlock.block)+') not valid! (Expected '+IntToStr(Bank.BlocksCount)+')';
      exit;
    end;
    OpBlock := NewBlockOperations.OperationBlock;
    TLog.NewLog(ltdebug,Classname,Format('Starting AddNewBlockChain %d Operations %d from %s NewBlock:%s',[
      OpBlock.block,NewBlockOperations.Count,sClientRemoteAddr,TPCOperationsComp.OperationBlockToText(OpBlock)]));
    If Not TPCThread.TryProtectEnterCriticalSection(Self,5000,FLockMempool) then begin
      If NewBlockOperations.OperationBlock.block<>Bank.BlocksCount then exit;
      s := 'Cannot AddNewBlockChain due blocking lock operations node';
      TLog.NewLog(lterror,Classname,s);
      if TThread.CurrentThread.ThreadID=MainThreadID then raise Exception.Create(s) else exit;
    end;
    try
      // Check block number:
      if TPCOperationsComp.EqualsOperationBlock(Bank.LastOperationBlock,NewBlockOperations.OperationBlock) then begin
        errors := 'Duplicated block';
        exit;
      end;
      MarkVerifiedECDSASignaturesFromMemPool(NewBlockOperations); // Improvement speed v4.0.2
      // Improvement TNode speed 2.1.6
      // Does not need to save a FOperations backup because is Sanitized by "TNode.OnBankNewBlock"
      Result := Bank.AddNewBlockChainBlock(NewBlockOperations,TNetData.NetData.NetworkAdjustedTime.GetMaxAllowedTimestampForNewBlock,errors);
      if Result then begin
        {$IFDEF USE_ABSTRACTMEM}
        Bank.SafeBox.PCAbstractMem.FlushCache;
        {$ENDIF}
        if Assigned(SenderConnection) then begin
          FNodeLog.NotifyNewLog(ltupdate,SenderConnection.ClassName,Format(';%d;%s;%s;;%d;%d;%d;%s',[OpBlock.block,sClientRemoteAddr,OpBlock.block_payload.ToPrintable,
            OpBlock.timestamp,UnivDateTimeToUnix(DateTime2UnivDateTime(Now)),UnivDateTimeToUnix(DateTime2UnivDateTime(Now)) - OpBlock.timestamp,IntToHex(OpBlock.compact_target,8)]));
        end else begin
          FNodeLog.NotifyNewLog(ltupdate,ClassName,Format(';%d;%s;%s;;%d;%d;%d;%s',[OpBlock.block,'NIL',OpBlock.block_payload.ToPrintable,
            OpBlock.timestamp,UnivDateTimeToUnix(DateTime2UnivDateTime(Now)),UnivDateTimeToUnix(DateTime2UnivDateTime(Now)) - OpBlock.timestamp,IntToHex(OpBlock.compact_target,8)]));
        end;
      end else begin
        if Assigned(SenderConnection) then begin
          FNodeLog.NotifyNewLog(lterror,SenderConnection.ClassName,Format(';%d;%s;%s;%s;%d;%d;%d;%s',[OpBlock.block,sClientRemoteAddr,OpBlock.block_payload.ToPrintable,errors,
            OpBlock.timestamp,UnivDateTimeToUnix(DateTime2UnivDateTime(Now)),UnivDateTimeToUnix(DateTime2UnivDateTime(Now)) - OpBlock.timestamp,IntToHex(OpBlock.compact_target,8)]));
        end else begin
          FNodeLog.NotifyNewLog(lterror,ClassName,Format(';%d;%s;%s;%s;%d;%d;%d;%s',[OpBlock.block,'NIL',OpBlock.block_payload.ToPrintable,errors,
            OpBlock.timestamp,UnivDateTimeToUnix(DateTime2UnivDateTime(Now)),UnivDateTimeToUnix(DateTime2UnivDateTime(Now)) - OpBlock.timestamp,IntToHex(OpBlock.compact_target,8)]));
        end;
      end;
      if Result then begin
        opsht := TOperationsHashTree.Create;
        Try
          j := Random(5);
          If (Bank.LastBlockFound.OperationBlock.block>j) then
            minBlockResend:=Bank.LastBlockFound.OperationBlock.block - j
          else minBlockResend:=1;
          maxResend := CT_MaxResendMemPoolOperations;
          i := 0;
          LLockedMempool := LockMempoolRead;
          Try

          While (opsht.OperationsCount<maxResend) And (i<LLockedMempool.Count) do begin
            resendOp := LLockedMempool.Operation[i];
            j := resendOp.ResendOnBlock;
            if ((resendOp.ResendCount<2) and ((j<=0) Or (j<=minBlockResend))) then begin
              // Only will "re-send" operations that where received on block <= minBlockResend
              opsht.AddOperationToHashTree(resendOp);
              // Add to sent operations
              resendOp.ResendOnBlock := LLockedMempool.OperationBlock.block;
              resendOp.ResendCount := resendOp.ResendCount + 1;
            end;
            inc(i);
          end;
          If opsht.OperationsCount>0 then begin
            TLog.NewLog(ltinfo,classname,Format('Resending %d operations for new block (Mempool Pending Operations:%d)',[opsht.OperationsCount,LLockedMempool.Count]));
            {$IFDEF HIGHLOG}
            if opsht.OperationsCount>0 then begin
              for i := 0 to opsht.OperationsCount - 1 do begin
                TLog.NewLog(ltInfo,ClassName,'Resending ('+inttostr(i+1)+'/'+inttostr(opsht.OperationsCount)+'): '+opsht.GetOperation(i).ToString);
              end;
            end
            {$ENDIF}
          end;
          Finally
            UnlockMempoolRead;
          End;
          // Notify to clients
          {$IFnDEF TESTING_NO_POW_CHECK}
          if FBroadcastData then begin
            j := TNetData.NetData.ConnectionsCountAll;
            for i:=0 to j-1 do begin
              if (TNetData.NetData.GetConnection(i,nc)) then begin
                if (nc.Connected) And (nc.RemoteOperationBlock.block>0) then begin
                  if (nc<>SenderConnection) then begin
                    TThreadNodeNotifyNewBlock.Create(nc,Bank.LastBlockFound,opsht);
                  end else if (opsht.OperationsCount>0) then begin
                    // New 4.0.1 Notify not added operations
                    TThreadNodeNotifyOperations.Create(nc,opsht);
                  end;
                end;
              end;
            end;
          end;
          {$ENDIF}
        Finally
          opsht.Free;
        End;
      end;
    finally
      FLockMempool.Release;
      TLog.NewLog(ltdebug,Classname,Format('Finalizing AddNewBlockChain %d Operations %d from %s NewBlock:%s',[
        OpBlock.block,NewBlockOperations.Count,sClientRemoteAddr,TPCOperationsComp.OperationBlockToText(OpBlock)]));
    End;
  finally
    NewBlockOperations.Unlock;
  end;
  if Result then begin
    // Notify it!
    NotifyBlocksChanged;
  end;
end;

function TNode.AddOperation(SenderConnection : TNetConnection; Operation: TPCOperation; var errors: String): Boolean;
var ops : TOperationsHashTree;
begin
  ops := TOperationsHashTree.Create;
  Try
    ops.AddOperationToHashTree(Operation);
    Result := AddOperations(SenderConnection,ops,Nil,errors)=1;
  Finally
    ops.Free;
  End;
end;

function TNode.AddOperations(SenderConnection : TNetConnection; AOperationsHashTreeToAdd : TOperationsHashTree; OperationsResult : TOperationsResumeList; var errors: String): Integer;
  {$IFDEF BufferOfFutureOperations}
  Procedure Process_BufferOfFutureOperations(ALockedMempool : TPCOperationsComp; valids_operations : TOperationsHashTree);
  Var i,j, nAdded, nDeleted : Integer;
    sAcc : TAccount;
    ActOp : TPCOperation;
    e : String;
  Begin
    // Prior to add new operations, will try to add waiting ones
    nAdded := 0; nDeleted := 0;
    For j:=0 to 3 do begin
      i := 0;
      While (i<FBufferAuxWaitingOperations.OperationsCount) do begin
        ActOp := FBufferAuxWaitingOperations.GetOperation(i);
        If ALockedMempool.AddOperation(true,ActOp,e) then begin
          TLog.NewLog(ltInfo,Classname,Format('AddOperation FromBufferWaitingOperations %d/%d: %s',[i+1,FBufferAuxWaitingOperations.OperationsCount,ActOp.ToString]));
          inc(nAdded);
          valids_operations.AddOperationToHashTree(ActOp);
          FBufferAuxWaitingOperations.Delete(i);
        end else begin
          sAcc := ALockedMempool.SafeBoxTransaction.Account(ActOp.SignerAccount);
          If (sAcc.n_operation>ActOp.N_Operation) Or
             ((sAcc.n_operation=ActOp.N_Operation) AND (sAcc.balance>0)) then begin
             FBufferAuxWaitingOperations.Delete(i);
             inc(nDeleted);
          end else inc(i);
        end;
      end;
    end;
    If (nAdded>0) or (nDeleted>0) or (FBufferAuxWaitingOperations.OperationsCount>0) then begin
      TLog.NewLog(ltInfo,Classname,Format('FromBufferWaitingOperations status - Added:%d Deleted:%d Buffer:%d',[nAdded,nDeleted,FBufferAuxWaitingOperations.OperationsCount]));
    end;
  end;
  {$ENDIF}
Var
  i,j,nSpam,nError,nRepeated : Integer;
  LValids_operations : TOperationsHashTree;
  nc : TNetConnection;
  e : String;
  s : String;
  OPR : TOperationResume;
  ActOp : TPCOperation;
  {$IFDEF BufferOfFutureOperations}sAcc : TAccount;{$ENDIF}
  LLockedMempool : TPCOperationsComp;
  LOpsToAdd : TList<TPCOperation>;
  LTempSafeboxTransaction : TPCSafeBoxTransaction;
  LTickCount : TTickCount;
begin
  Result := -1; // -1 Means Node is blocked or disabled
  if Assigned(OperationsResult) then OperationsResult.Clear;
  if FDisabledsNewBlocksCount>0 then begin
    errors := Format('Cannot Add Operations due is adding disabled - OpCount:%d',[AOperationsHashTreeToAdd.OperationsCount]);
    TLog.NewLog(ltinfo,Classname,errors);
    exit;
  end;
  nSpam := 0;
  nRepeated := 0;
  nError := 0;
  errors := '';
  Result := 0;
  LTickCount := TPlatform.GetTickCount;
  LValids_operations := TOperationsHashTree.Create;
  try
    LOpsToAdd := TList<TPCOperation>.Create;
    try
      // In order to allow income operations from multiple threads will divide the
      // process in LOCKING steps: (instead of a single global locking)
      // 1 - Add received AOperationsHashTreeToAdd in global FMemPoolAddingOperationsList
      //     without duplicates. This allows receive same operation twice and execute
      //     only first received
      // 2 - Verify signatures in a multithread (if CPU's available)
      // 3 - For each not repeated operation, try to add to mempool

      // Step 1: Add operations to FMemPoolAddingOperationsList
      LLockedMempool := LockMempoolWrite;
      try
        for i := 0 to AOperationsHashTreeToAdd.OperationsCount-1 do begin
          ActOp := AOperationsHashTreeToAdd.GetOperation(i);
          j := FMemPoolAddingOperationsList.IndexOf( ActOp.Sha256 );
          if (j<0) then begin
            LOpsToAdd.Add(ActOp);
            FMemPoolAddingOperationsList.Add(ActOp.Sha256);
          end;
        end;
      finally
        UnlockMempoolWrite;
      end;

      // Step 2:
      LTempSafeboxTransaction := TPCSafeBoxTransaction.Create(Bank.SafeBox);
      try
        TPCOperationsSignatureValidator.MultiThreadPreValidateSignatures(LTempSafeboxTransaction,LOpsToAdd,Nil);
      finally
        LTempSafeboxTransaction.Free;
      end;

      {$IFDEF BufferOfFutureOperations}
      LLockedMempool := LockMempoolWrite;
      try
        Process_BufferOfFutureOperations(LLockedMempool,LValids_operations);
      finally
        UnlockMempoolWrite;
      end;
      {$ENDIF}

      // Step 3:
      for j := 0 to LOpsToAdd.Count-1 do begin
        ActOp := LOpsToAdd[j];
        LLockedMempool := LockMempoolWrite;
        try

          If (LLockedMempool.OperationsHashTree.IndexOfOperation(ActOp)<0) then begin
            // Protocol 2 limitation: In order to prevent spam of operations without Fee, will protect it
            If (ActOp.OperationFee=0) And (Bank.SafeBox.CurrentProtocol>=CT_PROTOCOL_2) And
               (LLockedMempool.OperationsHashTree.CountOperationsBySameSignerWithoutFee(ActOp.SignerAccount)>=CT_MaxAccountOperationsPerBlockWithoutFee) then begin
              inc(nSpam);
              e := Format('Account %s zero fee operations per block limit:%d',[TAccountComp.AccountNumberToAccountTxtNumber(ActOp.SignerAccount),CT_MaxAccountOperationsPerBlockWithoutFee]);
              if (nSpam<=5) then begin  // To Limit errors in a String... speed up
                if (errors<>'') then errors := errors+' ';
                errors := errors+'Op '+IntToStr(j+1)+'/'+IntToStr(LOpsToAdd.Count)+':'+e;
              end;
              {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,Classname,Format('AddOperation spam %d/%d: %s  - Error:%s',[(j+1),LOpsToAdd.Count,ActOp.ToString,e]));{$ENDIF}
              if Assigned(OperationsResult) then begin
                TPCOperation.OperationToOperationResume(0,ActOp,True,ActOp.SignerAccount,OPR);
                OPR.valid := false;
                OPR.NOpInsideBlock:=-1;
                OPR.OperationHash := Nil;
                OPR.errors := e;
                OperationsResult.Add(OPR);
              end;
            end else begin
              if (LLockedMempool.AddOperation(true,ActOp,e)) then begin
                inc(Result);
                ActOp.DiscoveredOnBlock := LLockedMempool.OperationBlock.block;
                LValids_operations.AddOperationToHashTree(ActOp);
                {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,Classname,Format('AddOperation %d/%d: %s',[(j+1),LOpsToAdd.Count,ActOp.ToString]));{$ENDIF}
                if Assigned(OperationsResult) then begin
                  TPCOperation.OperationToOperationResume(0,ActOp,True,ActOp.SignerAccount,OPR);
                  OPR.NOpInsideBlock:=LLockedMempool.Count-1;
                  OPR.Balance := LLockedMempool.SafeBoxTransaction.Account(ActOp.SignerAccount).balance;
                  OperationsResult.Add(OPR);
                end;
              end else begin
                inc(nError);
                if (nError<=5) then begin  // To Limit errors in a String... speed up
                  if (errors<>'') then errors := errors+' ';
                  errors := errors+'Op '+IntToStr(j+1)+'/'+IntToStr(LOpsToAdd.Count)+':'+e;
                end;
                {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,Classname,Format('AddOperation invalid/duplicated %d/%d: %s  - Error:%s',[(j+1),LOpsToAdd.Count,ActOp.ToString,e]));{$ENDIF}
                if Assigned(OperationsResult) then begin
                  TPCOperation.OperationToOperationResume(0,ActOp,True,ActOp.SignerAccount,OPR);
                  OPR.valid := false;
                  OPR.NOpInsideBlock:=-1;
                  OPR.OperationHash := Nil;
                  OPR.errors := e;
                  OperationsResult.Add(OPR);
                end;
                {$IFDEF BufferOfFutureOperations}
                // Used to solve 2.0.0 "invalid order of operations" bug
                If (Assigned(SenderConnection)) Then begin
                  if ActOp.SignerAccount<LLockedMempool.SafeBoxTransaction.FreezedSafeBox.AccountsCount then begin
                    sAcc := LLockedMempool.SafeBoxTransaction.Account(ActOp.SignerAccount);
                    If (sAcc.n_operation<ActOp.N_Operation) Or
                       ((sAcc.n_operation=ActOp.N_Operation) AND (sAcc.balance=0) And (ActOp.OperationFee>0) And (ActOp.OpType = CT_Op_Changekey)) then begin
                      If FBufferAuxWaitingOperations.IndexOfOperation(ActOp)<0 then begin
                        FBufferAuxWaitingOperations.AddOperationToHashTree(ActOp);
                        TLog.NewLog(ltInfo,Classname,Format('New FromBufferWaitingOperations %d/%d (new buffer size:%d): %s',[j+1,LOpsToAdd.Count,FBufferAuxWaitingOperations.OperationsCount,ActOp.ToString]));
                      end;
                    end;
                  end;
                end;
                {$ENDIF}
              end;
            end;
          end else begin
            inc(nRepeated);
            e := Format('AddOperation made before %d/%d: %s',[(j+1),LOpsToAdd.Count,ActOp.ToString]);
            if (nRepeated<=5) then begin  // To Limit errors in a String... speed up
              if (errors<>'') then errors := errors+' ';
              errors := errors + e;
            end;
            if Assigned(OperationsResult) then begin
              TPCOperation.OperationToOperationResume(0,ActOp,True,ActOp.SignerAccount,OPR);
              OPR.valid := false;
              OPR.NOpInsideBlock:=-1;
              OPR.OperationHash := Nil;
              OPR.errors := e;
              OperationsResult.Add(OPR);
            end;
            {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,Classname,Format('AddOperation made before %d/%d: %s',[(j+1),LOpsToAdd.Count,ActOp.ToString]));{$ENDIF}
          end;
        finally
          UnlockMempoolWrite;
        end;
      end; // for i
      If Result<>0 then begin
        FSaveMempoolOperationsThread.Touch; // This will indicate to thread that mempool needs to be saved
        LTickCount := TPlatform.GetElapsedMilliseconds(LTickCount);
        if LTickCount=0 then LTickCount:=1;
        if Assigned(SenderConnection) then begin
          s := SenderConnection.ClientRemoteAddr;
        end else s := '(SELF)';
        {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,Classname,Format('Finalizing AddOperations from %s Operations:%d of %d valids:%d spam:%d invalids:%d repeated:%d Miliseconds:%d %.1f ops/sec',
          [s,LOpsToAdd.Count,AOperationsHashTreeToAdd.OperationsCount,Result,nSpam,nError,nRepeated,LTickCount,LOpsToAdd.Count * 1000 / LTickCount]));{$ENDIF}
        if FBroadcastData then begin
          // Send to other nodes
          j := TNetData.NetData.ConnectionsCountAll;
          for i:=0 to j-1 do begin
            If TNetData.NetData.GetConnection(i,nc) then begin
              if (nc<>SenderConnection) And (nc.Connected) And (nc.RemoteOperationBlock.block>0) then TThreadNodeNotifyOperations.Create(nc,LValids_operations);
            end;
          end;
        end;
      end;
    finally
      // Remove LOpsToAdd from FMemPoolAddingOperationsList
      LLockedMempool := LockMempoolWrite;
      try
        for i := 0 to LOpsToAdd.Count-1 do begin
          ActOp := LOpsToAdd[i];
          FMemPoolAddingOperationsList.Remove(ActOp.Sha256);
        end;
      finally
        UnlockMempoolWrite;
      end;
      LOpsToAdd.Free;
    end;
  finally
    LValids_operations.Free;
  end;
  // Notify it!
  for i := 0 to FNotifyList.Count-1 do begin
    TNodeNotifyEvents( FNotifyList[i] ).NotifyOperationsChanged;
  end;
end;

procedure TNode.AutoDiscoverNodes(const ips: String);
Var i,j : Integer;
  nsarr : TNodeServerAddressArray;
begin
  DecodeIpStringToNodeServerAddressArray(ips+';'+PeerCache,nsarr);
  for i := low(nsarr) to high(nsarr) do begin
    TNetData.NetData.AddServer(nsarr[i]);
  end;
  j := (CT_MaxServersConnected -  TNetData.NetData.ConnectionsCount(true));
  if j<=0 then exit;
  TNetData.NetData.DiscoverServers;
end;

constructor TNode.Create(AOwner: TComponent);
begin
  FMaxPayToKeyPurchasePrice := 0;
  FNodeLog := TLog.Create(Self);
  FNodeLog.ProcessGlobalLogs := false;
  RegisterOperationsClass;
  if Assigned(_Node) then raise Exception.Create('Duplicate nodes protection');
  TLog.NewLog(ltInfo,ClassName,'TNode.Create '+NodeVersion);
  inherited;
  FDisabledsNewBlocksCount := 0;
  FLockMempool := TPCCriticalSection.Create('TNode_LockMempool');
  FOperationSequenceLock := TPCCriticalSection.Create('TNode_OperationSequenceLock');
  FBank := TPCBank.Create(Self);
  FBCBankNotify := TPCBankNotify.Create(Self);
  FBCBankNotify.Bank := FBank;
  FBCBankNotify.OnNewBlock := OnBankNewBlock;
  FNetServer := TNetServer.Create;
  FMemPoolOperationsComp := TPCOperationsComp.Create(Nil);
  FMemPoolOperationsComp.bank := FBank;
  FNotifyList := TList<TNodeNotifyEvents>.Create;
  FMemPoolAddingOperationsList := TOrderedRawList.Create;
  {$IFDEF BufferOfFutureOperations}
  FBufferAuxWaitingOperations := TOperationsHashTree.Create;
  {$ENDIF}
  FBroadcastData := True;
  FUpdateBlockchain := True;
  FSaveMempoolOperationsThread := TSaveMempoolOperationsThread.Create(True);
  FSaveMempoolOperationsThread.FNode := Self;
  FSaveMempoolOperationsThread.Resume;
  if Not Assigned(_Node) then _Node := Self;
end;

class procedure TNode.DecodeIpStringToNodeServerAddressArray(const Ips: String;
  var NodeServerAddressArray: TNodeServerAddressArray);
  Function GetIp(var ips_string : String; var nsa : TNodeServerAddress) : Boolean;
  Const CT_IP_CHARS = ['a'..'z','A'..'Z','0'..'9','.','-','_'];
  var i : Integer;
    port : String;
  begin
    nsa := CT_TNodeServerAddress_NUL;
    Result := false;
    if length(trim(ips_string))=0 then begin
      ips_string := '';
      exit;
    end;
    // Delete invalid chars:
    i := 0;
    while (i<=(ips_string.Length-1)) AND (NOT (ips_string.Chars[i] IN CT_IP_CHARS)) do inc(i);
    if (i>0) then ips_string := ips_string.Substring(i,ips_string.Length);
    // Capture IP value
    i := 0;
    while (i<=(ips_string.Length-1)) and (ips_string.Chars[i] in CT_IP_CHARS) do inc(i);
    if (i>0) then begin
      nsa.ip := ips_string.Substring(0,i);
      // Capture possible :Port value
      if (i<=(ips_string.Length-1)) and (ips_string.Chars[i]=':') then begin
        inc(i);
        port := '';
        while (i<=(ips_string.Length-1)) and (ips_string.Chars[i] in ['0'..'9']) do begin
          port := port + ips_string.Chars[i];
          inc(i);
        end;
        nsa.port := StrToIntDef(port,0);
      end;
    end;
    ips_string := ips_string.Substring(i+1,Length(ips_string));
    if nsa.port=0 then nsa.port := CT_NetServer_Port;
    Result := (Trim(nsa.ip)<>'');
  end;
Var i,j : Integer;
  ips_string : String;
  nsa : TNodeServerAddress;
begin
  SetLength(NodeServerAddressArray,0);
  ips_string := Ips;
  repeat
    If GetIp(ips_string,nsa) then begin
      SetLength(NodeServerAddressArray,length(NodeServerAddressArray)+1);
      NodeServerAddressArray[High(NodeServerAddressArray)] := nsa;
    end;
  until (Length(ips_string)=0);
end;

destructor TNode.Destroy;
Var step : String;
begin
  TLog.NewLog(ltInfo,ClassName,'TNode.Destroy START');
  Try
    step := 'Deleting SaveMempoolOperationsThread';
    FSaveMempoolOperationsThread.Terminate;
    FSaveMempoolOperationsThread.WaitFor;
    FreeAndNil(FSaveMempoolOperationsThread);

    step := 'Deleting critical section';
    FreeAndNil(FLockMempool);
    FreeAndNil(FOperationSequenceLock);

    step := 'Desactivating server';
    FNetServer.Active := false;

    step := 'Destroying NetServer';
    FreeAndNil(FNetServer);

    step := 'Destroying NotifyList';
    FreeAndNil(FNotifyList);
    step := 'Destroying Operations';
    FreeAndNil(FMemPoolOperationsComp);
    FreeAndNil(FMemPoolAddingOperationsList);
    step := 'Assigning NIL to node var';
    if _Node=Self then _Node := Nil;

    step := 'Destroying Bank';
    FreeAndNil(FBCBankNotify);
    FreeAndNil(FBank);
    {$IFDEF BufferOfFutureOperations}
    FreeAndNil(FBufferAuxWaitingOperations);
    {$ENDIF}
    step := 'inherited';
    FreeAndNil(FNodeLog);
    inherited;
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,Classname,'Error destroying Node step: '+step+' Errors ('+E.ClassName+'): ' +E.Message);
      Raise;
    end;
  End;
  TLog.NewLog(ltInfo,ClassName,'TNode.Destroy END');
end;

procedure TNode.DisableNewBlocks;
begin
  inc(FDisabledsNewBlocksCount);
end;

procedure TNode.EnableNewBlocks;
begin
  if FDisabledsNewBlocksCount=0 then raise Exception.Create('Dev error 20160924-1');
  dec(FDisabledsNewBlocksCount);
end;

function TNode.TryFindAccountByKey(const APubKey: TAccountKey;
  out AAccountNumber: Cardinal): Boolean;
  // Finds the smallest numbered account with selected key (or returns false)
var Lpka : TSafeboxPubKeysAndAccounts;
  LAccountsNumberList : TAccountsNumbersList;
begin
  Result := False;
  Lpka := Bank.SafeBox.OrderedAccountKeysList;
  if Assigned(Lpka) then begin
    LAccountsNumberList := Lpka.GetAccountsUsingThisKey(APubKey);
    if Assigned(LAccountsNumberList) then begin
      if LAccountsNumberList.Count>0 then begin
        AAccountNumber := LAccountsNumberList.Get(0);
        Result := True;
      end;
    end;
  end;
end;

function TNode.TryFindPublicSaleAccount(AMaximumPrice: Int64; APreventRaceCondition : Boolean;
  out AAccountNumber: Cardinal): Boolean;
  // Finds an account at or below argument purchase price (or returns false)
  // APreventRaceCondition: When True will return a random account in valid range price
  // Limitations: Account must be >0
var LtempAccNumber : Int64;
  LLastValidAccount, LCurrAccount : TAccount;
  LContinueSearching : Boolean;
begin
  Result := False;

  // Sorted list: Bank.SafeBox.AccountsOrderedBySalePrice
  // Note: List is sorted by Sale price (ASCENDING), but NOT by public/private sale, must check

  if Not Bank.SafeBox.AccountsOrderedBySalePrice.FindLowest(LtempAccNumber) then Exit(False);
  LCurrAccount := GetMempoolAccount(LtempAccNumber);

  if (LCurrAccount.accountInfo.price<=AMaximumPrice)
    and (TAccountComp.IsAccountForPublicSale(LCurrAccount.accountInfo)) then begin
    LLastValidAccount := LCurrAccount;
    LContinueSearching := (APreventRaceCondition) And (Random(50)=0);
  end else begin
    LLastValidAccount := CT_Account_NUL;
    LContinueSearching := True;
  end;

  while (LCurrAccount.accountInfo.price<=AMaximumPrice) and (LContinueSearching) do begin

    if TAccountComp.IsAccountForPublicSale(LCurrAccount.accountInfo) then LLastValidAccount := LCurrAccount;

    if Not (Bank.SafeBox.AccountsOrderedBySalePrice.FindSuccessor(LtempAccNumber,LtempAccNumber)) then Break;
    LCurrAccount := GetMempoolAccount(LtempAccNumber);

    // If price increased, then do not continue and use LastValidAccount
    if (LLastValidAccount.account>0)
      and (LLastValidAccount.accountInfo.price <> LCurrAccount.accountInfo.price) then Break;

    // Continue?
    LContinueSearching :=
      (LLastValidAccount.account=0) // This means that no valid account has been found yet...
      or
      (LContinueSearching And (Random(50)=0)); // Random prevention
  end;
  if (LLastValidAccount.account>0) then begin
    AAccountNumber := LLastValidAccount.account;
    Result := True;
  end else begin
    AAccountNumber := 0;
    Result := False;
  end;
end;

Function TNode.TryResolveEPASA(const AEPasa : TEPasa; out AResolvedAccount: Cardinal): Boolean;
var LErrMsg : String;
begin
  Result := TryResolveEPASA(AEPasa, AResolvedAccount, LErrMsg);
end;

Function TNode.TryResolveEPASA(const AEPasa : TEPasa; out AResolvedAccount: Cardinal; out AErrorMessage: String): Boolean;
var
  LAccountKey : TAccountKey;
  LRequiresPurchase : Boolean;
begin
  Result := TryResolveEPASA(AEPasa, AResolvedAccount, LAccountKey, LRequiresPurchase, AErrorMessage);
  if Result AND AEPasa.IsPayToKey then begin
    Result := False;
    AErrorMessage := 'EPASA was a pay-to-key style';
  end;
end;

Function TNode.TryResolveEPASA(const AEPasa : TEPasa; out AResolvedAccount: Cardinal; out AResolvedKey : TAccountKey; out ARequiresPurchase : boolean): Boolean;
var LErrMsg : String;
begin
  Result := TryResolveEPASA(AEPasa, AResolvedAccount, AResolvedKey, ARequiresPurchase, LErrMsg);
end;

Function TNode.TryResolveEPASA(const AEPasa : TEPasa; out AResolvedAccount: Cardinal; out AResolvedKey : TAccountKey; out ARequiresPurchase : boolean; out AErrorMessage: String): Boolean;
var
  LErrMsg : String;
begin
  AResolvedAccount := 0;
  AResolvedKey.Clear;
  ARequiresPurchase := False;
  AErrorMessage := '';
  if (AEPasa.IsPayToKey) then begin
    // Parse account key in EPASA
    if NOT TAccountComp.AccountPublicKeyImport(AEPasa.Payload, AResolvedKey, LErrMsg) then begin
      AResolvedAccount := CT_AccountNo_NUL;
      AResolvedKey := CT_Account_NUL.accountInfo.accountKey;
      ARequiresPurchase := False;
      AErrorMessage := Format('Invalid key specified in PayToKey EPASA "%s". %s',[AEPasa.ToString(), LErrMsg]);
      Exit(False);
    end;

    // Try to find key in safebox
    if TryFindAccountByKey(AResolvedKey, AResolvedAccount) then begin
      // Key already exists in SafeBox, so send to that account
      ARequiresPurchase := False;
      Exit(True);
    end;

    // If no key found, find optimal public purchase account
    if TryFindPublicSaleAccount(MaxPayToKeyPurchasePrice, True, AResolvedAccount) then begin
      // Account needs to be purchased
      ARequiresPurchase := True;
      Exit(True);
    end;

    // Account could not be resolved
    AResolvedAccount := CT_AccountNo_NUL;
    AResolvedKey := CT_Account_NUL.accountInfo.accountKey;
    ARequiresPurchase := False;
    AErrorMessage := 'No account could be resolved for pay to key EPASA';
    Exit(False);

  end else if (AEPasa.IsAddressedByName) then begin
    // Find account by name
    AResolvedAccount := Bank.SafeBox.FindAccountByName(AEPasa.AccountName);
    AResolvedKey := CT_Account_NUL.accountInfo.accountKey;
    ARequiresPurchase := False;
    if AResolvedAccount = CT_AccountNo_NUL then begin
      // No account with name found
      AResolvedAccount := CT_AccountNo_NUL;
      AResolvedKey := CT_Account_NUL.accountInfo.accountKey;
      ARequiresPurchase := False;
      AErrorMessage := Format('No account with name "%s" was found', [AEPasa.AccountName]);
      Exit(False);
    end;
    Exit(True);
  end;
  // addressed by number
  if NOT AEPasa.IsAddressedByNumber then raise Exception.Create('Internal Error c8ecd69d-3621-4f5e-b4f1-9926ab2f5013');
  if NOT AEPasa.Account.HasValue then raise Exception.Create('Internal Error 544c8cb9-b700-4b5f-93ca-4d045d0a06ae');
  AResolvedAccount := AEPasa.Account.Value;
  if (AResolvedAccount < 0) or (AResolvedAccount >= Self.Bank.AccountsCount) then begin
    AResolvedAccount := CT_AccountNo_NUL;
    AResolvedKey := CT_Account_NUL.accountInfo.accountKey;
    ARequiresPurchase := False;
    AErrorMessage := Format('Account number %d does not exist in safebox',[AEPasa.Account.Value]);
    Exit(False);
  end;
  Result := true;
end;


function TNode.TryLockNode(MaxWaitMilliseconds: Cardinal): Boolean;
begin
  Result := TPCThread.TryProtectEnterCriticalSection(Self,MaxWaitMilliseconds,FLockMempool);
end;

procedure TNode.UnlockNode;
begin
  FLockMempool.Release;
end;

procedure TNode.MarkVerifiedECDSASignaturesFromMemPool(newOperationsToValidate: TPCOperationsComp);
var LLockedMempool : TPCOperationsComp;
begin
  // Introduced on Build 4.0.2 to increase speed using MEMPOOL verified operations instead of verify again everytime
  // Will check if "newOperationsToValidate" operations are on MEMPOOL. If found, will set same FHasValidSignature value in order to mark as verified
  LLockedMempool := LockMempoolRead;
  try
    if newOperationsToValidate = LLockedMempool then Exit; // Is the same, do nothing
    if newOperationsToValidate.OperationBlock.protocol_version <> newOperationsToValidate.OperationBlock.protocol_version then Exit; // Must be same protocol
    newOperationsToValidate.Lock;
    try
      LLockedMempool.OperationsHashTree.MarkVerifiedECDSASignatures(newOperationsToValidate.OperationsHashTree);
    finally
      newOperationsToValidate.Unlock;
    end;
  finally
    UnlockMempoolRead;
  end;
end;

class function TNode.EncodeNodeServerAddressArrayToIpString(const NodeServerAddressArray: TNodeServerAddressArray): String;
var i : Integer;
begin
  Result := '';
  for i := low(NodeServerAddressArray) to high(NodeServerAddressArray) do begin
    if (Result<>'') then Result := Result + ';';
    Result := Result + NodeServerAddressArray[i].ip;
    if NodeServerAddressArray[i].port>0 then begin
      Result := Result + ':'+IntToStr(NodeServerAddressArray[i].port);
    end;
  end;
end;

function TNode.GetNodeLogFilename: String;
begin
  Result := FNodeLog.FileName;
end;

function TNode.IsBlockChainValid(var WhyNot : String): Boolean;
Var unixtimediff : Integer;
begin
  Result :=false;
  if (TNetData.NetData.NetStatistics.ActiveConnections<=0)  then begin
    WhyNot := 'No connection to check blockchain';
    exit;
  end;
  if (Bank.LastOperationBlock.block<=0) then begin
    WhyNot := 'No blockchain';
    exit;
  end;
  unixtimediff := UnivDateTimeToUnix(DateTime2UnivDateTime(Now)) - Bank.LastOperationBlock.timestamp;
  If (unixtimediff<0) then begin
    WhyNot := 'Invalid Last Block Time';
    exit;
  end;
  if (unixtimediff>(CT_NewLineSecondsAvg*10)) then begin
    WhyNot := 'Last block has a long time ago... '+inttostr(unixtimediff);
    exit;
  end;
  Result := true;
end;

function TNode.IsReady(var CurrentProcess: String): Boolean;
var LLockedMempool : TPCOperationsComp;
begin
  Result := false;
  CurrentProcess := '';
  if FBank.IsReady(CurrentProcess) then begin
    if FNetServer.Active then begin
      if Not TNetData.NetData.IsGettingNewBlockChainFromClient(CurrentProcess) then begin
        LLockedMempool := LockMempoolRead;
        try
          if TNetData.NetData.MaxRemoteOperationBlock.block>LLockedMempool.OperationBlock.block then begin
            CurrentProcess := 'Found block '+inttostr(TNetData.NetData.MaxRemoteOperationBlock.block)+' (Wait until downloaded)';
          end else begin
            CurrentProcess := '';
            Result := true;
          end;
        finally
          UnlockMempoolRead;
        end;
      end;
    end else begin
      CurrentProcess := 'Server not active';
    end;
  end;
end;

function TNode.NetServer: TNetServer;
begin
  Result := FNetServer;
end;

class function TNode.Node: TNode;
begin
  if not assigned(_Node) then _Node := TNode.Create(Nil);
  Result := _Node;
end;

class function TNode.NodeExists: Boolean;
begin
  Result := Assigned(_Node);
end;

class function TNode.NodeVersion: String;
begin
  Result := CT_ClientAppVersion
    {$IFDEF USE_ABSTRACTMEM}+'am'{$ENDIF}
    {$IFDEF LINUX}+'L'{$ELSE}+'W'{$ENDIF}
    {$IFDEF FPC}{$IFDEF LCL}+'l'{$ELSE}+'f'{$ENDIF}{$ENDIF}
    {$IFDEF FPC}{$IFDEF CPU32}+'32b'{$ELSE}+'64b'{$ENDIF}{$ELSE}{$IFDEF CPU32BITS}+'32b'{$ELSE}+'64b'{$ENDIF}{$ENDIF}
    {$IFDEF Use_CryptoLib4Pascal}+'CL4P'{$ENDIF};
end;

procedure TNode.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
end;

procedure TNode.NotifyBlocksChanged;
Var i : Integer;
begin
  for i := 0 to FNotifyList.Count-1 do begin
    TNodeNotifyEvents( FNotifyList[i] ).NotifyBlocksChanged;
  end;
end;

function TNode.FindNOperation(block, account, n_operation: Cardinal;
  var OpResume: TOperationResume): TSearchOpHashResult;
  // Note: block = 0 search in all blocks. If Block>0 must match a valid block with operation with this account
var oprl : TOperationsResumeList;
begin
  oprl := TOperationsResumeList.Create;
  try
    Result := FindNOperations(account,block,block=0,n_operation,n_operation,oprl);
    If oprl.Count>0 then begin
      OpResume := oprl.Items[0];
    end else OpResume := CT_TOperationResume_NUL;
  finally
    oprl.Free;
  end;
end;

function TNode.FindNOperations(account, start_block : Cardinal; allow_search_previous : Boolean; n_operation_low, n_operation_high: Cardinal; OpResumeList: TOperationsResumeList): TSearchOpHashResult;
var i : Integer;
  op : TPCOperation;
  aux_block, block : Cardinal;
  OperationComp : TPCOperationsComp;
  opr : TOperationResume;
  n_operation, found_n_operation : Cardinal;
  LLockedMempool : TPCOperationsComp;
begin
  OpResumeList.Clear;
  Result := OpHash_invalid_params;
  block := start_block;
  If (block>=Bank.BlocksCount) then exit; // Invalid block number
  If (account>=Bank.AccountsCount) then exit; // Invalid account number
  If (n_operation_high<n_operation_low) then exit;
  LLockedMempool := LockMempoolRead;
  try
    n_operation := LLockedMempool.SafeBoxTransaction.Account(account).n_operation;
  finally
    UnlockMempoolRead;
  end;
  if (n_operation>n_operation_high) then n_operation := n_operation_high;
  if (n_operation<n_operation_low) then Exit;
  If (block=0) then begin
    // Start searching on pending blocks
    LLockedMempool := LockMempoolRead;
    try
      For i:=LLockedMempool.Count-1 downto 0 do begin
        op := LLockedMempool.Operation[i];
        If (op.IsSignerAccount(account)) then begin
          found_n_operation := op.GetAccountN_Operation(account);
          if (found_n_operation<n_operation_low) then Exit; // Not found
          If (found_n_operation<=n_operation) then begin
            TPCOperation.OperationToOperationResume(0,op,False,account,opr);
            opr.Balance:=-1;
            OpResumeList.Add(opr);
            if (n_operation>n_operation_low) then dec(n_operation)
            else begin
              Result := OpHash_found;
              Exit;
            end;
          end;
        end;
      end;
      block := Bank.SafeBox.Account(account).GetLastUpdatedBlock;
    finally
      UnlockMempoolRead;
    end;
  end;
  // Search in previous blocks
  OperationComp := TPCOperationsComp.Create(Nil);
  Try
    While (n_operation>0) And (n_operation>=n_operation_low) And (block>0) do begin
      aux_block := block;
      If Not Bank.LoadOperations(OperationComp,block) then begin
        Result := OpHash_block_not_found; // Cannot continue searching!
        exit;
      end;
      For i:=OperationComp.Count-1 downto 0 do begin
        op := OperationComp.Operation[i];
        if (op.IsSignerAccount(account)) then begin
          If (n_operation_high=n_operation_low) and (op.GetAccountN_Operation(account)=n_operation) // If searching only 1 n_operation, n_operation must match
            Or
            (n_operation_high>n_operation_low) and (op.GetAccountN_Operation(account)<=n_operation) and (op.GetAccountN_Operation(account)>=n_operation_low) and (op.GetAccountN_Operation(account)<=n_operation_high) then begin
            TPCOperation.OperationToOperationResume(block,op,True,account,opr);
            opr.time:=Bank.SafeBox.GetBlockInfo(block).timestamp;
            opr.NOpInsideBlock:=i;
            opr.Balance:=-1;
            OpResumeList.Add(opr);
            if (n_operation>n_operation_low) then dec(n_operation)
            else begin
              Result := OpHash_found;
              Exit;
            end;
          end else begin
            If (op.GetAccountN_Operation(account) < n_operation) then begin
              If (n_operation_high>n_operation_low) then Result := OpHash_found; // multiple search, result is found (not an error)
              Exit // First occurrence is lower
            end;
          end;
        end;
      end;
      block := OperationComp.PreviousUpdatedBlocks.GetPreviousUpdatedBlock(account,block);
      if (block>aux_block) then exit // Error... not found a valid block positioning
      else if (block=aux_block) then begin
        if ((start_block=0) Or (allow_search_previous)) then dec(block) // downgrade until found a block with operations
        else Exit; // Not found in current block
      end else if (start_block>0) and (not allow_search_previous) and (OpResumeList.Count=0) then Exit; // does not need to decrease
    end;
  finally
    OperationComp.Free;
  end;
  Result := OpHash_found;
end;

procedure TNode.InitSafeboxAndOperations(max_block_to_read : Cardinal = $FFFFFFFF; restoreProgressNotify : TProgressNotify = Nil);
var opht : TOperationsHashTree;
  oprl : TOperationsResumeList;
  errors : String;
  n : Integer;
begin
  Bank.DiskRestoreFromOperations(max_block_to_read,restoreProgressNotify);
  opht := TOperationsHashTree.Create;
  oprl := TOperationsResumeList.Create;
  try
    Bank.Storage.LoadPendingBufferOperations(opht); // New Build 2.1.4 to load pending operations buffer
    n := AddOperations(Nil,opht,oprl,errors);
    TLog.NewLog(ltInfo,ClassName,Format('Pending buffer restored operations:%d added:%d final_operations:%d errors:%s',[opht.OperationsCount,n,MempoolOperationsCount,errors]));
  finally
    opht.Free;
    oprl.Free;
  end;
end;

function TNode.FindOperation(Const AOperationHash : TRawBytes; var AOperationResume : TOperationResume) : TSearchOpHashResult;
{ With a OperationHash, search it }
var
  i : Integer;
  op : TPCOperation;
  opHashValid : TRawBytes;
  md160 : TRawBytes;
  LLockedMempool : TPCOperationsComp;
  LBlock, LAccount, LN_Operation : Cardinal;
begin
  Result := OpHash_invalid_params;
  // Decode OperationHash
  If not TPCOperation.DecodeOperationHash(AOperationHash,LBlock,LAccount,LN_Operation,md160) then exit;
  //
  If (LAccount>=Bank.AccountsCount) then exit; // Invalid account number
  // If block=0 then we must search in pending operations first
  if (LBlock=0) then begin
    LLockedMempool := LockMempoolRead;
    Try
      LLockedMempool.Lock;
      Try
        For i:=0 to LLockedMempool.Count-1 do begin
          op := LLockedMempool.Operation[i];
          If (op.SignerAccount=LAccount) then begin
            opHashValid := TPCOperation.OperationHashValid(op,0);
            If TBaseType.Equals(opHashValid,AOperationHash) then begin
              TPCOperation.OperationToOperationResume(0,op,True,LAccount,AOperationResume);
              AOperationResume.Balance := -1;
              AOperationResume.NOpInsideBlock := i;
              Result := OpHash_found;
              exit;
            end;
          end;
        end;
      finally
        LLockedMempool.Unlock;
      end;
    Finally
      UnlockMempoolRead;
    End;
  end;
  Result := Bank.Storage.FindOperation(AOperationHash,AOperationResume);
end;

procedure TNode.NotifyNetClientMessage(Sender: TNetConnection; const TheMessage: String);
Var i : Integer;
begin
  for i := 0 to FNotifyList.Count-1 do begin
    if Assigned( TNodeNotifyEvents( FNotifyList[i] ).OnNodeMessageEvent) then begin
      TNodeNotifyEvents( FNotifyList[i] ).FMessages.AddObject(TheMessage,Sender);
    end;
  end;
end;

function TNode.MempoolOperationsCount: Integer;
var LLockedMempool : TPCOperationsComp;
begin
  LLockedMempool := LockMempoolRead;
  try
    Result := LLockedMempool.Count;
  finally
    UnlockMempoolRead;
  end;
end;

function TNode.GetAccountsAvailableByPublicKey(const APubKey: TAccountKey;
  out AOnSafebox, AOnMempool: Integer): Integer;
var LPubKeys: TList<TAccountKey>;
begin
  LPubKeys := TList<TAccountKey>.Create;
  Try
    LPubKeys.Add(APubKey);
    Result := GetAccountsAvailableByPublicKey(LPubKeys,AOnSafebox,AOnMempool);
  Finally
    LPubKeys.Free;
  End;
end;

function TNode.GetAccountsAvailableByPublicKey(
  const APubKeys: TList<TAccountKey>; out AOnSafebox,
  AOnMempool: Integer): Integer;
var Lmempool : TPCOperationsComp;
  i,j,k : Integer;
  Lop : TPCOperation;
  LopResume : TOperationResume;
  Lpubkeys : TSafeboxPubKeysAndAccounts;
  Laccounts : TAccountsNumbersList;
begin
  AOnMempool := 0;
  AOnSafebox := 0;
  // Check safebox
  Lpubkeys := Bank.SafeBox.OrderedAccountKeysList;
  if Assigned(Lpubkeys) then begin
    for i := 0 to APubKeys.Count-1 do begin
      Laccounts := Lpubkeys.GetAccountsUsingThisKey(APubKeys[i]);
      if Assigned(Laccounts) then begin
        Inc(AOnSafebox,Laccounts.Count);
      end;
    end;
  end else AOnSafebox := -1;
  for i := 0 to APubKeys.Count-1 do begin
    // Check mempool
    Lmempool := LockMempoolRead;
    try
      for j := 0 to Lmempool.Count-1 do begin
        Lop := Lmempool.Operation[j];
        Lop.OperationToOperationResume(Bank.BlocksCount,Lop,True,Lop.SignerAccount,LopResume);
        for k:=0 to Length(LopResume.Changers)-1 do begin
          if (public_key in LopResume.Changers[k].Changes_type) and (LopResume.Changers[k].New_Accountkey.IsEqualTo(APubKeys[i])) then begin
            // New account is on the mempool!
            inc(AOnMempool);
          end;
        end;
      end;
    finally
      UnlockMempoolRead;
    end;
  end;
  if AOnSafebox>=0 then Result := (AOnMempool + AOnsafebox)
  else Result := AOnMempool;
end;

function TNode.GetMempoolAccount(AAccountNumber : Cardinal): TAccount;
var LLockedMempool : TPCOperationsComp;
begin
  LLockedMempool := LockMempoolRead;
  try
    Result := LLockedMempool.SafeBoxTransaction.Account(AAccountNumber);
  finally
    UnlockMempoolRead;
  end;
end;

class function TNode.GetPascalCoinDataFolder: String;
begin
  if (_PascalCoinDataFolder.Trim.Length>0) then begin
    Result := _PascalCoinDataFolder;
  end else begin
    Result := TFolderHelper.GetDataFolder(CT_PascalCoin_Data_Folder);
  end;
end;

class procedure TNode.SetPascalCoinDataFolder(const ANewDataFolder: String);
begin
  _PascalCoinDataFolder := ANewDataFolder;
end;

function TNode.LockMempoolRead: TPCOperationsComp;
begin
  FLockMempool.Acquire;
  Result := FMemPoolOperationsComp;
end;

procedure TNode.UnlockMempoolRead;
begin
  FLockMempool.Release;
end;

function TNode.LockMempoolWrite: TPCOperationsComp;
begin
  // TODO: Must lock WRITE EXCLUSIVE NO READ !!! XXXXXXXXXXXXXXXX
  FLockMempool.Acquire;
  Result := FMemPoolOperationsComp;
end;

procedure TNode.UnlockMempoolWrite;
begin
  FLockMempool.Release;
end;

procedure TNode.OnBankNewBlock(Sender: TObject);
var LLockedMempool : TPCOperationsComp;
begin
  LLockedMempool := LockMempoolWrite;
  try
    LLockedMempool.SanitizeOperations;
  finally
    UnlockMempoolWrite;
  end;
  NotifyBlocksChanged;
end;

function TNode.SendNodeMessage(Target: TNetConnection; const TheMessage: String; var errors: String): Boolean;
Var i,j : Integer;
  nc : TNetConnection;
  s : String;
begin
  Result := false;
  if Not TPCThread.TryProtectEnterCriticalSection(Self,4000,FLockMempool) then begin
    s := 'Cannot Send node message due blocking lock operations node';
    TLog.NewLog(lterror,Classname,s);
    if TThread.CurrentThread.ThreadID=MainThreadID then raise Exception.Create(s) else exit;
  end;
  try
    errors := '';
    if assigned(Target) then begin
      Target.Send_Message(TheMessage);
    end else begin
      j := TNetData.NetData.ConnectionsCountAll;
      for i:=0 to j-1 do begin
        if TNetData.NetData.GetConnection(i,nc) then begin
          If TNetData.NetData.ConnectionLock(Self,nc,500) then begin
            try
              nc.Send_Message(TheMessage);
            finally
              TNetData.NetData.ConnectionUnlock(nc)
            end;
          end;
        end;
      end;
    end;
    result := true;
  finally
    FLockMempool.Release;
  end;
end;

procedure TNode.SetNodeLogFilename(const Value: String);
begin
  FNodeLog.FileName := Value;
end;

{ TNodeMessageManyEventHelper }

procedure TNodeMessageManyEventHelper.Add(listener : TNodeMessageEvent);
begin
  if TArrayTool<TNodeMessageEvent>.IndexOf(self, listener) = -1 then begin
    TArrayTool<TNodeMessageEvent>.Add(self, listener);
  end;
end;

procedure TNodeMessageManyEventHelper.Remove(listener : TNodeMessageEvent);
begin
  TArrayTool<TNodeMessageEvent>.Remove(self, listener);
end;

procedure TNodeMessageManyEventHelper.Invoke(NetConnection : TNetConnection; MessageData : String);
var i : Integer;
begin
  for i := low(self) to high(self) do
    self[i](NetConnection, MessageData);
end;

{ TNodeNotifyEvents }

constructor TNodeNotifyEvents.Create(AOwner: TComponent);
begin
  inherited;
  FOnOperationsChanged := Nil;
  FOnBlocksChanged := Nil;
  FOnNodeMessageEvent := Nil;
  FWatchKeys := Nil;
  FOnKeyActivity:=Nil;
  FMessages := TStringList.Create;
  FPendingNotificationsList := TPCThreadList<Pointer>.Create('TNodeNotifyEvents_PendingNotificationsList');
  FThreadSafeNodeNotifyEvent := TThreadSafeNodeNotifyEvent.Create(Self);
  Node := _Node;
end;

destructor TNodeNotifyEvents.Destroy;
begin
  if Assigned(FNode) then FNode.FNotifyList.Remove(Self);
  FThreadSafeNodeNotifyEvent.FNodeNotifyEvents := Nil;
  FThreadSafeNodeNotifyEvent.Terminate;
  FThreadSafeNodeNotifyEvent.WaitFor;
  FreeAndNil(FThreadSafeNodeNotifyEvent);
  FreeAndNil(FPendingNotificationsList);
  FreeAndNil(FMessages);
  inherited;
end;

procedure TNodeNotifyEvents.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation=opremove) then begin
    if AComponent=FNode then FNode := Nil;
  end;
end;

procedure TNodeNotifyEvents.NotifyBlocksChanged;
begin
  if Assigned(FThreadSafeNodeNotifyEvent) then FThreadSafeNodeNotifyEvent.FNotifyBlocksChanged := true;
end;

procedure TNodeNotifyEvents.NotifyOperationsChanged;
begin
  if Assigned(FThreadSafeNodeNotifyEvent) then FThreadSafeNodeNotifyEvent.FNotifyOperationsChanged := true;
end;

procedure TNodeNotifyEvents.SetWatchKeys(AValue: TOrderedAccountKeysList);
begin
  if FWatchKeys=AValue then Exit;
  FWatchKeys:=AValue;
end;

procedure TNodeNotifyEvents.SetNode(const Value: TNode);
begin
  if FNode=Value then exit;
  if Assigned(FNode) then begin
    FNode.RemoveFreeNotification(Self);
    FNode.FNotifyList.Remove(Self);
  end;
  FNode := Value;
  if Assigned(FNode) then begin
    FNode.FreeNotification(Self);
    FNode.FNotifyList.Add(Self);
  end;
end;

{ TThreadSafeNodeNotifyEvent }

procedure TThreadSafeNodeNotifyEvent.BCExecute;
begin
  while (not Terminated) AND (Assigned(FNodeNotifyEvents)) do begin
    if (FNotifyOperationsChanged) Or (FNotifyBlocksChanged) Or (FNodeNotifyEvents.FMessages.Count>0) then Synchronize(SynchronizedProcess);
    Sleep(100);
  end;
end;

constructor TThreadSafeNodeNotifyEvent.Create(ANodeNotifyEvents: TNodeNotifyEvents);
begin
  FNodeNotifyEvents := ANodeNotifyEvents;
  Inherited Create(False);
end;

procedure TThreadSafeNodeNotifyEvent.SynchronizedProcess;
Var i : Integer;
  can_alert_keys : Boolean;
begin
  Try
    If (Terminated) Or (Not Assigned(FNodeNotifyEvents)) then exit;
    can_alert_keys := False;
    if FNotifyBlocksChanged then begin
      FNotifyBlocksChanged := false;
      can_alert_keys := True;
      DebugStep:='Notify OnBlocksChanged';
      if Assigned(FNodeNotifyEvents) And (Assigned(FNodeNotifyEvents.FOnBlocksChanged)) then
        FNodeNotifyEvents.FOnBlocksChanged(FNodeNotifyEvents);
    end;
    if FNotifyOperationsChanged then begin
      FNotifyOperationsChanged := false;
      can_alert_keys := True;
      DebugStep:='Notify OnOperationsChanged';
      if Assigned(FNodeNotifyEvents) And (Assigned(FNodeNotifyEvents.FOnOperationsChanged)) then
        FNodeNotifyEvents.FOnOperationsChanged(FNodeNotifyEvents);
    end;
    if FNodeNotifyEvents.FMessages.Count>0 then begin
      DebugStep:='Notify OnNodeMessageEvent';
      if Assigned(FNodeNotifyEvents) And (Assigned(FNodeNotifyEvents.FOnNodeMessageEvent)) then begin
        for i := 0 to FNodeNotifyEvents.FMessages.Count - 1 do begin
          DebugStep:='Notify OnNodeMessageEvent '+inttostr(i+1)+'/'+inttostr(FNodeNotifyEvents.FMessages.Count);
          FNodeNotifyEvents.FOnNodeMessageEvent(TNetConnection(FNodeNotifyEvents.FMessages.Objects[i]),FNodeNotifyEvents.FMessages.Strings[i]);
        end;
      end;
      FNodeNotifyEvents.FMessages.Clear;
    end;
    if (can_alert_keys) And Assigned(FNodeNotifyEvents) And (Assigned(FNodeNotifyEvents.FWatchKeys)) then begin
      DebugStep:='Notify WatchKeys';
      If FNodeNotifyEvents.FWatchKeys.HasAccountKeyChanged then begin
        FNodeNotifyEvents.FWatchKeys.ClearAccountKeyChanges;
        if Assigned(FNodeNotifyEvents.FOnKeyActivity) then begin
          DebugStep:='Notify WatchKeys OnKeyActivity';
          FNodeNotifyEvents.FOnKeyActivity(FNodeNotifyEvents);
        end;
      end;
    end;
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,ClassName,'Exception inside a Synchronized process: '+E.ClassName+':'+E.Message+' Step:'+DebugStep);
    end;
  End;
end;

{ TThreadNodeNotifyNewBlock }

procedure TThreadNodeNotifyNewBlock.BCExecute;
begin
  DebugStep := 'Locking';
  if TNetData.NetData.ConnectionLock(Self,FNetConnection,5000) then begin
    try
      DebugStep := 'Checking connected';
      if Not FNetconnection.Connected then exit;
      {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,ClassName,'Sending new block found to '+FNetConnection.Client.ClientRemoteAddr);{$ENDIF}
      DebugStep := 'Sending';
      FNetConnection.Send_NewBlockFound(FNewBlockOperations);
      DebugStep := 'Checking connected again';
      if Not FNetConnection.Connected then exit;
      DebugStep := 'Need send opreations?';
      if FSanitizedOperationsHashTree.OperationsCount>0 then begin
        DebugStep := 'Sending '+inttostr(FSanitizedOperationsHashTree.OperationsCount)+' sanitized operations';
        TLog.NewLog(ltdebug,ClassName,'Sending '+inttostr(FSanitizedOperationsHashTree.OperationsCount)+' sanitized operations to '+FNetConnection.ClientRemoteAddr);
        TThreadNodeNotifyOperations.Create(FNetConnection,FSanitizedOperationsHashTree);
      end;
      DebugStep := 'Unlocking';
    finally
      TNetData.NetData.ConnectionUnlock(FNetConnection);
    end;
  end;
  DebugStep := 'Finalizing';
end;

constructor TThreadNodeNotifyNewBlock.Create(NetConnection: TNetConnection; MakeACopyOfNewBlockOperations: TPCOperationsComp; MakeACopyOfSanitizedOperationsHashTree : TOperationsHashTree);
begin
  FNetConnection := NetConnection;
  FSanitizedOperationsHashTree := TOperationsHashTree.Create;
  FSanitizedOperationsHashTree.CopyFromHashTree(MakeACopyOfSanitizedOperationsHashTree);
  FNewBlockOperations := TPCOperationsComp.Create(Nil);
  FNewBlockOperations.CopyFrom(MakeACopyOfNewBlockOperations);
  Inherited Create(True);
  FreeOnTerminate := true;
  Suspended:=False;
end;

destructor TThreadNodeNotifyNewBlock.Destroy;
begin
  FreeAndNil(FSanitizedOperationsHashTree);
  FreeAndNil(FNewBlockOperations);
  inherited;
end;

{ TThreadNodeNotifyOperations }

procedure TThreadNodeNotifyOperations.BCExecute;
begin
  Sleep(Random(3000)); // Delay 0..3 seconds to allow receive data and don't send if not necessary
  if TNetData.NetData.ConnectionLock(Self, FNetConnection, 5000) then begin
    try
      if Not FNetconnection.Connected then exit;
      FNetConnection.Send_AddOperations(Nil);
    finally
      TNetData.NetData.ConnectionUnlock(FNetConnection);
    end;
  end;
end;

constructor TThreadNodeNotifyOperations.Create(NetConnection: TNetConnection; MakeACopyOfOperationsHashTree: TOperationsHashTree);
begin
  FNetConnection := NetConnection;
  FNetConnection.AddOperationsToBufferForSend(MakeACopyOfOperationsHashTree);
  Inherited Create(True);
  FreeOnTerminate := true;
  Suspended:=False;
end;

destructor TThreadNodeNotifyOperations.Destroy;
begin
  inherited;
end;

{ TSaveMempoolOperationsThread }

procedure TSaveMempoolOperationsThread.BCExecute;
var i : Integer;
  LLocked : TPCOperationsComp;
begin
  FPendingToSave := false;
  repeat
    if FPendingToSave then begin
      LLocked := FNode.LockMempoolRead;
      try
        FPendingToSave := False;
        FNode.Bank.Storage.SavePendingBufferOperations(LLocked.OperationsHashTree);
      finally
        FNode.UnlockMempoolRead;
      end;
    end;
    // Wait 10 seconds prior to save updates on mempool
    i := 0;
    while (i<1000) and (Not Terminated) do begin
      Sleep(10);
      inc(i);
    end;
  until (false) or (Terminated);
end;

procedure TSaveMempoolOperationsThread.Touch;
begin
  FPendingToSave := True;
end;

initialization
  _Node := Nil;
  _PascalCoinDataFolder := '';
finalization
  FreeAndNil(_Node);
end.
