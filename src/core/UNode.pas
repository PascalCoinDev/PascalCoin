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
  UBlockChain, UNetProtocol, UAccounts, UCrypto, UEPasa, UThread, UConst, SyncObjs, ULog, UBaseTypes, UPCOrderedLists;

{$I ./../config.inc}

Type

  { TNode }

  TNodeNotifyEvents = Class;

  TNode = Class;

  // Pending account deltas are deliberately unavailable while a received
  // blockchain backlog is applied. Callers must retry after the typed status
  // reports availability; an old overlay must never be used with a newer tip.
  EPendingOperationsUnavailable = Class(Exception);
  ENodeStorageUnavailable = Class(Exception);

  TNodeStorageFailureStage = (sfsUnknown, sfsSafeboxFlush, sfsCheckpoint, sfsBlockStream, sfsPendingOperations, sfsRestore);
  TNodeStorageFailure = record
    HasFailure: Boolean;
    Stage: TNodeStorageFailureStage;
    FileName, ErrorClass, ErrorMessage: String;
  end;

  // A selected target survives gaps between batches. Already received work
  // remains synchronization even after transport shutdown; local pages resume
  // when that physical backlog drains, not merely when sockets disconnect.
  TNodeSynchronizationState = (nssOffline, nssDisconnected, nssIdle, nssSynchronizing, nssStorageError);
  TNodeSynchronizationStatus = record
    State: TNodeSynchronizationState;
    NetworkActive: Boolean;
    PeerCount: Integer;
    HasRemoteTarget: Boolean;
    LocalBlockCount, TargetBlockCount: Cardinal;
    // All physical work, including blocks already received before networking
    // stopped. Connectivity never discards or conceals that processing backlog.
    ActiveTransfers, PendingOperationsCount: Integer;
    HasMempoolSummary: Boolean;
    PendingOperationsAvailable: Boolean;
    CompactTarget, LastBlockTimestamp: Cardinal;
    ProtocolVersion: Word;
    Progress: TProgressInfo;
    StorageFailure: TNodeStorageFailure;
  end;
  TNodeSynchronizationNotify = procedure(Sender: TObject;
    const AStatus: TNodeSynchronizationStatus) of object;

  TSaveMempoolOperationsThread = Class(TPCThread)
  private
    FNode : TNode;
    FPendingToSave : LongInt;
    FWorkAvailable : TEvent;
    function TakeSaveRequest: Boolean;
    function HasSaveRequest: Boolean;
  protected
    procedure BCExecute; override;
  public
    constructor Create(ANode : TNode);
    destructor Destroy; override;
    procedure Touch;
    procedure Wake;
    procedure Stop;
  End;

  TNode = Class(TComponent)
  private
    FNodeLog : TLog;
    FLockMempool : TReadWriteLock;
    FOperationSequenceLock : TPCCriticalSection;
    FNotifyList : TList<TNodeNotifyEvents>;
    FNotifyLock: TPCCriticalSection;
    FSynchronizationLock: TPCCriticalSection;
    FSynchronizationStatus: TNodeSynchronizationStatus;
    FSynchronizationPeers: TDictionary<TObject, Cardinal>;
    FSynchronizationGeneration: UInt64;
    FSynchronizationScopes: TDictionary<TThreadID, TList<UInt64>>;
    FBank : TPCBank;
    FMemPoolOperationsComp : TPCOperationsComp;
    // Mutated under node WRITE plus the short status lock. Status readers may
    // inspect it under that short lock without touching the pending overlay.
    FMempoolNeedsReconciliation : Boolean;
    // Announced blocks defer their old bounded pending resend until the final
    // coherent overlay. GETBLOCKS imports never request a rebroadcast.
    FResendPendingOperations : Boolean;
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
    procedure RecalculateSynchronizationStatus;
    procedure RetireSynchronizationSession;
    function IsCurrentSynchronizationScope: Boolean;
    procedure NotifySynchronizationChanged;
    procedure CaptureSynchronizationMempoolSummary;
    procedure ReconcileMempoolIfReady;
    procedure WakeMempoolMaintenance;
    procedure RequireMempoolAvailable;
    function LockMempoolOperationsWrite : TPCOperationsComp;
    procedure CollectPendingResends(AOperations : TOperationsHashTree);
    procedure BroadcastPendingResends(AOperations : TOperationsHashTree);
  protected
    procedure DoFlushSafebox; virtual;
    function DoFlushBlockStorage: Boolean; virtual;
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
    // Readers may overlap; all changes to shared pending state require write ownership.
    function LockMempoolRead : TPCOperationsComp;
    function TryLockMempoolRead(MaxWaitMilliseconds : Cardinal; out AMempool : TPCOperationsComp) : Boolean;
    // Core transport reconstruction may read immutable operation/index data
    // while the account overlay is unavailable. Never use SafeBoxTransaction
    // through this lease. Release it with the ordinary UnlockMempoolRead.
    function TryLockMempoolOperationsRead(MaxWaitMilliseconds : Cardinal;
      out AMempool : TPCOperationsComp) : Boolean;
    procedure UnlockMempoolRead;
    function PendingOperationsAvailable : Boolean;
    // Reserve the sole potential writer before inspecting a conditional mutation.
    // Enter/exit write separately inside this scope; never promote an ordinary read.
    function LockMempoolUpgradeableRead : TPCOperationsComp;
    procedure UnlockMempoolUpgradeableRead;
    function LockMempoolWrite : TPCOperationsComp;
    procedure UnlockMempoolWrite;
    //
    Function AddNewBlockChain(SenderConnection : TNetConnection; NewBlockOperations: TPCOperationsComp; var errors: String): Boolean;
    // Both announced blocks and catch-up batches use the existing block audit
    // format. The caller retains the header because bank acceptance clears it.
    procedure LogBlockChainResult(SenderConnection : TNetConnection;
      const AOperationBlock : TOperationBlock; AAccepted : Boolean; const AErrors : String);
    procedure LogBlockChainBatchReceived(SenderConnection : TNetConnection;
      ACount, AFirstBlock, ALastBlock : Cardinal; APayloadBytes : Int64);
    Function AddOperations(SenderConnection : TNetConnection; AOperationsHashTreeToAdd : TOperationsHashTree; OperationsResult : TOperationsResumeList; var errors: String; AProgressNotify : TProgressNotify = Nil): Integer;
    Function AddOperation(SenderConnection : TNetConnection; Operation : TPCOperation; var errors: String): Boolean;
    Function SendNodeMessage(Target : TNetConnection; const TheMessage : String; var errors : String) : Boolean;
    //
    Procedure NotifyBlocksChanged;
    // Balance these calls on the same thread in LIFO try/finally scopes around
    // decoding, validation and application. Targets survive inter-batch gaps.
    procedure BeginBlockChainUpdate;
    procedure EndBlockChainUpdate;
    function IsUpdatingBlockChain: Boolean;
    // A retired network session cannot request further data. Its already
    // received blocks still finish and remain visible as synchronization work.
    function CanRequestBlockChainData: Boolean;
    // These setters publish facts only; listeners run on the existing notifier
    // thread/main-thread bridge, never on the network producer's call stack.
    function GetSynchronizationStatus: TNodeSynchronizationStatus;
    procedure ReportStorageFailure(AStage: TNodeStorageFailureStage; const AFileName: String; AError: Exception);
    function HasStorageFailure: Boolean;
    // Persist already accepted work; failures pause admission until restart.
    procedure FlushSafebox;
    procedure FlushBlockStorage;
    procedure SetSynchronizationNetworkActive(AActive: Boolean);
    procedure SetSynchronizationPeerConnected(APeer: TObject; AConnected: Boolean);
    procedure SetSynchronizationTarget(APeer: TObject; ATargetBlockCount: Cardinal);
    procedure UpdateSynchronizationPeerTarget(APeer: TObject; ATargetBlockCount: Cardinal);
    procedure SetSynchronizationProgress(const AProgress: TProgressInfo);
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
    FNotifySynchronizationChanged: LongInt;
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
    FOnSynchronizationChanged: TNodeSynchronizationNotify;
    FMessages : TStringList;
    FOnNodeMessageEvent: TNodeMessageEvent;
    FWatchKeys: TOrderedAccountKeysList;
    procedure SetNode(const Value: TNode);
    Procedure NotifyBlocksChanged;
    Procedure NotifyOperationsChanged;
    procedure NotifySynchronizationChanged;
    procedure SetWatchKeys(AValue: TOrderedAccountKeysList);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Property Node : TNode read FNode write SetNode;
    Property OnBlocksChanged : TNotifyEvent read FOnBlocksChanged write FOnBlocksChanged;
    Property OnOperationsChanged : TNotifyEvent read FOnOperationsChanged write FOnOperationsChanged;
    property OnSynchronizationChanged: TNodeSynchronizationNotify
      read FOnSynchronizationChanged write FOnSynchronizationChanged;
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
    FLeaseOwner : TNetData;
  protected
    procedure BCExecute; override;
  public
    Constructor Create(NetConnection : TNetConnection; MakeACopyOfOperationsHashTree : TOperationsHashTree;
      ALeaseOwner : TNetData = Nil);
    destructor Destroy; override;
  End;

implementation

Uses UOpTransaction, UTime, UCommon, UPCOperationsSignatureValidator,
  UFolderHelper, USettings, UFileMem, UAbstractMem,
  {$IFDEF USE_ABSTRACTMEM}UAbstractMemBlockchainStorage,{$ENDIF} UPCAbstractMem;

var _Node : TNode;
  _PascalCoinDataFolder : String;

{ TNode }

function TNode.AddNewBlockChain(SenderConnection: TNetConnection; NewBlockOperations: TPCOperationsComp; var errors: String): Boolean;
Var i,j : Integer;
  nc : TNetConnection;
  s,sClientRemoteAddr : String;
  OpBlock : TOperationBlock;
  opsht : TOperationsHashTree;
begin
  Result := false;
  errors := '';
  if HasStorageFailure then begin
    errors := GetSynchronizationStatus.StorageFailure.ErrorMessage;
    Exit;
  end;
  if Assigned(SenderConnection) then sClientRemoteAddr := SenderConnection.ClientRemoteAddr
  else sClientRemoteAddr:='(SELF)';
  if FDisabledsNewBlocksCount>0 then begin
    TLog.NewLog(lterror,Classname,Format('Cannot Add new BlockChain due is adding disabled - Connection:%s NewBlock:%s',[
    sClientRemoteAddr,TPCOperationsComp.OperationBlockToText(NewBlockOperations.OperationBlock)]));
    errors := 'Adding blocks is disabled';
    exit;
  end;
  BeginBlockChainUpdate;
  try
  NewBlockOperations.Lock; // New protection
  Try
    If NewBlockOperations.OperationBlock.block<>Bank.BlocksCount then begin
      errors := 'New block number ('+IntToStr(NewBlockOperations.OperationBlock.block)+') not valid! (Expected '+IntToStr(Bank.BlocksCount)+')';
      exit;
    end;
    OpBlock := NewBlockOperations.OperationBlock;
    TLog.NewLog(ltdebug,Classname,Format('Starting AddNewBlockChain %d Operations %d from %s NewBlock:%s',[
      OpBlock.block,NewBlockOperations.Count,sClientRemoteAddr,TPCOperationsComp.OperationBlockToText(OpBlock)]));
    If Not TryLockNode(5000) then begin
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
        If Not FBank.IsRestoringFromFile then begin
          FlushSafebox;
        end;
        {$ENDIF}
      end;
      LogBlockChainResult(SenderConnection,OpBlock,Result,errors);
      if Result then begin
        opsht := TOperationsHashTree.Create;
        Try
          // Reconciliation will expose the accepted pending sequence again.
          // Until then, announce only the fully validated confirmed block.
          if PendingOperationsAvailable then CollectPendingResends(opsht)
          else if FMemPoolOperationsComp.Count>0 then FResendPendingOperations := True;
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
      UnlockNode;
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
  finally
    EndBlockChainUpdate;
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

function TNode.AddOperations(SenderConnection : TNetConnection; AOperationsHashTreeToAdd : TOperationsHashTree; OperationsResult : TOperationsResumeList; var errors: String; AProgressNotify : TProgressNotify): Integer;
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
  LLastProgress : TTickCount;
  LReservationIndexes : TList<Integer>;
  LCompletedApplying : Boolean;

  procedure NotifyProgress(AStage : TProgressStage; ACurrent, ATotal : Integer; AForce : Boolean);
  begin
    if Assigned(AProgressNotify) and (AForce or
      (TPlatform.GetElapsedMilliseconds(LLastProgress) >= 200)) then begin
      LLastProgress := TPlatform.GetTickCount;
      // The consumer owns display text; preserve synchronous cancellation here.
      AProgressNotify(Self,TProgressInfo.Create(AStage,ACurrent,ATotal));
    end;
  end;
begin
  Result := -1; // -1 Means Node is blocked or disabled
  if Assigned(OperationsResult) then OperationsResult.Clear;
  if HasStorageFailure then begin
    errors := GetSynchronizationStatus.StorageFailure.ErrorMessage;
    Exit;
  end;
  if not PendingOperationsAvailable then begin
    errors := 'Pending operations are unavailable during blockchain synchronization';
    Exit;
  end;
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
  LLastProgress := LTickCount;
  LCompletedApplying := False;
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
      NotifyProgress(psPreparingOperations,0,AOperationsHashTreeToAdd.OperationsCount,True);
      // Preserve the original batch lock for ordinary callers. A restoring
      // caller reports progress between atomic admissions, outside this lock.
      if not Assigned(AProgressNotify) then LLockedMempool := LockMempoolOperationsWrite;
      try
        for i := 0 to AOperationsHashTreeToAdd.OperationsCount-1 do begin
          if Assigned(AProgressNotify) then LLockedMempool := LockMempoolOperationsWrite;
          try
            ActOp := AOperationsHashTreeToAdd.GetOperation(i);
            j := FMemPoolAddingOperationsList.IndexOf( ActOp.Sha256 );
            if (j<0) then begin
              LOpsToAdd.Add(ActOp);
              FMemPoolAddingOperationsList.Add(ActOp.Sha256);
            end;
          finally
            if Assigned(AProgressNotify) then UnlockMempoolWrite;
          end;
          NotifyProgress(psPreparingOperations,i+1,AOperationsHashTreeToAdd.OperationsCount,False);
        end;
      finally
        if not Assigned(AProgressNotify) then UnlockMempoolWrite;
      end;
      NotifyProgress(psPreparingOperations,AOperationsHashTreeToAdd.OperationsCount,AOperationsHashTreeToAdd.OperationsCount,True);

      // Step 2:
      NotifyProgress(psValidatingSignatures,0,LOpsToAdd.Count,True);
      LTempSafeboxTransaction := TPCSafeBoxTransaction.Create(Bank.SafeBox);
      try
        TPCOperationsSignatureValidator.MultiThreadPreValidateSignatures(LTempSafeboxTransaction,LOpsToAdd,AProgressNotify);
      finally
        LTempSafeboxTransaction.Free;
      end;
      NotifyProgress(psValidatingSignatures,LOpsToAdd.Count,LOpsToAdd.Count,True);

      {$IFDEF BufferOfFutureOperations}
      LLockedMempool := LockMempoolOperationsWrite;
      try
        if PendingOperationsAvailable then
          Process_BufferOfFutureOperations(LLockedMempool,LValids_operations);
      finally
        UnlockMempoolWrite;
      end;
      {$ENDIF}

      // Step 3:
      NotifyProgress(psApplyingOperations,0,LOpsToAdd.Count,True);
      for j := 0 to LOpsToAdd.Count-1 do begin
        ActOp := LOpsToAdd[j];
        LLockedMempool := LockMempoolOperationsWrite;
        try

          // A block may arrive after this batch reserved hashes or verified
          // signatures. Never execute against an overlay dirtied meanwhile.
          if not PendingOperationsAvailable then begin
            errors := 'Pending operations are unavailable during blockchain synchronization';
            if Result=0 then Result := -1;
            Break;
          end;

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
        NotifyProgress(psApplyingOperations,j+1,LOpsToAdd.Count,False);
      end; // for i
      NotifyProgress(psApplyingOperations,LOpsToAdd.Count,LOpsToAdd.Count,True);
      // A progress callback can cancel here; the finally block must still
      // release this batch's reservations before the loader can finish.
      NotifyProgress(psFinalizingOperations,0,LOpsToAdd.Count,True);
      LCompletedApplying := True;
      If Result<>0 then begin
        LTickCount := TPlatform.GetElapsedMilliseconds(LTickCount);
        if LTickCount=0 then LTickCount:=1;
        if Assigned(SenderConnection) then begin
          s := SenderConnection.ClientRemoteAddr;
        end else s := '(SELF)';
        {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,Classname,Format('Finalizing AddOperations from %s Operations:%d of %d valids:%d spam:%d invalids:%d repeated:%d Miliseconds:%d %.1f ops/sec',
          [s,LOpsToAdd.Count,AOperationsHashTreeToAdd.OperationsCount,Result,nSpam,nError,nRepeated,LTickCount,LOpsToAdd.Count * 1000 / LTickCount]));{$ENDIF}
        if FBroadcastData and PendingOperationsAvailable then begin
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
      try
        LReservationIndexes := TList<Integer>.Create;
        try
          LReservationIndexes.Capacity := LOpsToAdd.Count;
          LLockedMempool := LockMempoolOperationsWrite;
          try
            // Other callers may have reserved interleaved hashes. Resolve all
            // indexes under this lock, then compact only this batch in one pass.
            for i := 0 to LOpsToAdd.Count-1 do begin
              j := FMemPoolAddingOperationsList.IndexOf(LOpsToAdd[i].Sha256);
              if j>=0 then LReservationIndexes.Add(j);
            end;
            LReservationIndexes.Sort;
            FMemPoolAddingOperationsList.DeleteIndexes(LReservationIndexes.ToArray);
          finally
            UnlockMempoolWrite;
          end;
        finally
          LReservationIndexes.Free;
        end;
      finally
        LOpsToAdd.Free;
      end;
    end;
  finally
    try
      LValids_operations.Free;
    finally
      // Cleanup precedes saving, including when later broadcasting fails.
      // Cancellation before completed application never saves a partial restore.
      if LCompletedApplying and (Result<>0) then FSaveMempoolOperationsThread.Touch;
    end;
  end;
  // Notify it!
  FNotifyLock.Acquire;
  try
    for i := 0 to FNotifyList.Count-1 do
      FNotifyList[i].NotifyOperationsChanged;
  finally
    FNotifyLock.Release;
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
  // Block application reenters through bank callbacks and signature reuse.
  FLockMempool := TReadWriteLock.Create(lrpSupportsRecursion);
  FOperationSequenceLock := TPCCriticalSection.Create('TNode_OperationSequenceLock');
  FNotifyLock := TPCCriticalSection.Create('TNode_NotifyLock');
  FSynchronizationLock := TPCCriticalSection.Create('TNode_SynchronizationLock');
  FSynchronizationPeers := TDictionary<TObject, Cardinal>.Create;
  FSynchronizationScopes := TDictionary<TThreadID, TList<UInt64>>.Create;
  FSynchronizationGeneration := 1;
  FSynchronizationStatus := Default(TNodeSynchronizationStatus);
  // TNetData starts enabled; its explicit setter subsequently publishes offline.
  FSynchronizationStatus.NetworkActive := True;
  FSynchronizationStatus.State := nssDisconnected;
  FSynchronizationStatus.PendingOperationsAvailable := True;
  FNotifyList := TList<TNodeNotifyEvents>.Create;
  FBank := TPCBank.Create(Self);
  FBCBankNotify := TPCBankNotify.Create(Self);
  FBCBankNotify.Bank := FBank;
  FBCBankNotify.OnNewBlock := OnBankNewBlock;
  FNetServer := TNetServer.Create;
  FMemPoolOperationsComp := TPCOperationsComp.Create(Nil);
  FMemPoolOperationsComp.bank := FBank;
  FMemPoolAddingOperationsList := TOrderedRawList.Create;
  {$IFDEF BufferOfFutureOperations}
  FBufferAuxWaitingOperations := TOperationsHashTree.Create;
  {$ENDIF}
  FBroadcastData := True;
  FUpdateBlockchain := True;
  FSaveMempoolOperationsThread := TSaveMempoolOperationsThread.Create(Self);
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
  LScopes: TList<UInt64>;
  {$IFDEF USE_ABSTRACTMEM}
  I: Integer;
  LStorage: TAbstractMemBlockchainStorage;
  LStorageFileName: String;
  {$ENDIF}
begin
  TLog.NewLog(ltInfo,ClassName,'TNode.Destroy START');
  Try
    // Shared network producers are stopped by their caller. The node's own
    // accepted-client callbacks must also finish before their template/locks
    // disappear, and before the saver captures the final dirty operation batch.
    step := 'Desactivating server';
    FNetServer.Active := false;

    step := 'Destroying NetServer';
    FreeAndNil(FNetServer);

    step := 'Deleting SaveMempoolOperationsThread';
    FSaveMempoolOperationsThread.Stop;
    FSaveMempoolOperationsThread.WaitFor;
    FreeAndNil(FSaveMempoolOperationsThread);

    {$IFDEF USE_ABSTRACTMEM}
    step := 'Finalizing blockchain storage workers';
    // A draining index writer can still report its first persistence failure.
    // Keep all node/listener locks alive until those owned workers have joined.
    // Inspect existing components; the lazy Storage getter could create a backend.
    if Assigned(FBank) then
      for I := FBank.ComponentCount-1 downto 0 do
        if FBank.Components[I] is TAbstractMemBlockchainStorage then begin
          LStorage := TAbstractMemBlockchainStorage(FBank.Components[I]);
          LStorageFileName := 'blockchain storage';
          if Assigned(LStorage.FileMem) then LStorageFileName := LStorage.FileMem.FileName;
          try
            LStorage.UseMultithread := False;
          except
            on E: Exception do begin
              // Known write failures stay latched while later ownership cleanup
              // proceeds. Cancellation, exhaustion and programmer faults propagate.
              if (E is EStreamError) or (E is EOSError) or (E is EFileMem) or
                (E is EAbstractMem) or (E is EPCAbstractMem) or
                (E is EAbstractMemBlockchainStorage) then
                ReportStorageFailure(sfsBlockStream,LStorageFileName,E)
              else raise;
            end;
          end;
        end;
    {$ENDIF}

    step := 'Deleting critical section';
    FreeAndNil(FLockMempool);
    FreeAndNil(FOperationSequenceLock);

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
    FreeAndNil(FSynchronizationPeers);
    for LScopes in FSynchronizationScopes.Values do LScopes.Free;
    FreeAndNil(FSynchronizationScopes);
    FreeAndNil(FSynchronizationLock);
    FreeAndNil(FNotifyLock);
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
  // Preserve the legacy exclusive node gate and its maximum wait contract.
  if MaxWaitMilliseconds > 60000 then MaxWaitMilliseconds := 60000;
  Result := FLockMempool.TryAcquireWriteLock(Integer(MaxWaitMilliseconds));
end;

procedure TNode.UnlockNode;
begin
  try
    if FLockMempool.RecursiveWriteCount=1 then CaptureSynchronizationMempoolSummary;
  finally
    FLockMempool.ReleaseWriteLock;
  end;
end;

procedure TNode.MarkVerifiedECDSASignaturesFromMemPool(newOperationsToValidate: TPCOperationsComp);
var LLockedMempool : TPCOperationsComp;
begin
  // Introduced on Build 4.0.2 to increase speed using MEMPOOL verified operations instead of verify again everytime
  // Will check if "newOperationsToValidate" operations are on MEMPOOL. If found, will set same FHasValidSignature value in order to mark as verified
  // Hash trees can share operation instances; signature flags require write ownership.
  // This lease touches signature/index metadata only. Reusing a cached
  // signature must not force a replay or expose the deferred account overlay.
  FLockMempool.AcquireWriteLock;
  LLockedMempool := FMemPoolOperationsComp;
  try
    if newOperationsToValidate = LLockedMempool then Exit; // Is the same, do nothing
    // Signing digests change between protocols. A cached signature result from
    // the pending template is reusable only under the same block protocol.
    if newOperationsToValidate.OperationBlock.protocol_version <>
      LLockedMempool.OperationBlock.protocol_version then Exit;
    newOperationsToValidate.Lock;
    try
      LLockedMempool.OperationsHashTree.MarkVerifiedECDSASignatures(newOperationsToValidate.OperationsHashTree);
    finally
      newOperationsToValidate.Unlock;
    end;
  finally
    FLockMempool.ReleaseWriteLock;
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
  if HasStorageFailure then begin
    WhyNot := GetSynchronizationStatus.StorageFailure.ErrorMessage;
    Exit;
  end;
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
var LLockedMempool : TPCOperationsComp; LProgress : TProgressInfo;
begin
  Result := false;
  CurrentProcess := '';
  if HasStorageFailure then begin
    CurrentProcess := GetSynchronizationStatus.StorageFailure.ErrorMessage;
    Exit;
  end;
  if not PendingOperationsAvailable then begin
    CurrentProcess := 'Blockchain synchronization is active';
    Exit;
  end;
  if FBank.IsReady(CurrentProcess) then begin
    if FNetServer.Active then begin
      if Not TNetData.NetData.IsGettingNewBlockChainFromClient(LProgress) then begin
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
      end else
        // Readiness diagnostics are separate from the UI's progress presentation.
        CurrentProcess := 'Blockchain synchronization is active';
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

procedure TNode.BeginBlockChainUpdate;
var LScopes: TList<UInt64>;
  LGeneration: UInt64;
begin
  FSynchronizationLock.Acquire;
  try
    if not FSynchronizationScopes.TryGetValue(TThread.CurrentThread.ThreadID,LScopes) then begin
      LScopes := TList<UInt64>.Create;
      FSynchronizationScopes.Add(TThread.CurrentThread.ThreadID,LScopes);
    end;
    if LScopes.Count>0 then LGeneration := LScopes[LScopes.Count-1]
    else if FSynchronizationStatus.NetworkActive and (FSynchronizationPeers.Count>0) then
      LGeneration := FSynchronizationGeneration
    else LGeneration := 0;
    // Nested block application inherits its download's session. Reconnecting
    // must not turn the remainder of an old download into a new session.
    LScopes.Add(LGeneration);
    Inc(FSynchronizationStatus.ActiveTransfers);
    RecalculateSynchronizationStatus;
  finally
    FSynchronizationLock.Release;
  end;
  NotifySynchronizationChanged;
end;

procedure TNode.EndBlockChainUpdate;
var LScopes: TList<UInt64>;
begin
  FSynchronizationLock.Acquire;
  try
    if not FSynchronizationScopes.TryGetValue(TThread.CurrentThread.ThreadID,LScopes) or
      (LScopes.Count=0) then
      raise Exception.Create('Unbalanced blockchain update scope');
    LScopes.Delete(LScopes.Count-1);
    if LScopes.Count=0 then begin
      FSynchronizationScopes.Remove(TThread.CurrentThread.ThreadID);
      LScopes.Free;
    end;
    Dec(FSynchronizationStatus.ActiveTransfers);
    RecalculateSynchronizationStatus;
  finally
    FSynchronizationLock.Release;
  end;
  NotifySynchronizationChanged;
  // Completion is an event, not a UI timer or synchronous UI callback. The
  // existing maintenance owner reconciles only after the whole selected
  // backlog drains; gaps between its batches must not trigger full replay.
  WakeMempoolMaintenance;
  if PendingOperationsAvailable then NotifyBlocksChanged;
end;

procedure TNode.RetireSynchronizationSession;
begin
  // Retire only future network acquisition. Received blocks keep processing and
  // keep the UI synchronizing until their balanced physical scopes finish.
  Inc(FSynchronizationGeneration);
end;

function TNode.IsCurrentSynchronizationScope: Boolean;
var LScopes: TList<UInt64>;
begin
  // Caller owns the status lock. A fresh producer has no enclosing old scope.
  Result := not FSynchronizationScopes.TryGetValue(TThread.CurrentThread.ThreadID,LScopes);
  if not Result then
    Result := (LScopes.Count>0) and
      (LScopes[LScopes.Count-1]=FSynchronizationGeneration);
end;

function TNode.GetSynchronizationStatus: TNodeSynchronizationStatus;
begin
  // Only a coherent record copy. Never acquire mempool/Bank/SafeBox here.
  FSynchronizationLock.Acquire;
  try
    Result := FSynchronizationStatus;
  finally
    FSynchronizationLock.Release;
  end;
end;

procedure TNode.DoFlushSafebox;
begin
  {$IFDEF USE_ABSTRACTMEM}
  FBank.SafeBox.PCAbstractMem.FlushCache;
  {$ENDIF}
end;

function TNode.DoFlushBlockStorage: Boolean;
begin
  Result := True;
  {$IFDEF USE_ABSTRACTMEM}
  if (FBank.Storage is TAbstractMemBlockchainStorage) and
    (TAbstractMemBlockchainStorage(FBank.Storage).PendingToSave=0) then
    // The backend rechecks its private failure under the file lock. A node-wide
    // latch alone cannot prevent a competing flush from certifying partial indexes.
    Result := TAbstractMemBlockchainStorage(FBank.Storage).FlushCache;
  {$ENDIF}
end;

procedure TNode.FlushSafebox;
var LFileName: String;
begin
  LFileName := 'safebox';
  try
    {$IFDEF USE_ABSTRACTMEM}
    LFileName := FBank.SafeBox.PCAbstractMem.FileName;
    {$ENDIF}
    DoFlushSafebox;
  except
    on E: Exception do begin
      // Cache eviction/flush is persistence too, even after block validation
      // and operation storage have succeeded. Never reclassify cancellation.
      if (E is EStreamError) or (E is EOSError) or (E is EFileMem) or
        (E is EAbstractMem) or (E is EPCAbstractMem) then
        ReportStorageFailure(sfsSafeboxFlush,LFileName,E);
      raise;
    end;
  end;
end;

procedure TNode.FlushBlockStorage;
var LFileName: String;
begin
  LFileName := 'blockchain storage';
  try
    {$IFDEF USE_ABSTRACTMEM}
    if (FBank.Storage is TAbstractMemBlockchainStorage) and
      Assigned(TAbstractMemBlockchainStorage(FBank.Storage).FileMem) then
      LFileName := TAbstractMemBlockchainStorage(FBank.Storage).FileMem.FileName;
    {$ENDIF}
    if not DoFlushBlockStorage then
      raise EWriteError.Create('Cannot flush blockchain storage '+LFileName);
  except
    on E: Exception do begin
      if (E is EStreamError) or (E is EOSError) or (E is EFileMem) or
        (E is EAbstractMem) then ReportStorageFailure(sfsBlockStream,LFileName,E);
      raise;
    end;
  end;
end;

procedure TNode.ReportStorageFailure(AStage: TNodeStorageFailureStage; const AFileName: String; AError: Exception);
var LFailure: TNodeStorageFailure; LChanged: Boolean;
begin
  // Preserve the first persistence failure until this node is destroyed. Copy
  // exception data, never retain an exception object owned by the producer.
  if (AError=Nil) or (AError is EOutOfMemory) or (AError is EAbort) or (AError is EAccessViolation) then Exit;
  LFailure := Default(TNodeStorageFailure);
  LFailure.HasFailure := True;
  LFailure.Stage := AStage;
  LFailure.FileName := AFileName;
  LFailure.ErrorClass := AError.ClassName;
  LFailure.ErrorMessage := AError.Message;
  LChanged := False;
  FSynchronizationLock.Acquire;
  try
    if Not FSynchronizationStatus.StorageFailure.HasFailure then begin
      FSynchronizationStatus.StorageFailure := LFailure;
      RetireSynchronizationSession;
      RecalculateSynchronizationStatus;
      LChanged := True;
    end;
  finally
    FSynchronizationLock.Release;
  end;
  // This publishes existing notifier flags only. A bank/network producer never
  // calls GUI subscribers or acquires the node writer while reporting failure.
  if LChanged then NotifySynchronizationChanged;
end;

function TNode.HasStorageFailure: Boolean;
begin
  FSynchronizationLock.Acquire;
  try
    Result := FSynchronizationStatus.StorageFailure.HasFailure;
  finally
    FSynchronizationLock.Release;
  end;
end;

procedure TNode.RecalculateSynchronizationStatus;
var LTarget: Cardinal; LChainBusy: Boolean;
begin
  // Caller owns only the short status lock. No peer object is dereferenced.
  FSynchronizationStatus.PeerCount := FSynchronizationPeers.Count;
  FSynchronizationStatus.TargetBlockCount := 0;
  for LTarget in FSynchronizationPeers.Values do
    if LTarget>FSynchronizationStatus.TargetBlockCount then
      FSynchronizationStatus.TargetBlockCount := LTarget;
  FSynchronizationStatus.HasRemoteTarget := FSynchronizationStatus.TargetBlockCount>0;
  LChainBusy := (FSynchronizationStatus.ActiveTransfers>0) or
    (FSynchronizationStatus.NetworkActive and (FSynchronizationStatus.PeerCount>0) and
     (FSynchronizationStatus.TargetBlockCount>FSynchronizationStatus.LocalBlockCount));
  FSynchronizationStatus.PendingOperationsAvailable :=
    not FSynchronizationStatus.StorageFailure.HasFailure and
    not FMempoolNeedsReconciliation and not LChainBusy;
  if FSynchronizationStatus.StorageFailure.HasFailure then
    FSynchronizationStatus.State := nssStorageError
  else if LChainBusy or FMempoolNeedsReconciliation then
    FSynchronizationStatus.State := nssSynchronizing
  else if not FSynchronizationStatus.NetworkActive then
    FSynchronizationStatus.State := nssOffline
  else if FSynchronizationStatus.PeerCount=0 then
    FSynchronizationStatus.State := nssDisconnected
  else if FSynchronizationStatus.TargetBlockCount>FSynchronizationStatus.LocalBlockCount then
    FSynchronizationStatus.State := nssSynchronizing
  else
    FSynchronizationStatus.State := nssIdle;
  if FSynchronizationStatus.State<>nssSynchronizing then
    FSynchronizationStatus.Progress := Default(TProgressInfo);
end;

procedure TNode.NotifySynchronizationChanged;
var i: Integer;
begin
  // Producer work is bounded flag publication, not a call into a UI listener.
  FNotifyLock.Acquire;
  try
    for i := 0 to FNotifyList.Count-1 do
      FNotifyList[i].NotifySynchronizationChanged;
  finally
    FNotifyLock.Release;
  end;
end;

procedure TNode.SetSynchronizationNetworkActive(AActive: Boolean);
begin
  FSynchronizationLock.Acquire;
  try
    FSynchronizationStatus.NetworkActive := AActive;
    if not AActive then begin
      FSynchronizationPeers.Clear;
      RetireSynchronizationSession;
    end;
    RecalculateSynchronizationStatus;
  finally
    FSynchronizationLock.Release;
  end;
  NotifySynchronizationChanged;
  WakeMempoolMaintenance;
end;

procedure TNode.SetSynchronizationPeerConnected(APeer: TObject; AConnected: Boolean);
var LHadPeers: Boolean;
begin
  if not Assigned(APeer) then Exit;
  FSynchronizationLock.Acquire;
  try
    LHadPeers := FSynchronizationPeers.Count>0;
    if not AConnected then FSynchronizationPeers.Remove(APeer)
    else if FSynchronizationStatus.NetworkActive and
      not FSynchronizationPeers.ContainsKey(APeer) then
      FSynchronizationPeers.Add(APeer,0);
    if LHadPeers and (FSynchronizationPeers.Count=0) then
      RetireSynchronizationSession;
    RecalculateSynchronizationStatus;
  finally
    FSynchronizationLock.Release;
  end;
  NotifySynchronizationChanged;
  WakeMempoolMaintenance;
end;

procedure TNode.SetSynchronizationTarget(APeer: TObject; ATargetBlockCount: Cardinal);
begin
  FSynchronizationLock.Acquire;
  try
    // A selected target is meaningful only for a currently connected peer.
    // Reconnecting must not revive a discarded session's remote height.
    if IsCurrentSynchronizationScope and FSynchronizationStatus.NetworkActive and
      FSynchronizationPeers.ContainsKey(APeer) then
      FSynchronizationPeers[APeer] := ATargetBlockCount;
    RecalculateSynchronizationStatus;
  finally
    FSynchronizationLock.Release;
  end;
  NotifySynchronizationChanged;
  WakeMempoolMaintenance;
end;

procedure TNode.UpdateSynchronizationPeerTarget(APeer: TObject; ATargetBlockCount: Cardinal);
var LPrevious: Cardinal;
begin
  FSynchronizationLock.Acquire;
  try
    // HELLO/new-block advertisements update an already selected download peer;
    // arbitrary advertised heights do not select a synchronization target.
    if IsCurrentSynchronizationScope and FSynchronizationStatus.NetworkActive and
      FSynchronizationPeers.TryGetValue(APeer,LPrevious) and (LPrevious>0) then
      FSynchronizationPeers[APeer] := ATargetBlockCount;
    RecalculateSynchronizationStatus;
  finally
    FSynchronizationLock.Release;
  end;
  NotifySynchronizationChanged;
  WakeMempoolMaintenance;
end;

procedure TNode.SetSynchronizationProgress(const AProgress: TProgressInfo);
begin
  FSynchronizationLock.Acquire;
  try
    if FSynchronizationStatus.State=nssSynchronizing then
      FSynchronizationStatus.Progress := AProgress;
  finally
    FSynchronizationLock.Release;
  end;
  NotifySynchronizationChanged;
end;

procedure TNode.CaptureSynchronizationMempoolSummary;
var LBlock, LLastBlock: TOperationBlock;
  LCount: Integer;
  LChanged: Boolean;
begin
  // Caller owns a write scope. During catch-up this publishes confirmed facts
  // and the retained raw operation count, explicitly marked unavailable as a
  // pending overlay. It performs no account scan or disk access.
  LBlock := FMemPoolOperationsComp.OperationBlock;
  LLastBlock := FBank.LastOperationBlock;
  if FMempoolNeedsReconciliation then begin
    // The pending template intentionally still belongs to the old base. Basic
    // synchronization facts must instead describe the newly committed bank.
    LBlock.block := FBank.BlocksCount;
    LBlock.compact_target := LLastBlock.compact_target;
  end;
  LCount := FMemPoolOperationsComp.Count;
  FSynchronizationLock.Acquire;
  try
    LChanged := not FSynchronizationStatus.HasMempoolSummary or
      (FSynchronizationStatus.LocalBlockCount<>LBlock.block) or
      (FSynchronizationStatus.PendingOperationsCount<>LCount) or
      (FSynchronizationStatus.CompactTarget<>LBlock.compact_target) or
      (FSynchronizationStatus.LastBlockTimestamp<>LLastBlock.timestamp) or
      (FSynchronizationStatus.ProtocolVersion<>LLastBlock.protocol_version);
    FSynchronizationStatus.HasMempoolSummary := True;
    FSynchronizationStatus.LocalBlockCount := LBlock.block;
    FSynchronizationStatus.PendingOperationsCount := LCount;
    FSynchronizationStatus.CompactTarget := LBlock.compact_target;
    FSynchronizationStatus.LastBlockTimestamp := LLastBlock.timestamp;
    FSynchronizationStatus.ProtocolVersion := LLastBlock.protocol_version;
    RecalculateSynchronizationStatus;
  finally
    FSynchronizationLock.Release;
  end;
  if LChanged then NotifySynchronizationChanged;
end;

procedure TNode.WakeMempoolMaintenance;
begin
  if Assigned(FSaveMempoolOperationsThread) then
    FSaveMempoolOperationsThread.Wake;
end;

procedure TNode.ReconcileMempoolIfReady;
var LCanReconcile: Boolean; LPreviousHash: TRawBytes; I: Integer;
  LResends: TOperationsHashTree;
begin
  // This method runs on the existing owned maintenance thread. In particular,
  // OFF/disconnect between batches must not replay pending operations on the
  // main thread, and a selected target ahead must not replay after every batch.
  // Reject ordinary per-block wakeups before entering the writer queue. The
  // same predicate is checked again after admission to cover a new producer.
  FSynchronizationLock.Acquire;
  try
    LCanReconcile := not FSynchronizationStatus.StorageFailure.HasFailure and
      not FSynchronizationStatus.PendingOperationsAvailable and
      (FSynchronizationStatus.ActiveTransfers=0) and
      ((not FSynchronizationStatus.NetworkActive) or
       (FSynchronizationStatus.PeerCount=0) or
       (FSynchronizationStatus.TargetBlockCount<=FSynchronizationStatus.LocalBlockCount));
  finally
    FSynchronizationLock.Release;
  end;
  if not LCanReconcile then Exit;
  LResends := Nil;
  try
  FLockMempool.AcquireWriteLock;
  try
    if not FMempoolNeedsReconciliation then Exit;
    FSynchronizationLock.Acquire;
    try
      LCanReconcile := not FSynchronizationStatus.StorageFailure.HasFailure and
        (FSynchronizationStatus.ActiveTransfers=0) and
        ((not FSynchronizationStatus.NetworkActive) or
         (FSynchronizationStatus.PeerCount=0) or
         (FSynchronizationStatus.TargetBlockCount<=FSynchronizationStatus.LocalBlockCount));
    finally
      FSynchronizationLock.Release;
    end;
    if not LCanReconcile then Exit;
    LPreviousHash := FMemPoolOperationsComp.OperationsHashTree.HashTree;
    // Full original replay is required once: protocol/height-sensitive rules,
    // account seals and conflict removal must match the final confirmed base.
    FMemPoolOperationsComp.SanitizeOperations;
    if FResendPendingOperations then begin
      LResends := TOperationsHashTree.Create;
      CollectPendingResends(LResends);
      FResendPendingOperations := False;
    end;
    FSynchronizationLock.Acquire;
    try
      FMempoolNeedsReconciliation := False;
      RecalculateSynchronizationStatus;
    finally
      FSynchronizationLock.Release;
    end;
    if not TBaseType.Equals(LPreviousHash,FMemPoolOperationsComp.OperationsHashTree.HashTree) then
      FSaveMempoolOperationsThread.Touch;
    CaptureSynchronizationMempoolSummary;
  finally
    FLockMempool.ReleaseWriteLock;
  end;
  // Availability is published only after a complete coherent replay. A later
  // producer can dirty it again, in which case observers retain their queries.
  NotifySynchronizationChanged;
  NotifyBlocksChanged;
  FNotifyLock.Acquire;
  try
    for I := 0 to FNotifyList.Count-1 do
      FNotifyList[I].NotifyOperationsChanged;
  finally
    FNotifyLock.Release;
  end;
  if Assigned(LResends) then BroadcastPendingResends(LResends);
  finally
    LResends.Free;
  end;
end;

procedure TNode.CollectPendingResends(AOperations: TOperationsHashTree);
var I, J: Integer; LMinBlock: Cardinal; LOperation: TPCOperation;
begin
  // Caller owns node WRITE and a coherent pending overlay. Keep the original
  // bounded selection and counters, merely delayed until reconciliation.
  J := Random(5);
  if Bank.LastBlockFound.OperationBlock.block>J then
    LMinBlock := Bank.LastBlockFound.OperationBlock.block-J
  else LMinBlock := 1;
  I := 0;
  while (AOperations.OperationsCount<CT_MaxResendMemPoolOperations) and
    (I<FMemPoolOperationsComp.Count) do begin
    LOperation := FMemPoolOperationsComp.Operation[I];
    J := LOperation.ResendOnBlock;
    if (LOperation.ResendCount<2) and ((J<=0) or (J<=LMinBlock)) then begin
      AOperations.AddOperationToHashTree(LOperation);
      LOperation.ResendOnBlock := FMemPoolOperationsComp.OperationBlock.block;
      LOperation.ResendCount := LOperation.ResendCount+1;
    end;
    Inc(I);
  end;
  if AOperations.OperationsCount>0 then begin
    TLog.NewLog(ltinfo,ClassName,Format('Resending %d operations for new block (Mempool Pending Operations:%d)',[
      AOperations.OperationsCount,FMemPoolOperationsComp.Count]));
    {$IFDEF HIGHLOG}
    for I := 0 to AOperations.OperationsCount-1 do
      TLog.NewLog(ltinfo,ClassName,'Resending ('+IntToStr(I+1)+'/'+IntToStr(AOperations.OperationsCount)+'): '+AOperations.GetOperation(I).ToString);
    {$ENDIF}
  end;
end;

procedure TNode.BroadcastPendingResends(AOperations: TOperationsHashTree);
var LNet: TNetData; LConnections: TList<TNetConnection>; LConnection: TNetConnection;
  I: Integer;
begin
  if not FBroadcastData or (AOperations.OperationsCount=0) or
    not TNetData.NetDataExists then Exit;
  LNet := TNetData.NetData;
  LConnections := TList<TNetConnection>.Create;
  try
    // These existing lifetime leases also keep their NetData owner alive
    // while shutdown waits for every leased connection to finish retiring.
    LNet.AcquireDownloadConnections(LConnections);
    for I := 0 to LConnections.Count-1 do begin
      LConnection := LConnections[I];
      if LNet.ConnectionLock(Self,LConnection,0) then begin
        try
          if LConnection.RemoteOperationBlock.block>0 then begin
            // Transfer the lease before construction: constructor failure also
            // destroys the notifier and releases its transferred lease.
            LConnections[I] := Nil;
            TThreadNodeNotifyOperations.Create(LConnection,AOperations,LNet);
          end;
        finally LNet.ConnectionUnlock(LConnection); end;
      end;
    end;
  finally
    for I := 0 to LConnections.Count-1 do
      if Assigned(LConnections[I]) then LNet.ReleaseDownloadConnection(LConnections[I]);
    LConnections.Free;
  end;
end;

function TNode.IsUpdatingBlockChain: Boolean;
var LStatus: TNodeSynchronizationStatus;
begin
  // Physical scopes have one authoritative count. UI refresh admission must
  // not add a second activity clock or infer work from elapsed notification time.
  LStatus := GetSynchronizationStatus;
  Result := LStatus.ActiveTransfers>0;
end;

function TNode.CanRequestBlockChainData: Boolean;
begin
  FSynchronizationLock.Acquire;
  try
    Result := not FSynchronizationStatus.StorageFailure.HasFailure and
      FSynchronizationStatus.NetworkActive and IsCurrentSynchronizationScope;
  finally
    FSynchronizationLock.Release;
  end;
end;

procedure TNode.NotifyBlocksChanged;
Var i : Integer;
begin
  FNotifyLock.Acquire;
  try
    for i := 0 to FNotifyList.Count-1 do
      FNotifyList[i].NotifyBlocksChanged;
  finally
    FNotifyLock.Release;
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
    if Assigned(restoreProgressNotify) then
      restoreProgressNotify(Self,TProgressInfo.Create(psLoadingPendingOperations));
    Bank.Storage.LoadPendingBufferOperations(opht,restoreProgressNotify); // New Build 2.1.4 to load pending operations buffer
    n := AddOperations(Nil,opht,oprl,errors,restoreProgressNotify);
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
  FNotifyLock.Acquire;
  try
    for i := 0 to FNotifyList.Count-1 do begin
      if Assigned( FNotifyList[i].OnNodeMessageEvent) then
        FNotifyList[i].FMessages.AddObject(TheMessage,Sender);
    end;
  finally
    FNotifyLock.Release;
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
  RequireMempoolAvailable;
  FLockMempool.AcquireReadLock;
  try
    RequireMempoolAvailable;
    Result := FMemPoolOperationsComp;
  except
    FLockMempool.ReleaseReadLock;
    raise;
  end;
end;

function TNode.TryLockMempoolRead(MaxWaitMilliseconds: Cardinal;
  out AMempool: TPCOperationsComp): Boolean;
begin
  AMempool := Nil;
  if not PendingOperationsAvailable then Exit(False);
  Result := TryLockMempoolOperationsRead(MaxWaitMilliseconds,AMempool);
  if Result and not PendingOperationsAvailable then begin
    FLockMempool.ReleaseReadLock;
    AMempool := Nil;
    Result := False;
  end;
end;

function TNode.TryLockMempoolOperationsRead(MaxWaitMilliseconds: Cardinal;
  out AMempool: TPCOperationsComp): Boolean;
begin
  AMempool := Nil;
  if MaxWaitMilliseconds > 60000 then MaxWaitMilliseconds := 60000;
  Result := FLockMempool.TryAcquireReadLock(Integer(MaxWaitMilliseconds));
  if Result then AMempool := FMemPoolOperationsComp;
end;

function TNode.PendingOperationsAvailable: Boolean;
begin
  FSynchronizationLock.Acquire;
  try
    Result := FSynchronizationStatus.PendingOperationsAvailable;
  finally
    FSynchronizationLock.Release;
  end;
end;

procedure TNode.RequireMempoolAvailable;
begin
  if HasStorageFailure then
    raise ENodeStorageUnavailable.Create(GetSynchronizationStatus.StorageFailure.ErrorMessage);
  if not PendingOperationsAvailable then
    raise EPendingOperationsUnavailable.Create('Pending operations are unavailable during blockchain synchronization');
end;

procedure TNode.UnlockMempoolRead;
begin
  FLockMempool.ReleaseReadLock;
end;

function TNode.LockMempoolUpgradeableRead: TPCOperationsComp;
begin
  // Ordinary readers may coexist; another potential upgrader waits until exit.
  RequireMempoolAvailable;
  FLockMempool.AcquireUpgradeableReadLock;
  try
    RequireMempoolAvailable;
    Result := FMemPoolOperationsComp;
  except
    FLockMempool.ReleaseUpgradeableReadLock;
    raise;
  end;
end;

procedure TNode.UnlockMempoolUpgradeableRead;
begin
  // The caller must first release any nested write acquisition it entered.
  FLockMempool.ReleaseUpgradeableReadLock;
end;

function TNode.LockMempoolWrite: TPCOperationsComp;
begin
  // When called inside an upgradeable read, wait for other readers to drain.
  // UnlockMempoolWrite then returns to that retained outer upgradeable scope.
  RequireMempoolAvailable;
  FLockMempool.AcquireWriteLock;
  try
    RequireMempoolAvailable;
    Result := FMemPoolOperationsComp;
  except
    FLockMempool.ReleaseWriteLock;
    raise;
  end;
end;

function TNode.LockMempoolOperationsWrite: TPCOperationsComp;
begin
  // Reservations, cleanup, signature caches and final serialization need the
  // operation sequence itself, never the unavailable pending account overlay.
  FLockMempool.AcquireWriteLock;
  Result := FMemPoolOperationsComp;
end;

procedure TNode.UnlockMempoolWrite;
begin
  try
    if FLockMempool.RecursiveWriteCount=1 then CaptureSynchronizationMempoolSummary;
  finally
    FLockMempool.ReleaseWriteLock;
  end;
end;

procedure TNode.OnBankNewBlock(Sender: TObject);
var LLockedMempool : TPCOperationsComp;
  LPreviousPendingHash: TRawBytes; LDefer: Boolean;
begin
  // Bank validation still completes synchronously and exactly. Only rebuilding
  // the separate pending overlay is deferred while a received backlog exists.
  FLockMempool.AcquireWriteLock;
  try
    LLockedMempool := FMemPoolOperationsComp;
    FSynchronizationLock.Acquire;
    try
      LDefer := not FSynchronizationStatus.PendingOperationsAvailable;
      if LDefer and (LLockedMempool.Count>0) then begin
        FMempoolNeedsReconciliation := True;
        RecalculateSynchronizationStatus;
      end;
    finally
      FSynchronizationLock.Release;
    end;
    if not LDefer or (LLockedMempool.Count=0) then begin
      LPreviousPendingHash := LLockedMempool.OperationsHashTree.HashTree;
      LLockedMempool.SanitizeOperations;
      if Assigned(FSaveMempoolOperationsThread) and
        not TBaseType.Equals(LPreviousPendingHash,LLockedMempool.OperationsHashTree.HashTree) then
        FSaveMempoolOperationsThread.Touch;
    end;
    // Confirmed status remains current even when pending balances are hidden.
    CaptureSynchronizationMempoolSummary;
  finally
    FLockMempool.ReleaseWriteLock;
  end;
  NotifyBlocksChanged;
  WakeMempoolMaintenance;
end;

function TNode.SendNodeMessage(Target: TNetConnection; const TheMessage: String; var errors: String): Boolean;
Var i,j : Integer;
  nc : TNetConnection;
  s : String;
begin
  Result := false;
  if Not TryLockNode(4000) then begin
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
    UnlockNode;
  end;
end;

procedure TNode.LogBlockChainBatchReceived(SenderConnection: TNetConnection;
  ACount, AFirstBlock, ALastBlock: Cardinal; APayloadBytes: Int64);
var LSource, LRemoteAddress: String;
begin
  if Assigned(SenderConnection) then begin
    LSource := SenderConnection.ClassName;
    LRemoteAddress := SenderConnection.ClientRemoteAddr;
  end else begin
    LSource := ClassName;
    LRemoteAddress := 'NIL';
  end;
  // Receipt is separate from acceptance: the whole response is already local
  // before validation/reconciliation can continue after networking is stopped.
  FNodeLog.NotifyNewLog(ltinfo,LSource,Format(
    'Block batch received: Count:%d First:%d Last:%d PayloadBytes:%d Peer:%s',[
    ACount,AFirstBlock,ALastBlock,APayloadBytes,LRemoteAddress]));
end;

procedure TNode.LogBlockChainResult(SenderConnection: TNetConnection;
  const AOperationBlock: TOperationBlock; AAccepted: Boolean; const AErrors: String);
var LSource, LRemoteAddress: String;
  LAcceptedAt: Cardinal;
begin
  if Assigned(SenderConnection) then begin
    LSource := SenderConnection.ClassName;
    LRemoteAddress := SenderConnection.ClientRemoteAddr;
  end else begin
    LSource := ClassName;
    LRemoteAddress := 'NIL';
  end;
  LAcceptedAt := UnivDateTimeToUnix(DateTime2UnivDateTime(Now));
  // Keep the original columns: the elapsed value is the block's age at
  // acceptance, not its processing duration. Batch downloads must log here
  // without adopting AddNewBlockChain's announcement/rebroadcast behavior.
  if AAccepted then begin
    FNodeLog.NotifyNewLog(ltupdate,LSource,Format(';%d;%s;%s;;%d;%d;%d;%s',[
      AOperationBlock.block,LRemoteAddress,AOperationBlock.block_payload.ToPrintable,
      AOperationBlock.timestamp,LAcceptedAt,LAcceptedAt - AOperationBlock.timestamp,
      IntToHex(AOperationBlock.compact_target,8)]));
  end else begin
    FNodeLog.NotifyNewLog(lterror,LSource,Format(';%d;%s;%s;%s;%d;%d;%d;%s',[
      AOperationBlock.block,LRemoteAddress,AOperationBlock.block_payload.ToPrintable,AErrors,
      AOperationBlock.timestamp,LAcceptedAt,LAcceptedAt - AOperationBlock.timestamp,
      IntToHex(AOperationBlock.compact_target,8)]));
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
  Node := Nil;
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

procedure TNodeNotifyEvents.NotifySynchronizationChanged;
begin
  if not Assigned(FThreadSafeNodeNotifyEvent) then Exit;
  {$IFDEF FPC}
  InterlockedExchange(FThreadSafeNodeNotifyEvent.FNotifySynchronizationChanged,1);
  {$ELSE}
  TInterlocked.Exchange(FThreadSafeNodeNotifyEvent.FNotifySynchronizationChanged,1);
  {$ENDIF}
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
    FNode.FNotifyLock.Acquire;
    try
      FNode.FNotifyList.Remove(Self);
    finally
      FNode.FNotifyLock.Release;
    end;
  end;
  FNode := Value;
  if Assigned(FNode) then begin
    FNode.FreeNotification(Self);
    FNode.FNotifyLock.Acquire;
    try
      FNode.FNotifyList.Add(Self);
    finally
      FNode.FNotifyLock.Release;
    end;
    NotifySynchronizationChanged;
  end;
end;

{ TThreadSafeNodeNotifyEvent }

procedure TThreadSafeNodeNotifyEvent.BCExecute;
begin
  while (not Terminated) AND (Assigned(FNodeNotifyEvents)) do begin
    if (FNotifyOperationsChanged) Or (FNotifyBlocksChanged) Or
      (FNotifySynchronizationChanged<>0) Or
      (FNodeNotifyEvents.FMessages.Count>0) then Synchronize(SynchronizedProcess);
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
  LStatus: TNodeSynchronizationStatus;
  LNotifySynchronization: Boolean;
begin
  LNotifySynchronization := False;
  Try
    If (Terminated) Or (Not Assigned(FNodeNotifyEvents)) then exit;
    // Clear first, then sample: a producer arriving during the copy leaves a
    // notification for the next pass. No producer waits for these callbacks.
    {$IFDEF FPC}
    LNotifySynchronization := InterlockedExchange(FNotifySynchronizationChanged,0)<>0;
    {$ELSE}
    LNotifySynchronization := TInterlocked.Exchange(FNotifySynchronizationChanged,0)<>0;
    {$ENDIF}
    LStatus := Default(TNodeSynchronizationStatus);
    if Assigned(FNodeNotifyEvents.FNode) then
      LStatus := FNodeNotifyEvents.FNode.GetSynchronizationStatus;
    // Pause queries before block/operation listeners run. On completion, retain
    // their latest requests first, then resume exactly once with final inputs.
    if LNotifySynchronization and (LStatus.State in [nssSynchronizing,nssStorageError]) and
      Assigned(FNodeNotifyEvents.FOnSynchronizationChanged) then begin
      DebugStep := 'Notify OnSynchronizationChanged';
      FNodeNotifyEvents.FOnSynchronizationChanged(FNodeNotifyEvents,LStatus);
    end;
    try
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
    finally
      // An unrelated refresh listener must not consume the final state flag
      // and leave a grid held forever. Deliver completion even on that path.
      if LNotifySynchronization and (not (LStatus.State in [nssSynchronizing,nssStorageError])) and
        Assigned(FNodeNotifyEvents) and
        Assigned(FNodeNotifyEvents.FOnSynchronizationChanged) then begin
        DebugStep := 'Notify OnSynchronizationChanged';
        FNodeNotifyEvents.FOnSynchronizationChanged(FNodeNotifyEvents,LStatus);
      end;
    end;
  Except
    On E:Exception do begin
      // A one-shot status-handler failure can recover without another producer
      // input. Retry on the existing notifier, never on the network call stack.
      if LNotifySynchronization and Assigned(FNodeNotifyEvents) then
        FNodeNotifyEvents.NotifySynchronizationChanged;
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
var LStart: TTickCount; LDelay: Cardinal;
begin
  if Terminated then Exit;
  if Assigned(FLeaseOwner) then begin
    // The transferred connection lease pins both connection and owner. Keep
    // the old randomized send delay, but let orderly shutdown wake it promptly.
    LDelay := Random(3000);
    LStart := TPlatform.GetTickCount;
    while TPlatform.GetElapsedMilliseconds(LStart)<LDelay do begin
      if Terminated or FLeaseOwner.ShutdownRequested then Exit;
      Sleep(10);
    end;
    LStart := TPlatform.GetTickCount;
    while not Terminated and not FLeaseOwner.ShutdownRequested do begin
      if FLeaseOwner.ConnectionLock(Self,FNetConnection,100) then begin
        try
          if FNetConnection.Connected then FNetConnection.Send_AddOperations(Nil);
        finally FLeaseOwner.ConnectionUnlock(FNetConnection); end;
        Exit;
      end;
      if TPlatform.GetElapsedMilliseconds(LStart)>=5000 then Exit;
    end;
    Exit;
  end;
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

constructor TThreadNodeNotifyOperations.Create(NetConnection: TNetConnection;
  MakeACopyOfOperationsHashTree: TOperationsHashTree; ALeaseOwner: TNetData);
begin
  FNetConnection := NetConnection;
  FLeaseOwner := ALeaseOwner;
  Inherited Create(True);
  FreeOnTerminate := false;
  FNetConnection.AddOperationsToBufferForSend(MakeACopyOfOperationsHashTree);
  FreeOnTerminate := true;
  Suspended:=False;
end;

destructor TThreadNodeNotifyOperations.Destroy;
var LLeaseOwner: TNetData;
begin
  LLeaseOwner := FLeaseOwner;
  try
    inherited;
  finally
    FLeaseOwner := Nil;
    if Assigned(LLeaseOwner) then LLeaseOwner.ReleaseDownloadConnection(FNetConnection);
  end;
end;

{ TSaveMempoolOperationsThread }

constructor TSaveMempoolOperationsThread.Create(ANode: TNode);
begin
  FNode := ANode;
  FWorkAvailable := TEvent.Create(Nil,False,False,'');
  inherited Create(True);
end;

destructor TSaveMempoolOperationsThread.Destroy;
begin
  inherited;
  FWorkAvailable.Free;
end;

procedure TSaveMempoolOperationsThread.Wake;
begin
  FWorkAvailable.SetEvent;
end;

procedure TSaveMempoolOperationsThread.Stop;
begin
  Terminate;
  Wake;
end;

procedure TSaveMempoolOperationsThread.BCExecute;
var LLocked : TPCOperationsComp;
  LSaveFailed, LFinalAttempt: Boolean;
  LRetryReconciliation: Boolean;
  LLastSave: TTickCount; LElapsed: Int64; LWait: Cardinal;
  LLastReconciliationFailure: TTickCount; LRetryWait: Cardinal;
  LFailureFileName: String;
begin
  // A request can arrive before this thread is first scheduled. Do not clear
  // it here. Dirty publication is atomic and is consumed before capture, so a
  // later mutation cannot be hidden by completion of an older file write.
  LLastSave := TPlatform.GetTickCount;
  LRetryReconciliation := False;
  LLastReconciliationFailure := LLastSave;
  repeat
    LFinalAttempt := Terminated;
    LSaveFailed := False;
    if (not Terminated) and ((not LRetryReconciliation) or
      (TPlatform.GetElapsedMilliseconds(LLastReconciliationFailure)>=10000)) then begin
      try
        FNode.ReconcileMempoolIfReady;
        LRetryReconciliation := False;
      except
        on E: Exception do begin
          // Retain the original raw package and retry on this owner's existing
          // timed event wait. A transient validator failure must not leave an
          // offline node waiting forever; persistent failures must not spin.
          LRetryReconciliation := True;
          LLastReconciliationFailure := TPlatform.GetTickCount;
          TLog.NewLog(lterror,ClassName,'Cannot reconcile pending operations: '+E.ClassName+': '+E.Message);
        end;
      end;
    end;
    LElapsed := TPlatform.GetElapsedMilliseconds(LLastSave);
    if (Terminated or (LElapsed>=10000)) and TakeSaveRequest then begin
      try
        // The saved package contains the complete original operation sequence,
        // not the deferred account overlay. Saving it during catch-up or final
        // shutdown is safe; the normal next startup revalidates that sequence.
        FNode.FLockMempool.AcquireReadLock;
        LLocked := FNode.FMemPoolOperationsComp;
        try
          FNode.Bank.Storage.SavePendingBufferOperations(LLocked.OperationsHashTree);
        finally
          FNode.UnlockMempoolRead;
        end;
      except
        on E: Exception do begin
          LSaveFailed := True;
          LFailureFileName := 'pendingbuffer.ops';
          try
            LFailureFileName := FNode.Bank.GetStorageFolder(FNode.Bank.Orphan)+PathDelim+'pendingbuffer.ops';
          except
            // A diagnostic path lookup must not replace the original I/O failure.
          end;
          FNode.ReportStorageFailure(sfsPendingOperations,LFailureFileName,E);
          Touch;
          TLog.NewLog(lterror,ClassName,'Cannot save pending operations: '+E.ClassName+': '+E.Message);
        end;
      end;
      LLastSave := TPlatform.GetTickCount;
    end;
    // Termination during the quiet wait returns to the capture above first.
    // Normal shutdown therefore flushes the latest completed operation batch.
    // A failing final I/O attempt is logged rather than retried forever.
    if Terminated and ((LSaveFailed and LFinalAttempt) or not HasSaveRequest) then Break;
    // Existing ten-second persistence pacing is retained, but reconciliation
    // is woken by real scope/connectivity events. No status polling timer is
    // involved, and stopping wakes the owner to make its final complete save.
    if HasSaveRequest then begin
      LElapsed := TPlatform.GetElapsedMilliseconds(LLastSave);
      if LElapsed>=10000 then LWait := 0 else LWait := 10000-Cardinal(LElapsed);
    end else LWait := Cardinal(-1);
    if LRetryReconciliation then begin
      LElapsed := TPlatform.GetElapsedMilliseconds(LLastReconciliationFailure);
      if LElapsed>=10000 then LRetryWait := 0 else LRetryWait := 10000-Cardinal(LElapsed);
      if (LWait=Cardinal(-1)) or (LRetryWait<LWait) then LWait := LRetryWait;
    end;
    FWorkAvailable.WaitFor(LWait);
  until False;
end;

procedure TSaveMempoolOperationsThread.Touch;
begin
  {$IFDEF FPC}
  InterlockedExchange(FPendingToSave,1);
  {$ELSE}
  TInterlocked.Exchange(FPendingToSave,1);
  {$ENDIF}
  Wake;
end;

function TSaveMempoolOperationsThread.TakeSaveRequest: Boolean;
begin
  {$IFDEF FPC}
  Result := InterlockedExchange(FPendingToSave,0)<>0;
  {$ELSE}
  Result := TInterlocked.Exchange(FPendingToSave,0)<>0;
  {$ENDIF}
end;

function TSaveMempoolOperationsThread.HasSaveRequest: Boolean;
begin
  {$IFDEF FPC}
  Result := InterlockedCompareExchange(FPendingToSave,0,0)<>0;
  {$ELSE}
  Result := TInterlocked.CompareExchange(FPendingToSave,0,0)<>0;
  {$ENDIF}
end;

initialization
  _Node := Nil;
  _PascalCoinDataFolder := '';
finalization
  FreeAndNil(_Node);
end.
