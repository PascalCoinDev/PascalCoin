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
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF},
  UBlockChain, UNetProtocol, UAccounts, UCrypto, UThread, SyncObjs, ULog, UBaseTypes, UPCOrderedLists;

{$I config.inc}

Type

  { TNode }

  TSearchOperationResult = (found, invalid_params, blockchain_block_not_found);

  TNodeNotifyEvents = Class;

  TNode = Class(TComponent)
  private
    FNodeLog : TLog;
    FLockNodeOperations : TPCCriticalSection;
    FOperationSequenceLock : TPCCriticalSection;
    FNotifyList : TList<TNodeNotifyEvents>;
    FBank : TPCBank;
    FOperations : TPCOperationsComp;
    FNetServer : TNetServer;
    FBCBankNotify : TPCBankNotify;
    FPeerCache : String;
    FDisabledsNewBlocksCount : Integer;
    FSentOperations : TOrderedRawList;
    FBroadcastData : Boolean;
    FUpdateBlockchain: Boolean;
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
    //
    Property Operations : TPCOperationsComp read FOperations;
    //
    Function AddNewBlockChain(SenderConnection : TNetConnection; NewBlockOperations: TPCOperationsComp; var newBlockAccount: TBlockAccount; var errors: String): Boolean;
    Function AddOperations(SenderConnection : TNetConnection; OperationsHashTree : TOperationsHashTree; OperationsResult : TOperationsResumeList; var errors: String): Integer;
    Function AddOperation(SenderConnection : TNetConnection; Operation : TPCOperation; var errors: String): Boolean;
    Function SendNodeMessage(Target : TNetConnection; const TheMessage : String; var errors : String) : Boolean;
    //
    Procedure NotifyBlocksChanged;
    //
    procedure GetStoredOperationsFromAccount(AOwnerThread : TPCThread; const OperationsResume: TList<TOperationResume>; account_number: Cardinal; MaxDepth, StartOperation, EndOperation : Integer; SearchBackwardsStartingAtBlock : Cardinal=0); overload;
    procedure GetStoredOperationsFromAccount(const OperationsResume: TOperationsResumeList; account_number: Cardinal; MaxDepth, StartOperation, EndOperation : Integer; SearchBackwardsStartingAtBlock : Cardinal=0); overload;
    Function FindOperation(Const OperationComp : TPCOperationsComp; Const OperationHash : TRawBytes; var block : Cardinal; var operation_block_index : Integer) : Boolean;
    Function FindOperationExt(Const OperationComp : TPCOperationsComp; Const OperationHash : TRawBytes; var block : Cardinal; var operation_block_index : Integer) : TSearchOperationResult;
    Function FindNOperation(block, account, n_operation : Cardinal; var OpResume : TOperationResume) : TSearchOperationResult;
    Function FindNOperations(account, start_block : Cardinal; allow_search_previous : Boolean; n_operation_low, n_operation_high : Cardinal; OpResumeList : TOperationsResumeList) : TSearchOperationResult;
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
    Property BroadcastData : Boolean read FBroadcastData write FBroadcastData;
    Property UpdateBlockchain : Boolean read FUpdateBlockchain write FUpdateBlockchain;
    procedure MarkVerifiedECDSASignaturesFromMemPool(newOperationsToValidate : TPCOperationsComp);
    class function NodeVersion : String;
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

Uses UOpTransaction, UConst, UTime, UCommon;

var _Node : TNode;

{ TNode }

function TNode.AddNewBlockChain(SenderConnection: TNetConnection; NewBlockOperations: TPCOperationsComp;
  var newBlockAccount: TBlockAccount; var errors: String): Boolean;
Var i,j,maxResend : Integer;
  nc : TNetConnection;
  s,sClientRemoteAddr : String;
  OpBlock : TOperationBlock;
  opsht : TOperationsHashTree;
  minBlockResend : Cardinal;
  resendOp : TPCOperation;
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
    If Not TPCThread.TryProtectEnterCriticalSection(Self,5000,FLockNodeOperations) then begin
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
      Result := Bank.AddNewBlockChainBlock(NewBlockOperations,TNetData.NetData.NetworkAdjustedTime.GetMaxAllowedTimestampForNewBlock,newBlockAccount,errors);
      if Result then begin
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
          j := Random(3); // j=0,1 or 2
          If (Bank.LastBlockFound.OperationBlock.block>j) then
            minBlockResend:=Bank.LastBlockFound.OperationBlock.block - j
          else minBlockResend:=1;
          maxResend := CT_MaxResendMemPoolOperations;
          i := 0;
          While (opsht.OperationsCount<maxResend) And (i<FOperations.Count) do begin
            resendOp := FOperations.Operation[i];
            j := FSentOperations.GetTag(resendOp.Sha256);
            if (j=0) Or (j<=minBlockResend) then begin
              // Only will "re-send" operations that where received on block <= minBlockResend
              opsht.AddOperationToHashTree(resendOp);
              // Add to sent operations
              FSentOperations.SetTag(resendOp.Sha256,FOperations.OperationBlock.block); // Set tag new value
              FSentOperations.Add(FOperations.Operation[i].Sha256,Bank.LastBlockFound.OperationBlock.block);
            end else begin
              {$IFDEF HIGHLOG}TLog.NewLog(ltInfo,ClassName,'Sanitized operation not included to resend (j:'+IntToStr(j)+'>'+inttostr(minBlockResend)+') ('+inttostr(i+1)+'/'+inttostr(FOperations.Count)+'): '+FOperations.Operation[i].ToString);{$ENDIF}
            end;
            inc(i);
          end;
          If FOperations.Count>0 then begin
            TLog.NewLog(ltinfo,classname,Format('Resending %d operations for new block (Buffer Pending Operations:%d)',[opsht.OperationsCount,FOperations.Count]));
            {$IFDEF HIGHLOG}
            if opsht.OperationsCount>0 then begin
              for i := 0 to opsht.OperationsCount - 1 do begin
                TLog.NewLog(ltInfo,ClassName,'Resending ('+inttostr(i+1)+'/'+inttostr(opsht.OperationsCount)+'): '+opsht.GetOperation(i).ToString);
              end;
            end
            {$ENDIF}
          end;
          // Clean sent operations buffer
          j := 0;
          for i := FSentOperations.Count-1 downto 0 do begin
            If (FSentOperations.GetTag(i)<Bank.LastBlockFound.OperationBlock.block-2) then begin
              FSentOperations.Delete(i);
              inc(j);
            end;
          end;
          if j>0 then begin
            TLog.NewLog(ltInfo,ClassName,'Buffer Sent operations: Deleted '+IntToStr(j)+' old operations');
          end;
          TLog.NewLog(ltdebug,ClassName,'Buffer Sent operations: '+IntToStr(FSentOperations.Count));
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
      FLockNodeOperations.Release;
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

function TNode.AddOperations(SenderConnection : TNetConnection; OperationsHashTree : TOperationsHashTree; OperationsResult : TOperationsResumeList; var errors: String): Integer;
  {$IFDEF BufferOfFutureOperations}
  Procedure Process_BufferOfFutureOperations(valids_operations : TOperationsHashTree);
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
        If FOperations.AddOperation(true,ActOp,e) then begin
          TLog.NewLog(ltInfo,Classname,Format('AddOperation FromBufferWaitingOperations %d/%d: %s',[i+1,FBufferAuxWaitingOperations.OperationsCount,ActOp.ToString]));
          inc(nAdded);
          valids_operations.AddOperationToHashTree(ActOp);
          FBufferAuxWaitingOperations.Delete(i);
        end else begin
          sAcc := FOperations.SafeBoxTransaction.Account(ActOp.SignerAccount);
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
  valids_operations : TOperationsHashTree;
  nc : TNetConnection;
  e : String;
  s : String;
  OPR : TOperationResume;
  ActOp : TPCOperation;
  {$IFDEF BufferOfFutureOperations}sAcc : TAccount;{$ENDIF}
begin
  Result := -1; // -1 Means Node is blocked or disabled
  if Assigned(OperationsResult) then OperationsResult.Clear;
  if FDisabledsNewBlocksCount>0 then begin
    errors := Format('Cannot Add Operations due is adding disabled - OpCount:%d',[OperationsHashTree.OperationsCount]);
    TLog.NewLog(ltinfo,Classname,errors);
    exit;
  end;
  nSpam := 0;
  nRepeated := 0;
  nError := 0;
  errors := '';
  valids_operations := TOperationsHashTree.Create;
  try
    {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,Classname,Format('AddOperations Connection:%s Operations:%d',[
      Inttohex(PtrInt(SenderConnection),8),OperationsHashTree.OperationsCount]));{$ENDIF}
    if Not TPCThread.TryProtectEnterCriticalSection(Self,4000,FLockNodeOperations) then begin
      s := 'Cannot AddOperations due blocking lock operations node';
      TLog.NewLog(lterror,Classname,s);
      if TThread.CurrentThread.ThreadID=MainThreadID then raise Exception.Create(s) else exit;
    end;
    try
      Result := 0;
      {$IFDEF BufferOfFutureOperations}
      Process_BufferOfFutureOperations(valids_operations);
      {$ENDIF}
      for j := 0 to OperationsHashTree.OperationsCount-1 do begin
        ActOp := OperationsHashTree.GetOperation(j);
        If (FOperations.OperationsHashTree.IndexOfOperation(ActOp)<0) then begin
          // Protocol 2 limitation: In order to prevent spam of operations without Fee, will protect it
          If (ActOp.OperationFee=0) And (Bank.SafeBox.CurrentProtocol>=CT_PROTOCOL_2) And
             (FOperations.OperationsHashTree.CountOperationsBySameSignerWithoutFee(ActOp.SignerAccount)>=CT_MaxAccountOperationsPerBlockWithoutFee) then begin
            inc(nSpam);
            e := Format('Account %s zero fee operations per block limit:%d',[TAccountComp.AccountNumberToAccountTxtNumber(ActOp.SignerAccount),CT_MaxAccountOperationsPerBlockWithoutFee]);
            if (nSpam<=5) then begin  // To Limit errors in a String... speed up
              if (errors<>'') then errors := errors+' ';
              errors := errors+'Op '+IntToStr(j+1)+'/'+IntToStr(OperationsHashTree.OperationsCount)+':'+e;
            end;
            TLog.NewLog(ltdebug,Classname,Format('AddOperation spam %d/%d: %s  - Error:%s',
              [(j+1),OperationsHashTree.OperationsCount,ActOp.ToString,e]));
            if Assigned(OperationsResult) then begin
              TPCOperation.OperationToOperationResume(0,ActOp,True,ActOp.SignerAccount,OPR);
              OPR.valid := false;
              OPR.NOpInsideBlock:=-1;
              OPR.OperationHash := Nil;
              OPR.errors := e;
              OperationsResult.Add(OPR);
            end;
          end else begin
            if (FOperations.AddOperation(true,ActOp,e)) then begin
              inc(Result);
              FSentOperations.Add(ActOp.Sha256,FOperations.OperationBlock.block);
              valids_operations.AddOperationToHashTree(ActOp);
              TLog.NewLog(ltdebug,Classname,Format('AddOperation %d/%d: %s',[(j+1),OperationsHashTree.OperationsCount,ActOp.ToString]));
              if Assigned(OperationsResult) then begin
                TPCOperation.OperationToOperationResume(0,ActOp,True,ActOp.SignerAccount,OPR);
                OPR.NOpInsideBlock:=FOperations.Count-1;
                OPR.Balance := FOperations.SafeBoxTransaction.Account(ActOp.SignerAccount).balance;
                OperationsResult.Add(OPR);
              end;
            end else begin
              inc(nError);
              if (nError<=5) then begin  // To Limit errors in a String... speed up
                if (errors<>'') then errors := errors+' ';
                errors := errors+'Op '+IntToStr(j+1)+'/'+IntToStr(OperationsHashTree.OperationsCount)+':'+e;
              end;
              TLog.NewLog(ltdebug,Classname,Format('AddOperation invalid/duplicated %d/%d: %s  - Error:%s',
                [(j+1),OperationsHashTree.OperationsCount,ActOp.ToString,e]));
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
                sAcc := FOperations.SafeBoxTransaction.Account(ActOp.SignerAccount);
                If (sAcc.n_operation<ActOp.N_Operation) Or
                   ((sAcc.n_operation=ActOp.N_Operation) AND (sAcc.balance=0) And (ActOp.OperationFee>0) And (ActOp.OpType = CT_Op_Changekey)) then begin
                  If FBufferAuxWaitingOperations.IndexOfOperation(ActOp)<0 then begin
                    FBufferAuxWaitingOperations.AddOperationToHashTree(ActOp);
                    TLog.NewLog(ltInfo,Classname,Format('New FromBufferWaitingOperations %d/%d (new buffer size:%d): %s',[j+1,OperationsHashTree.OperationsCount,FBufferAuxWaitingOperations.OperationsCount,ActOp.ToString]));
                  end;
                end;
              end;
              {$ENDIF}
            end;
          end;
        end else begin
          inc(nRepeated);
          e := Format('AddOperation made before %d/%d: %s',[(j+1),OperationsHashTree.OperationsCount,ActOp.ToString]);
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
          {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,Classname,Format('AddOperation made before %d/%d: %s',[(j+1),OperationsHashTree.OperationsCount,ActOp.ToString]));{$ENDIF}
        end;
      end;
      // Save operations buffer
      If Result<>0 then begin
        Bank.Storage.SavePendingBufferOperations(Self.Operations.OperationsHashTree);
      end;
    finally
      FLockNodeOperations.Release;
      if Result<>0 then begin
        if Assigned(SenderConnection) then begin
          s := SenderConnection.ClientRemoteAddr;
        end else s := '(SELF)';
        TLog.NewLog(ltdebug,Classname,Format('Finalizing AddOperations from %s Operations:%d valids:%d spam:%d invalids:%d repeated:%d',[s,OperationsHashTree.OperationsCount,Result,nSpam,nError,nRepeated]));
      end;
    end;
    if Result=0 then exit;
    if FBroadcastData then begin
      // Send to other nodes
      j := TNetData.NetData.ConnectionsCountAll;
      for i:=0 to j-1 do begin
        If TNetData.NetData.GetConnection(i,nc) then begin
          if (nc<>SenderConnection) And (nc.Connected) And (nc.RemoteOperationBlock.block>0) then TThreadNodeNotifyOperations.Create(nc,valids_operations);
        end;
      end;
    end;
  finally
    valids_operations.Free;
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
  FSentOperations := TOrderedRawList.Create;
  FNodeLog := TLog.Create(Self);
  FNodeLog.ProcessGlobalLogs := false;
  RegisterOperationsClass;
  if Assigned(_Node) then raise Exception.Create('Duplicate nodes protection');
  TLog.NewLog(ltInfo,ClassName,'TNode.Create '+NodeVersion);
  inherited;
  FDisabledsNewBlocksCount := 0;
  FLockNodeOperations := TPCCriticalSection.Create('TNode_LockNodeOperations');
  FOperationSequenceLock := TPCCriticalSection.Create('TNode_OperationSequenceLock');
  FBank := TPCBank.Create(Self);
  FBCBankNotify := TPCBankNotify.Create(Self);
  FBCBankNotify.Bank := FBank;
  FBCBankNotify.OnNewBlock := OnBankNewBlock;
  FNetServer := TNetServer.Create;
  FOperations := TPCOperationsComp.Create(Nil);
  FOperations.bank := FBank;
  FNotifyList := TList<TNodeNotifyEvents>.Create;
  {$IFDEF BufferOfFutureOperations}
  FBufferAuxWaitingOperations := TOperationsHashTree.Create;
  {$ENDIF}
  FBroadcastData := True;
  FUpdateBlockchain := True;
  if Not Assigned(_Node) then _Node := Self;
end;

class procedure TNode.DecodeIpStringToNodeServerAddressArray(
  const Ips: String; Var NodeServerAddressArray: TNodeServerAddressArray);
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
    while (i<=High(ips_string)) AND (NOT (ips_string.Chars[i] IN CT_IP_CHARS)) do inc(i);
    if (i>Low(ips_string)) then ips_string := ips_string.Substring(i,Length(ips_string));
    // Capture IP value
    i := 0;
    while (i<=High(ips_string)) and (ips_string.Chars[i] in CT_IP_CHARS) do inc(i);
    if (i>0) then begin
      nsa.ip := ips_string.Substring(0,i);
      // Capture possible :Port value
      if (i<=High(ips_string)) and (ips_string.Chars[i]=':') then begin
        inc(i);
        port := '';
        while (i<=High(ips_string)) and (ips_string.Chars[i] in ['0'..'9']) do begin
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
    step := 'Deleting critical section';
    FreeAndNil(FLockNodeOperations);
    FreeAndNil(FOperationSequenceLock);

    step := 'Desactivating server';
    FNetServer.Active := false;

    step := 'Destroying NetServer';
    FreeAndNil(FNetServer);

    step := 'Destroying NotifyList';
    FreeAndNil(FNotifyList);
    step := 'Destroying Operations';
    FreeAndNil(FOperations);
    step := 'Assigning NIL to node var';
    if _Node=Self then _Node := Nil;
    Step := 'Destroying SentOperations list';
    FreeAndNil(FSentOperations);

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

function TNode.TryLockNode(MaxWaitMilliseconds: Cardinal): Boolean;
begin
  Result := TPCThread.TryProtectEnterCriticalSection(Self,MaxWaitMilliseconds,FLockNodeOperations);
end;

procedure TNode.UnlockNode;
begin
  FLockNodeOperations.Release;
end;

procedure TNode.MarkVerifiedECDSASignaturesFromMemPool(newOperationsToValidate: TPCOperationsComp);
begin
  // Introduced on Build 4.0.2 to increase speed using MEMPOOL verified operations instead of verify again everytime
  // Will check if "newOperationsToValidate" operations are on MEMPOOL. If found, will set same FHasValidSignature value in order to mark as verified
  if newOperationsToValidate = FOperations then Exit; // Is the same, do nothing
  if newOperationsToValidate.OperationBlock.protocol_version <> newOperationsToValidate.OperationBlock.protocol_version then Exit; // Must be same protocol
  newOperationsToValidate.Lock;
  try
    FLockNodeOperations.Acquire;
    try
      Operations.OperationsHashTree.MarkVerifiedECDSASignatures(newOperationsToValidate.OperationsHashTree);
    finally
      FLockNodeOperations.Release;
    end;
  finally
    newOperationsToValidate.Unlock;
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

function TNode.IsReady(Var CurrentProcess: String): Boolean;
begin
  Result := false;
  CurrentProcess := '';
  if FBank.IsReady(CurrentProcess) then begin
    if FNetServer.Active then begin
      if Not TNetData.NetData.IsGettingNewBlockChainFromClient(CurrentProcess) then begin
        if TNetData.NetData.MaxRemoteOperationBlock.block>FOperations.OperationBlock.block then begin
          CurrentProcess := 'Found block '+inttostr(TNetData.NetData.MaxRemoteOperationBlock.block)+' (Wait until downloaded)';
        end else begin
          CurrentProcess := '';
          Result := true;
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
  Result := CT_ClientAppVersion{$IFDEF LINUX}+'L'{$ELSE}+'W'{$ENDIF}{$IFDEF FPC}{$IFDEF LCL}+'l'{$ELSE}+'f'{$ENDIF}{$ENDIF}{$IFDEF FPC}{$IFDEF CPU32}+'32b'{$ELSE}+'64b'{$ENDIF}{$ELSE}{$IFDEF CPU32BITS}+'32b'{$ELSE}+'64b'{$ENDIF}{$ENDIF};
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

procedure TNode.GetStoredOperationsFromAccount(AOwnerThread : TPCThread; const OperationsResume: TList<TOperationResume>; account_number: Cardinal;
  MaxDepth, StartOperation, EndOperation: Integer; SearchBackwardsStartingAtBlock: Cardinal);
  // Optimization:
  // For better performance, will only include at "OperationsResume" values betweeen "startOperation" and "endOperation"

  // New use case: Will allow to start in an unknown block when first_block_is_unknows
  Procedure DoGetFromBlock(block_number : Integer; last_balance : Int64; act_depth : Integer; nOpsCounter : Integer; first_block_is_unknown : Boolean);
  var opc : TPCOperationsComp;
    op : TPCOperation;
    OPR : TOperationResume;
    l : TList<Cardinal>;
    i : Integer;
    last_block_number : Integer;
    found_in_block : Boolean;
    acc_0_miner_reward, acc_4_dev_reward : Int64;
    acc_4_for_dev : Boolean;
  begin
    if Assigned(AOwnerThread) then begin
      if AOwnerThread.terminated then Exit;
    end;
    if (act_depth<=0) then exit;
    opc := TPCOperationsComp.Create(Nil);
    Try
      l := TList<Cardinal>.Create;
      try
        last_block_number := block_number+1;
        while (last_block_number>block_number) And (act_depth>0)
          And (block_number >= (account_number DIV CT_AccountsPerBlock))
          And (nOpsCounter <= EndOperation) do begin
          if Assigned(AOwnerThread) then begin
            if AOwnerThread.terminated then Exit;
          end;
          found_in_block := False;
          last_block_number := block_number;
          l.Clear;
          If not Bank.Storage.LoadBlockChainBlock(opc,block_number) then begin
            TLog.NewLog(ltdebug,ClassName,'Block '+inttostr(block_number)+' not found. Cannot read operations');
            exit;
          end;
          opc.OperationsHashTree.GetOperationsAffectingAccount(account_number,l);
          for i := l.Count - 1 downto 0 do begin
            op := opc.Operation[PtrInt(l.Items[i])];
            If TPCOperation.OperationToOperationResume(block_number,Op,False,account_number,OPR) then begin
              OPR.NOpInsideBlock := PtrInt(l.Items[i]);
              OPR.time := opc.OperationBlock.timestamp;
              OPR.Block := block_number;
              If last_balance>=0 then begin
                OPR.Balance := last_balance;
                last_balance := last_balance - ( OPR.Amount + OPR.Fee );
              end else OPR.Balance := -1; // Undetermined
              if (nOpsCounter>=StartOperation) And (nOpsCounter<=EndOperation) then begin
                OperationsResume.Add(OPR);
              end;
              inc(nOpsCounter);
              found_in_block := True;
            end;
          end;

          // Is a new block operation?
          if (TAccountComp.AccountBlock(account_number)=block_number) then begin
            TPascalCoinProtocol.GetRewardDistributionForNewBlock(opc.OperationBlock,acc_0_miner_reward,acc_4_dev_reward,acc_4_for_dev);
            If ((account_number MOD CT_AccountsPerBlock)=0) Or
               (  ((account_number MOD CT_AccountsPerBlock)=CT_AccountsPerBlock-1) AND (acc_4_for_dev)  ) then begin
              OPR := CT_TOperationResume_NUL;
              OPR.OpType:=CT_PseudoOp_Reward;
              OPR.valid := true;
              OPR.Block := block_number;
              OPR.time := opc.OperationBlock.timestamp;
              OPR.AffectedAccount := account_number;
              If ((account_number MOD CT_AccountsPerBlock)=0) then begin
                OPR.Amount := acc_0_miner_reward;
                OPR.OperationTxt := 'Miner reward';
                OPR.OpSubtype:=CT_PseudoOpSubtype_Miner;
              end else begin
                OPR.Amount := acc_4_dev_reward;
                OPR.OperationTxt := 'Dev reward';
                OPR.OpSubtype:=CT_PseudoOpSubtype_Developer;
              end;
              If last_balance>=0 then begin
               OPR.Balance := last_balance;
              end else OPR.Balance := -1; // Undetermined
              if (nOpsCounter>=StartOperation) And (nOpsCounter<=EndOperation) then begin
               OperationsResume.Add(OPR);
              end;
              inc(nOpsCounter);
              found_in_block := True;
            end;
          end;
          //
          dec(act_depth);
          If (Not found_in_block) And (first_block_is_unknown) then begin
            Dec(block_number);
          end else begin
            block_number := opc.PreviousUpdatedBlocks.GetPreviousUpdatedBlock(account_number,block_number);
          end;
          opc.Clear(true);
        end;
      finally
        l.Free;
      end;
    Finally
      opc.Free;
    End;
  end;

Var acc : TAccount;
  startBlock : Cardinal;
  lastBalance : Int64;
begin
  if MaxDepth<0 then Exit;
  if account_number>=Bank.SafeBox.AccountsCount then Exit;
  if StartOperation>EndOperation then Exit;
  acc := Bank.SafeBox.Account(account_number);
  if (acc.updated_block>0) Or (acc.account=0) then Begin
    if (SearchBackwardsStartingAtBlock=0) Or (SearchBackwardsStartingAtBlock>=acc.updated_block) then begin
      startBlock := acc.updated_block;
      lastBalance := acc.balance;
    end else begin
      startBlock := SearchBackwardsStartingAtBlock;
      lastBalance := -1;
    end;
    DoGetFromBlock(startBlock,lastBalance,MaxDepth,0,startBlock<>acc.updated_block);
  end;
end;

procedure TNode.GetStoredOperationsFromAccount(const OperationsResume: TOperationsResumeList; account_number: Cardinal; MaxDepth, StartOperation, EndOperation: Integer; SearchBackwardsStartingAtBlock : Cardinal = 0);
var LOpList : TList<TOperationResume>;
  i : Integer;
begin
  LOpList := TList<TOperationResume>.Create;
  try
    GetStoredOperationsFromAccount(Nil,LOpList,account_number,MaxDepth,StartOperation,EndOperation,SearchBackwardsStartingAtBlock);
    for i := 0 to LOpList.Count-1 do begin
      OperationsResume.Add(LOpList[i]);
    end;
  finally
    LOpList.Free;
  end;
end;

function TNode.FindNOperation(block, account, n_operation: Cardinal;
  var OpResume: TOperationResume): TSearchOperationResult;
  // Note: block = 0 search in all blocks. If Block>0 must match a valid block with operation with this account
var oprl : TOperationsResumeList;
begin
  oprl := TOperationsResumeList.Create;
  try
    Result := FindNOperations(account,block,block=0,n_operation,n_operation,oprl);
    If oprl.Count>0 then begin
      OpResume := oprl.OperationResume[0];
    end else OpResume := CT_TOperationResume_NUL;
  finally
    oprl.Free;
  end;
end;

function TNode.FindNOperations(account, start_block : Cardinal; allow_search_previous : Boolean; n_operation_low, n_operation_high: Cardinal; OpResumeList: TOperationsResumeList): TSearchOperationResult;
var i : Integer;
  op : TPCOperation;
  aux_block, block : Cardinal;
  OperationComp : TPCOperationsComp;
  opr : TOperationResume;
  n_operation, found_n_operation : Cardinal;
begin
  OpResumeList.Clear;
  Result := invalid_params;
  block := start_block;
  If (block>=Bank.BlocksCount) then exit; // Invalid block number
  If (account>=Bank.AccountsCount) then exit; // Invalid account number
  If (n_operation_high<n_operation_low) then exit;
  n_operation := Operations.SafeBoxTransaction.Account(account).n_operation;
  if (n_operation>n_operation_high) then n_operation := n_operation_high;
  if (n_operation<n_operation_low) then Exit;
  If (block=0) then begin
    // Start searching on pending blocks
    Operations.Lock;
    Try
      For i:=Operations.Count-1 downto 0 do begin
        op := Operations.Operation[i];
        If (op.IsSignerAccount(account)) then begin
          found_n_operation := op.GetAccountN_Operation(account);
          if (found_n_operation<n_operation_low) then Exit; // Not found
          If (found_n_operation<=n_operation) then begin
            TPCOperation.OperationToOperationResume(0,op,False,account,opr);
            opr.Balance:=-1;
            OpResumeList.Add(opr);
            if (n_operation>n_operation_low) then dec(n_operation)
            else begin
              Result := found;
              Exit;
            end;
          end;
        end;
      end;
      block := Bank.SafeBox.Account(account).updated_block;
    finally
      Operations.Unlock;
    end;
  end;
  // Search in previous blocks
  OperationComp := TPCOperationsComp.Create(Nil);
  Try
    While (n_operation>0) And (n_operation>=n_operation_low) And (block>0) do begin
      aux_block := block;
      If Not Bank.LoadOperations(OperationComp,block) then begin
        Result := blockchain_block_not_found; // Cannot continue searching!
        exit;
      end;
      For i:=OperationComp.Count-1 downto 0 do begin
        op := OperationComp.Operation[i];
        if (op.IsSignerAccount(account)) then begin
          If (n_operation_high=n_operation_low) and (op.GetAccountN_Operation(account)=n_operation) // If searching only 1 n_operation, n_operation must match
            Or
            (n_operation_high>n_operation_low) and (op.GetAccountN_Operation(account)<=n_operation) and (op.GetAccountN_Operation(account)>=n_operation_low) and (op.GetAccountN_Operation(account)<=n_operation_high) then begin
            TPCOperation.OperationToOperationResume(block,op,True,account,opr);
            opr.time:=Bank.SafeBox.Block(block).blockchainInfo.timestamp;
            opr.NOpInsideBlock:=i;
            opr.Balance:=-1;
            OpResumeList.Add(opr);
            if (n_operation>n_operation_low) then dec(n_operation)
            else begin
              Result := found;
              Exit;
            end;
          end else begin
            If (op.GetAccountN_Operation(account) < n_operation) then begin
              If (n_operation_high>n_operation_low) then Result := found; // multiple search, result is found (not an error)
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
  Result := found;
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
    TLog.NewLog(ltInfo,ClassName,Format('Pending buffer restored operations:%d added:%d final_operations:%d errors:%s',[opht.OperationsCount,n,Operations.OperationsHashTree.OperationsCount,errors]));
  finally
    opht.Free;
    oprl.Free;
  end;
end;

function TNode.FindOperationExt(const OperationComp: TPCOperationsComp;
  const OperationHash: TRawBytes; var block: Cardinal;
  var operation_block_index: Integer): TSearchOperationResult;
{ With a OperationHash, search it }
var account,n_operation : Cardinal;
  i : Integer;
  op : TPCOperation;
  initial_block, aux_block, aux_n_op : Cardinal;
  opHashValid, opHash_OLD : TRawBytes;
  md160 : TRawBytes;
begin
  Result := invalid_params;
  // Decode OperationHash
  If not TPCOperation.DecodeOperationHash(OperationHash,block,account,n_operation,md160) then exit;
  initial_block := block;
  //
  If (account>=Bank.AccountsCount) then exit; // Invalid account number
  // If block=0 then we must search in pending operations first
  if (block=0) then begin
    FOperations.Lock;
    Try
      For i:=0 to FOperations.Count-1 do begin
        op := FOperations.Operation[i];
        If (op.SignerAccount=account) then begin
          opHashValid := TPCOperation.OperationHashValid(op,0);
          opHash_OLD := TPCOperation.OperationHash_OLD(op,0);
          If (opHashValid=OperationHash) or
            ((FBank.BlocksCount<CT_Protocol_Upgrade_v2_MinBlock) And (opHash_OLD=OperationHash)) then begin
            operation_block_index:=i;
            OperationComp.CopyFrom(FOperations);
            Result := found;
            exit;
          end;
        end;
      end;
    finally
      FOperations.Unlock;
    end;
    // block=0 and not found... start searching at block updated by account updated_block
    block := Bank.SafeBox.Account(account).updated_block;
    if Bank.SafeBox.Account(account).n_operation<n_operation then exit; // n_operation is greater than found in safebox
  end;
  if (block=0) or (block>=Bank.BlocksCount) then exit;
  // Search in previous blocks
  While (block>0) do begin
    aux_block := block;
    If Not Bank.LoadOperations(OperationComp,block) then begin
      Result := blockchain_block_not_found;
      exit;
    end;
    For i:=OperationComp.Count-1 downto 0 do begin
      op := OperationComp.Operation[i];
      if (op.IsSignerAccount(account)) then begin
        aux_n_op := op.GetAccountN_Operation(account);
        If (aux_n_op<n_operation) then exit; // n_operation is greaten than found
        If (aux_n_op=n_operation) then begin
          // Possible candidate or dead
          opHashValid := TPCOperation.OperationHashValid(op,initial_block);
          If (opHashValid=OperationHash) then begin
            operation_block_index:=i;
            Result := found;
            exit;
          end else if (block<CT_Protocol_Upgrade_v2_MinBlock) then begin
            opHash_OLD := TPCOperation.OperationHash_OLD(op,initial_block);
            if (opHash_OLD=OperationHash) then begin
              operation_block_index:=i;
              Result := found;
              exit;
            end else exit; // Not found!
          end else exit; // Not found!
        end;
      end;
    end;
    block := OperationComp.PreviousUpdatedBlocks.GetPreviousUpdatedBlock(account,block);
    if (block>=aux_block) then exit; // Error... not found a valid block positioning
    if (initial_block<>0) then exit; // If not found in specified block, no valid hash
  end;
end;

function TNode.FindOperation(const OperationComp: TPCOperationsComp;
  const OperationHash: TRawBytes; var block: Cardinal;
  var operation_block_index: Integer): Boolean;
  { With a OperationHash, search it }
var sor : TSearchOperationResult;
begin
  sor := FindOperationExt(OperationComp,OperationHash,block,operation_block_index);
  Result := sor = found;
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

procedure TNode.OnBankNewBlock(Sender: TObject);
begin
  FOperations.SanitizeOperations;
  NotifyBlocksChanged;
end;

function TNode.SendNodeMessage(Target: TNetConnection; const TheMessage: String; var errors: String): Boolean;
Var i,j : Integer;
  nc : TNetConnection;
  s : String;
begin
  Result := false;
  if Not TPCThread.TryProtectEnterCriticalSection(Self,4000,FLockNodeOperations) then begin
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
    FLockNodeOperations.Release;
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

initialization
  _Node := Nil;
finalization
  FreeAndNil(_Node);
end.
