unit UNode;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

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

interface

uses
  Classes, UBlockChain, UNetProtocol, UAccounts, UCrypto, UThread, SyncObjs, ULog;

Type

  { TNode }

  TNode = Class(TComponent)
  private
    FNodeLog : TLog;
    FLockNodeOperations : TPCCriticalSection;
    FNotifyList : TList;
    FBank : TPCBank;
    FOperations : TPCOperationsComp;
    FNetServer : TNetServer;
    FBCBankNotify : TPCBankNotify;
    FPeerCache : AnsiString;
    FDisabledsNewBlocksCount : Integer;
    Procedure OnBankNewBlock(Sender : TObject);
    procedure SetNodeLogFilename(const Value: AnsiString);
    function GetNodeLogFilename: AnsiString;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    Class Function Node : TNode;
    Class Procedure DecodeIpStringToNodeServerAddressArray(Const Ips : AnsiString; Var NodeServerAddressArray : TNodeServerAddressArray);
    Class Function EncodeNodeServerAddressArrayToIpString(Const NodeServerAddressArray : TNodeServerAddressArray) : AnsiString;
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Property Bank : TPCBank read FBank;
    Function NetServer : TNetServer;
    Procedure NotifyNetClientMessage(Sender : TNetConnection; Const TheMessage : AnsiString);
    //
    Property Operations : TPCOperationsComp read FOperations;
    //
    Function AddNewBlockChain(SenderConnection : TNetConnection; NewBlockOperations: TPCOperationsComp; var newBlockAccount: TBlockAccount; var errors: AnsiString): Boolean;
    Function AddOperations(SenderConnection : TNetConnection; Operations : TOperationsHashTree; OperationsResult : TOperationsResumeList; var errors: AnsiString): Integer;
    Function AddOperation(SenderConnection : TNetConnection; Operation : TPCOperation; var errors: AnsiString): Boolean;
    Function SendNodeMessage(Target : TNetConnection; TheMessage : AnsiString; var errors : AnsiString) : Boolean;
    //
    Procedure NotifyBlocksChanged;
    //
    procedure GetStoredOperationsFromAccount(const OperationsResume: TOperationsResumeList; account_number: Cardinal; MaxDepth, MaxOperations : Integer);
    Function FindOperation(Const OperationComp : TPCOperationsComp; Const OperationHash : TRawBytes; var block : Cardinal; var operation_block_index : Integer) : Boolean;
    //
    Procedure AutoDiscoverNodes(Const ips : AnsiString);
    Function IsBlockChainValid(var WhyNot : AnsiString) : Boolean;
    Function IsReady(Var CurrentProcess : AnsiString) : Boolean;
    Property PeerCache : AnsiString read FPeerCache write FPeerCache;
    Procedure DisableNewBlocks;
    Procedure EnableNewBlocks;
    Property NodeLogFilename : AnsiString read GetNodeLogFilename write SetNodeLogFilename;
  End;

  TNodeNotifyEvents = Class;

  TThreadSafeNodeNotifyEvent = Class(TPCThread)
    FNodeNotifyEvents : TNodeNotifyEvents;
    FNotifyBlocksChanged : Boolean;
    FNotifyOperationsChanged : Boolean;
    Procedure SynchronizedProcess;
  protected
    procedure BCExecute; override;
    Constructor Create(ANodeNotifyEvents : TNodeNotifyEvents);
  End;

  TNodeMessageEvent = Procedure(NetConnection : TNetConnection; MessageData : TRawBytes) of object;
  { TNodeNotifyEvents is ThreadSafe and will only notify in the main thread }
  TNodeNotifyEvents = Class(TComponent)
  private
    FNode: TNode;
    FPendingNotificationsList : TPCThreadList;
    FThreadSafeNodeNotifyEvent : TThreadSafeNodeNotifyEvent;
    FOnBlocksChanged: TNotifyEvent;
    FOnOperationsChanged: TNotifyEvent;
    FMessages : TStringList;
    FOnNodeMessageEvent: TNodeMessageEvent;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetNode(const Value: TNode);
    Procedure NotifyBlocksChanged;
    Procedure NotifyOperationsChanged;
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Property Node : TNode read FNode write SetNode;
    Property OnBlocksChanged : TNotifyEvent read FOnBlocksChanged write FOnBlocksChanged;
    Property OnOperationsChanged : TNotifyEvent read FOnOperationsChanged write FOnOperationsChanged;
    Property OnNodeMessageEvent : TNodeMessageEvent read FOnNodeMessageEvent write FOnNodeMessageEvent;
  End;

  TThreadNodeNotifyNewBlock = Class(TPCThread)
    FNetConnection : TNetConnection;
  protected
    procedure BCExecute; override;
    Constructor Create(NetConnection : TNetConnection);
  End;

  TThreadNodeNotifyOperations = Class(TPCThread)
    FNetConnection : TNetConnection;
    FOperationsHashTree : TOperationsHashTree;
  protected
    procedure BCExecute; override;
    Constructor Create(NetConnection : TNetConnection; MakeACopyOfOperationsHashTree : TOperationsHashTree);
    destructor Destroy; override;
  End;

implementation

Uses UOpTransaction, SysUtils,  UConst, UTime;

var _Node : TNode;

{ TNode }

function TNode.AddNewBlockChain(SenderConnection: TNetConnection; NewBlockOperations: TPCOperationsComp;
  var newBlockAccount: TBlockAccount; var errors: AnsiString): Boolean;
Var i,j : Integer;
  nc : TNetConnection;
  ms : TMemoryStream;
  s : String;
  errors2 : AnsiString;
  OpBlock : TOperationBlock;
begin
  Result := false;
  if FDisabledsNewBlocksCount>0 then begin
    TLog.NewLog(ltinfo,Classname,Format('Cannot Add new BlockChain due is adding disabled - Connection:%s NewBlock:%s',[
    Inttohex(PtrInt(SenderConnection),8),TPCOperationsComp.OperationBlockToText(NewBlockOperations.OperationBlock)]));
    exit;
  end;
  If NewBlockOperations.OperationBlock.block<>Bank.BlocksCount then exit;
  OpBlock := NewBlockOperations.OperationBlock;
  TLog.NewLog(ltdebug,Classname,Format('AddNewBlockChain Connection:%s NewBlock:%s',[
    Inttohex(PtrInt(SenderConnection),8),TPCOperationsComp.OperationBlockToText(OpBlock)]));
  If Not TPCThread.TryProtectEnterCriticalSection(Self,2000,FLockNodeOperations) then begin
    If NewBlockOperations.OperationBlock.block<>Bank.BlocksCount then exit;
    s := 'Cannot AddNewBlockChain due blocking lock operations node';
    TLog.NewLog(lterror,Classname,s);
    if TThread.CurrentThread.ThreadID=MainThreadID then raise Exception.Create(s) else exit;
  end;
  try
    ms := TMemoryStream.Create;
    try
      FOperations.SaveBlockToStream(false,ms);
      Result := Bank.AddNewBlockChainBlock(NewBlockOperations,newBlockAccount,errors);
      if Result then begin
        if Assigned(SenderConnection) then begin
          FNodeLog.NotifyNewLog(ltupdate,SenderConnection.ClassName,Format(';%d;%s;%s;;%d;%d;%d',[OpBlock.block,SenderConnection.ClientRemoteAddr,OpBlock.block_payload,
            OpBlock.timestamp,UnivDateTimeToUnix(DateTime2UnivDateTime(Now)),UnivDateTimeToUnix(DateTime2UnivDateTime(Now)) - OpBlock.timestamp]));
        end else begin
          FNodeLog.NotifyNewLog(ltupdate,ClassName,Format(';%d;%s;%s;;%d;%d;%d',[OpBlock.block,'NIL',OpBlock.block_payload,
            OpBlock.timestamp,UnivDateTimeToUnix(DateTime2UnivDateTime(Now)),UnivDateTimeToUnix(DateTime2UnivDateTime(Now)) - OpBlock.timestamp]));
        end;
      end else begin
        if Assigned(SenderConnection) then begin
          FNodeLog.NotifyNewLog(lterror,SenderConnection.ClassName,Format(';%d;%s;%s;%s;%d;%d;%d',[OpBlock.block,SenderConnection.ClientRemoteAddr,OpBlock.block_payload,errors,
            OpBlock.timestamp,UnivDateTimeToUnix(DateTime2UnivDateTime(Now)),UnivDateTimeToUnix(DateTime2UnivDateTime(Now)) - OpBlock.timestamp]));
        end else begin
          FNodeLog.NotifyNewLog(lterror,ClassName,Format(';%d;%s;%s;%s;%d;%d;%d',[OpBlock.block,'NIL',OpBlock.block_payload,errors,
            OpBlock.timestamp,UnivDateTimeToUnix(DateTime2UnivDateTime(Now)),UnivDateTimeToUnix(DateTime2UnivDateTime(Now)) - OpBlock.timestamp]));
        end;
      end;
      FOperations.Clear(true);
      ms.Position:=0;
      If Not FOperations.LoadBlockFromStream(ms,errors2) then begin
        TLog.NewLog(lterror,Classname,'Error recovering operations to sanitize: '+errors2);
        if Result then errors := errors2
        else errors := errors +' - '+errors2;
      end;
    finally
      ms.Free;
    end;
    FOperations.SanitizeOperations;
  finally
    FLockNodeOperations.Release;
    TLog.NewLog(ltdebug,Classname,Format('Finalizing AddNewBlockChain Connection:%s NewBlock:%s',[
      Inttohex(PtrInt(SenderConnection),8),TPCOperationsComp.OperationBlockToText(OpBlock) ]));
  End;
  if Result then begin
    // Notify to clients
    j := TNetData.NetData.ConnectionsCountAll;
    for i:=0 to j-1 do begin
      if (TNetData.NetData.GetConnection(i,nc)) then begin
        if (nc<>SenderConnection) And (nc.Connected) then TThreadNodeNotifyNewBlock.Create(nc);
      end;
    end;
    // Notify it!
    NotifyBlocksChanged;
  end;
end;

function TNode.AddOperation(SenderConnection : TNetConnection; Operation: TPCOperation; var errors: AnsiString): Boolean;
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

function TNode.AddOperations(SenderConnection : TNetConnection; Operations : TOperationsHashTree; OperationsResult : TOperationsResumeList; var errors: AnsiString): Integer;
Var
  i,j : Integer;
  operationscomp : TPCOperationsComp;
  valids_operations : TOperationsHashTree;
  nc : TNetConnection;
  e : AnsiString;
  mtl : TList;
  s : String;
  OPR : TOperationResume;
  ActOp : TPCOperation;
begin
  Result := -1;
  if Assigned(OperationsResult) then OperationsResult.Clear;
  if FDisabledsNewBlocksCount>0 then begin
    errors := Format('Cannot Add Operations due is adding disabled - OpCount:%d',[Operations.OperationsCount]);
    TLog.NewLog(ltinfo,Classname,errors);
    exit;
  end;
  Result := 0;
  errors := '';
  valids_operations := TOperationsHashTree.Create;
  try
    TLog.NewLog(ltdebug,Classname,Format('AddOperations Connection:%s Operations:%d',[
      Inttohex(PtrInt(SenderConnection),8),Operations.OperationsCount]));
    if Not TPCThread.TryProtectEnterCriticalSection(Self,4000,FLockNodeOperations) then begin
      s := 'Cannot AddOperations due blocking lock operations node';
      TLog.NewLog(lterror,Classname,s);
      if TThread.CurrentThread.ThreadID=MainThreadID then raise Exception.Create(s) else exit;
    end;
    try
      for j := 0 to Operations.OperationsCount-1 do begin
        ActOp := Operations.GetOperation(j);
        If FOperations.OperationsHashTree.IndexOfOperation(ActOp)<0 then begin
          if (FOperations.AddOperation(true,ActOp,e)) then begin
            inc(Result);
            valids_operations.AddOperationToHashTree(ActOp);
            TLog.NewLog(ltdebug,Classname,Format('AddOperation %d/%d: %s',[(j+1),Operations.OperationsCount,ActOp.ToString]));
            if Assigned(OperationsResult) then begin
              TPCOperation.OperationToOperationResume(0,ActOp,ActOp.SenderAccount,OPR);
              OPR.NOpInsideBlock:=FOperations.Count-1;
              OPR.Balance := FOperations.SafeBoxTransaction.Account(ActOp.SenderAccount).balance;
              OperationsResult.Add(OPR);
            end;
          end else begin
            if (errors<>'') then errors := errors+' ';
            errors := errors+'Op '+IntToStr(j+1)+'/'+IntToStr(Operations.OperationsCount)+':'+e;
            TLog.NewLog(ltdebug,Classname,Format('AddOperation invalid/duplicated %d/%d: %s  - Error:%s',
              [(j+1),Operations.OperationsCount,ActOp.ToString,e]));
            if Assigned(OperationsResult) then begin
              TPCOperation.OperationToOperationResume(0,ActOp,ActOp.SenderAccount,OPR);
              OPR.valid := false;
              OPR.NOpInsideBlock:=-1;
              OPR.OperationHash := '';
              OPR.errors := e;
              OperationsResult.Add(OPR);
            end;
          end;
        end else begin
          // XXXXX DEBUG ONLY
          // TLog.NewLog(ltdebug,Classname,Format('AddOperation made before %d/%d: %s',[(j+1),Operations.OperationsCount,ActOp.ToString]));
        end;
      end;
    finally
      FLockNodeOperations.Release;
      if Result<>0 then begin
        TLog.NewLog(ltdebug,Classname,Format('Finalizing AddOperations Connection:%s Operations:%d valids:%d',[
          Inttohex(PtrInt(SenderConnection),8),Operations.OperationsCount,Result ]));
      end;
    end;
    if Result=0 then exit;
    // Send to other nodes
    j := TNetData.NetData.ConnectionsCountAll;
    for i:=0 to j-1 do begin
      If TNetData.NetData.GetConnection(i,nc) then begin
        if (nc<>SenderConnection) And (nc.Connected) then TThreadNodeNotifyOperations.Create(nc,valids_operations);
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

procedure TNode.AutoDiscoverNodes(const ips: AnsiString);
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
  FNodeLog := TLog.Create(Self);
  FNodeLog.ProcessGlobalLogs := false;
  RegisterOperationsClass;
  if Assigned(_Node) then raise Exception.Create('Duplicate nodes protection');
  TLog.NewLog(ltInfo,ClassName,'TNode.Create');
  inherited;
  FDisabledsNewBlocksCount := 0;
  FLockNodeOperations := TPCCriticalSection.Create('TNode_LockNodeOperations');
  FBank := TPCBank.Create(Self);
  FBCBankNotify := TPCBankNotify.Create(Self);
  FBCBankNotify.Bank := FBank;
  FBCBankNotify.OnNewBlock := OnBankNewBlock;
  FNetServer := TNetServer.Create;
  FOperations := TPCOperationsComp.Create(Self);
  FOperations.bank := FBank;
  FNotifyList := TList.Create;
  if Not Assigned(_Node) then _Node := Self;
end;

class procedure TNode.DecodeIpStringToNodeServerAddressArray(
  const Ips: AnsiString; var NodeServerAddressArray: TNodeServerAddressArray);
  Function GetIp(var ips_string : AnsiString; var nsa : TNodeServerAddress) : Boolean;
  Const CT_IP_CHARS = ['a'..'z','A'..'Z','0'..'9','.','-','_'];
  var i : Integer;
    port : AnsiString;
  begin
    nsa := CT_TNodeServerAddress_NUL;
    Result := false;
    if length(trim(ips_string))=0 then begin
      ips_string := '';
      exit;
    end;
    i := 1;
    while (i<length(ips_string)) AND (NOT (ips_string[i] IN CT_IP_CHARS)) do inc(i);
    if (i>1) then ips_string := copy(ips_string,i,length(ips_string));
    //
    i := 1;
    while (i<=length(ips_string)) and (ips_string[i] in CT_IP_CHARS) do inc(i);
    nsa.ip := copy(ips_string,1,i-1);
    if (i<=length(ips_string)) and (ips_string[i]=':') then begin
      inc(i);
      port := '';
      while (i<=length(ips_string)) and (ips_string[i] in ['0'..'9']) do begin
        port := port + ips_string[i];
        inc(i);
      end;
      nsa.port := StrToIntDef(port,0);
    end;
    ips_string := copy(ips_string,i+1,length(ips_string));
    if nsa.port=0 then nsa.port := CT_NetServer_Port;
    Result := (trim(nsa.ip)<>'');
  end;
Var i,j : Integer;
  ips_string : AnsiString;
  nsa : TNodeServerAddress;
begin
  SetLength(NodeServerAddressArray,0);
  ips_string := Ips;
  repeat
    If GetIp(ips_string,nsa) then begin
      SetLength(NodeServerAddressArray,length(NodeServerAddressArray)+1);
      NodeServerAddressArray[High(NodeServerAddressArray)] := nsa;
    end;
  until (ips_string='');
end;

destructor TNode.Destroy;
Var step : String;
begin
  TLog.NewLog(ltInfo,ClassName,'TNode.Destroy START');
  Try
    step := 'Deleting critical section';
    FreeAndNil(FLockNodeOperations);

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

    step := 'Destroying Bank';
    FreeAndNil(FBCBankNotify);
    FreeAndNil(FBank);

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

class function TNode.EncodeNodeServerAddressArrayToIpString(
  const NodeServerAddressArray: TNodeServerAddressArray): AnsiString;
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

function TNode.GetNodeLogFilename: AnsiString;
begin
  Result := FNodeLog.FileName;
end;

function TNode.IsBlockChainValid(var WhyNot : AnsiString): Boolean;
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

function TNode.IsReady(var CurrentProcess: AnsiString): Boolean;
begin
  Result := false;
  CurrentProcess := '';
  if FBank.IsReady(CurrentProcess) then begin
    if FNetServer.Active then begin
      if TNetData.NetData.IsGettingNewBlockChainFromClient then begin
        CurrentProcess := 'Obtaining valid BlockChain - Found block '+inttostr(TNetData.NetData.MaxRemoteOperationBlock.block);
      end else begin
        if TNetData.NetData.MaxRemoteOperationBlock.block>FOperations.OperationBlock.block then begin
          CurrentProcess := 'Found block '+inttostr(TNetData.NetData.MaxRemoteOperationBlock.block)+' (Wait until downloaded)';
        end else begin
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

procedure TNode.GetStoredOperationsFromAccount(const OperationsResume: TOperationsResumeList; account_number: Cardinal; MaxDepth, MaxOperations: Integer);
  Procedure DoGetFromBlock(block_number : Cardinal; last_balance : Int64; act_depth : Integer);
  var opc : TPCOperationsComp;
    op : TPCOperation;
    OPR : TOperationResume;
    l : TList;
    i : Integer;
    next_block_number : Cardinal;
  begin
    if (act_depth<=0) Or ((block_number<=0) And (block_number>0)) then exit;

    opc := TPCOperationsComp.Create(Nil);
    Try
      If not Bank.Storage.LoadBlockChainBlock(opc,block_number) then begin
        TLog.NewLog(lterror,ClassName,'Error searching for block '+inttostr(block_number));
        exit;
      end;
      l := TList.Create;
      try
        next_block_number := 0;
        opc.OperationsHashTree.GetOperationsAffectingAccount(account_number,l);
        for i := l.Count - 1 downto 0 do begin
          op := opc.Operation[PtrInt(l.Items[i])];
          if (i=0) then begin
            If op.SenderAccount=account_number then next_block_number := op.Previous_Sender_updated_block
            else next_block_number := op.Previous_Destination_updated_block;
          end;
          If TPCOperation.OperationToOperationResume(block_number,Op,account_number,OPR) then begin
            OPR.NOpInsideBlock := Op.tag; // Note: Used Op.tag to include operation index inside a list
            OPR.time := opc.OperationBlock.timestamp;
            OPR.Block := block_number;
            OPR.Balance := last_balance;
            last_balance := last_balance - ( OPR.Amount + OPR.Fee );
            OperationsResume.Add(OPR);
          end;
        end;
        // Is a new block operation?
        if (TAccountComp.AccountBlock(account_number)=block_number) And ((account_number MOD CT_AccountsPerBlock)=0) then begin
          OPR := CT_TOperationResume_NUL;
          OPR.valid := true;
          OPR.Block := block_number;
          OPR.time := opc.OperationBlock.timestamp;
          OPR.AffectedAccount := account_number;
          OPR.Amount := opc.OperationBlock.reward;
          OPR.Fee := opc.OperationBlock.fee;
          OPR.Balance := last_balance;
          OPR.OperationTxt := 'Blockchain reward';
          OperationsResume.Add(OPR);
        end;
        //
        opc.Clear(true);
        if (next_block_number>=0) And (next_block_number<block_number) And (act_depth>0)
           And (next_block_number >= (account_number DIV CT_AccountsPerBlock))
           And ((OperationsResume.Count<MaxOperations) Or (MaxOperations<=0))
           then DoGetFromBlock(next_block_number,last_balance,act_depth-1);
      finally
        l.Free;
      end;
    Finally
      opc.Free;
    End;
  end;

Var acc : TAccount;
begin
  if MaxDepth<0 then exit;
  if account_number>=Bank.SafeBox.AccountsCount then exit;
  acc := Bank.SafeBox.Account(account_number);
  if (acc.updated_block>0) Or (acc.account=0) then DoGetFromBlock(acc.updated_block,acc.balance,MaxDepth);
end;

function TNode.FindOperation(const OperationComp: TPCOperationsComp;
  const OperationHash: TRawBytes; var block: Cardinal;
  var operation_block_index: Integer): Boolean;
  { With a OperationHash, search it }
var account,n_operation : Cardinal;
  i : Integer;
  op : TPCOperation;
  initial_block, aux_block : Cardinal;
begin
  Result := False;
  // Decode OperationHash
  If not TPCOperation.DecodeOperationHash(OperationHash,block,account,n_operation) then exit;
  initial_block := block;
  //
  If (account>=Bank.AccountsCount) then exit; // Invalid account number
  // If block=0 then we must search in pending operations first
  if (block=0) then begin
    FOperations.Lock;
    Try
      For i:=0 to FOperations.Count-1 do begin
        If (FOperations.Operation[i].SenderAccount=account) then begin
          If (TPCOperation.OperationHash(FOperations.Operation[i],0)=OperationHash) then begin
            operation_block_index:=i;
            OperationComp.CopyFrom(FOperations);
            Result := true;
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
  While (Not Result) And (block>0) do begin
    aux_block := block;
    If Not Bank.LoadOperations(OperationComp,block) then exit;
    For i:=OperationComp.Count-1 downto 0 do begin
      op := OperationComp.Operation[i];
      if (op.SenderAccount=account) then begin
        If (op.N_Operation<n_operation) then exit; // n_operation is greaten than found
        If (op.N_Operation=n_operation) then begin
          // Possible candidate or dead
          If TPCOperation.OperationHash(op,initial_block)=OperationHash then begin
            operation_block_index:=i;
            Result := true;
            exit;
          end else exit; // Not found!
        end;
        If op.Previous_Sender_updated_block>block then exit;
        block := op.Previous_Sender_updated_block;
      end else if op.IsDestinationAccount(account) then begin
        If op.Previous_Destination_updated_block > block then exit;
        block := op.Previous_Destination_updated_block;
      end;
    end;
    if (block>=aux_block) then exit; // Error... not found a valid block positioning
    if (initial_block<>0) then exit; // If not found in specified block, no valid hash
  end;
end;

procedure TNode.NotifyNetClientMessage(Sender: TNetConnection; const TheMessage: AnsiString);
Var i : Integer;
  s : AnsiString;
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
end;

function TNode.SendNodeMessage(Target: TNetConnection; TheMessage: AnsiString; var errors: AnsiString): Boolean;
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

procedure TNode.SetNodeLogFilename(const Value: AnsiString);
begin
  FNodeLog.FileName := Value;
end;

{ TNodeNotifyEvents }

constructor TNodeNotifyEvents.Create(AOwner: TComponent);
begin
  inherited;
  FOnOperationsChanged := Nil;
  FOnBlocksChanged := Nil;
  FOnNodeMessageEvent := Nil;
  FMessages := TStringList.Create;
  FPendingNotificationsList := TPCThreadList.Create('TNodeNotifyEvents_PendingNotificationsList');
  FThreadSafeNodeNotifyEvent := TThreadSafeNodeNotifyEvent.Create(Self);
  FThreadSafeNodeNotifyEvent.FreeOnTerminate := true; // This is to prevent locking when freeing component
  Node := _Node;
end;

destructor TNodeNotifyEvents.Destroy;
begin
  if Assigned(FNode) then FNode.FNotifyList.Remove(Self);
  FThreadSafeNodeNotifyEvent.FNodeNotifyEvents := Nil;
  FThreadSafeNodeNotifyEvent.Terminate;
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

procedure TNodeNotifyEvents.SetNode(const Value: TNode);
begin
  if FNode=Value then exit;
  if Assigned(FNode) then begin
    FNode.RemoveFreeNotification(Self);
    FNode.FNotifyList.Add(Self);
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
  Inherited Create(false);
end;

procedure TThreadSafeNodeNotifyEvent.SynchronizedProcess;
Var i : Integer;
begin
  Try
    If (Terminated) Or (Not Assigned(FNodeNotifyEvents)) then exit;
    if FNotifyBlocksChanged then begin
      FNotifyBlocksChanged := false;
      DebugStep:='Notify OnBlocksChanged';
      if Assigned(FNodeNotifyEvents) And (Assigned(FNodeNotifyEvents.FOnBlocksChanged)) then
        FNodeNotifyEvents.FOnBlocksChanged(FNodeNotifyEvents);
    end;
    if FNotifyOperationsChanged then begin
      FNotifyOperationsChanged := false;
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
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,ClassName,'Exception inside a Synchronized process: '+E.ClassName+':'+E.Message+' Step:'+DebugStep);
    end;
  End;
end;

{ TThreadNodeNotifyNewBlock }

procedure TThreadNodeNotifyNewBlock.BCExecute;
begin
  if TNetData.NetData.ConnectionLock(Self,FNetConnection,500) then begin
    try
      if Not FNetconnection.Connected then exit;
      TLog.NewLog(ltdebug,ClassName,'Sending new block found to '+FNetConnection.Client.ClientRemoteAddr);
      FNetConnection.Send_NewBlockFound;
      if TNode.Node.Operations.OperationsHashTree.OperationsCount>0 then begin
         TLog.NewLog(ltdebug,ClassName,'Sending '+inttostr(TNode.Node.Operations.OperationsHashTree.OperationsCount)+' sanitized operations to '+FNetConnection.ClientRemoteAddr);
         FNetConnection.Send_AddOperations(TNode.Node.Operations.OperationsHashTree);
      end;
    finally
      TNetData.NetData.ConnectionUnlock(FNetConnection);
    end;
  end;
end;

constructor TThreadNodeNotifyNewBlock.Create(NetConnection: TNetConnection);
begin
  FNetConnection := NetConnection;
  Inherited Create(false);
  FreeOnTerminate := true;
end;

{ TThreadNodeNotifyOperations }

procedure TThreadNodeNotifyOperations.BCExecute;
Var nOpsCount : Integer;
begin
  nOpsCount := FOperationsHashTree.OperationsCount;
  if nOpsCount<=0 then exit;
  if TNetData.NetData.ConnectionLock(Self, FNetConnection, 500) then begin
    try
      if Not FNetconnection.Connected then exit;
      nOpsCount := FNetConnection.AddOperationsToBufferForSend(FOperationsHashTree);
    finally
      TNetData.NetData.ConnectionUnlock(FNetConnection);
    end;
  end;
  if nOpsCount<=0 then exit;
  Sleep(Random(5000)); // Delay 0..5 seconds to allow receive data and don't send if not necessary
  if TNetData.NetData.ConnectionLock(Self, FNetConnection, 500) then begin
    try
      if Not FNetconnection.Connected then exit;
      FNetConnection.Send_AddOperations(FOperationsHashTree);
    finally
      TNetData.NetData.ConnectionUnlock(FNetConnection);
    end;
  end;
end;

constructor TThreadNodeNotifyOperations.Create(NetConnection: TNetConnection; MakeACopyOfOperationsHashTree: TOperationsHashTree);
begin
  FOperationsHashTree := TOperationsHashTree.Create;
  FOperationsHashTree.CopyFromHashTree(MakeACopyOfOperationsHashTree);
  FNetConnection := NetConnection;
  Inherited Create(false);
  FreeOnTerminate := true;
end;

destructor TThreadNodeNotifyOperations.Destroy;
begin
  FreeAndNil(FOperationsHashTree);
  inherited;
end;

initialization
  _Node := Nil;
finalization
  FreeAndNil(_Node);
end.
