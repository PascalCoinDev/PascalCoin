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
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Classes, UBlockChain, UNetProtocol, UMiner, UAccounts, UCrypto, UThread, SyncObjs;

Type
  TNode = Class(TComponent)
  private
    FLockNodeOperations : TCriticalSection;
    FNotifyList : TList;
    FBank : TPCBank;
    FOperations : TPCOperationsComp;
    FNetServer : TNetServer;
    FMinerThreads : TPCThreadList;
    FBCBankNotify : TPCBankNotify;
    FPeerCache : AnsiString;
    FDisabledsNewBlocksCount : Integer;
    Procedure OnBankNewBlock(Sender : TObject);
    Procedure OnMinerThreadTerminate(Sender : TObject);
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
    Function MinersCount : Integer;
    Property MinerThreads : TPCThreadList read FMinerThreads;
    Function AddMiner(AccountKey : TAccountKey) : TMinerThread;
    Procedure DeleteMiner(index : Integer);
    Procedure NotifyNetClientMessage(Sender : TNetConnection; Const TheMessage : AnsiString);
    //
    Property Operations : TPCOperationsComp read FOperations;
    //
    Function AddNewBlockChain(SenderMiner : TMinerThread; SenderConnection : TNetConnection; NewBlockOperations: TPCOperationsComp; var newBlockAccount: TBlockAccount; var errors: AnsiString): Boolean;
    Function AddOperations(SenderConnection : TNetConnection; Operations : TOperationsHashTree; var errors: AnsiString): Integer;
    Function AddOperation(SenderConnection : TNetConnection; Operation : TPCOperation; var errors: AnsiString): Boolean;
    Function SendNodeMessage(Target : TNetConnection; TheMessage : AnsiString; var errors : AnsiString) : Boolean;
    //
    Procedure NotifyBlocksChanged;
    //
    Procedure AutoDiscoverNodes(Const ips : AnsiString);
    Function IsBlockChainValid(var WhyNot : AnsiString) : Boolean;
    Function IsReady(Var CurrentProcess : AnsiString) : Boolean;
    Property PeerCache : AnsiString read FPeerCache write FPeerCache;
    Procedure DisableNewBlocks;
    Procedure EnableNewBlocks;
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

Uses UOpTransaction, SysUtils, ULog, Forms, UConst, UTime;

var _Node : TNode;

{ TNode }

function TNode.AddMiner(AccountKey : TAccountKey) : TMinerThread;
Var op : TPCOperationsComp;
begin
  Result := Nil;
  TLog.NewLog(ltinfo,ClassName,'Creating a new miner');
  Result := TMinerThread.Create(Bank,AccountKey,nil,nil);
  Result.OnTerminate := OnMinerThreadTerminate;
  op := Result.MinerLockOperations;
  try
    op.CopyFromExceptAddressKey(FOperations);
  finally
    Result.MinerUnLockOperations(True);
  end;
  FMinerThreads.Add(Result);
end;

function TNode.AddNewBlockChain(SenderMiner: TMinerThread; SenderConnection: TNetConnection; NewBlockOperations: TPCOperationsComp;
  var newBlockAccount: TBlockAccount; var errors: AnsiString): Boolean;
Var i : Integer;
  operationscomp : TPCOperationsComp;
  nc : TNetConnection;
  ms : TMemoryStream;
  mtl : TList;
  netConnectionsList : TList;
  s : String;
begin
  Result := false;
  if FDisabledsNewBlocksCount>0 then begin
    TLog.NewLog(ltinfo,Classname,Format('Cannot Add new BlockChain due is adding disabled - Miner:%s Connection:%s NewBlock:%s',[Inttohex(Integer(SenderMiner),8),
    Inttohex(Integer(SenderConnection),8),TPCOperationsComp.OperationBlockToText(NewBlockOperations.OperationBlock)]));
    exit;
  end;
  TLog.NewLog(ltdebug,Classname,Format('AddNewBlockChain Miner:%s Connection:%s NewBlock:%s',[Inttohex(Integer(SenderMiner),8),
    Inttohex(Integer(SenderConnection),8),TPCOperationsComp.OperationBlockToText(NewBlockOperations.OperationBlock)]));
  If Not TPCThread.TryProtectEnterCriticalSection(Self,2000,FLockNodeOperations) then begin
    s := 'Cannot AddNewBlockChain due blocking lock operations node';
    TLog.NewLog(lterror,Classname,s);
    if TThread.CurrentThread.ThreadID=MainThreadID then raise Exception.Create(s) else exit;
  end;
  try
    ms := TMemoryStream.Create;
    try
      FOperations.SaveBlockToStream(false,ms);
      Result := Bank.AddNewBlockChainBlock(NewBlockOperations,newBlockAccount,errors);
      FOperations.Clear(true);
      ms.Position:=0;
      If Not FOperations.LoadBlockFromStream(ms,errors) then begin
        TLog.NewLog(lterror,Classname,'Error recovering operations to sanitize: '+errors);
      end;
    finally
      ms.Free;
    end;
    if Result then begin
      FOperations.SanitizeOperations;
      // Notify to all clients and other miners
      mtl := FMinerThreads.LockList;
      try
        for i := 0 to mtl.Count - 1 do begin
          if (mtl[i]<>SenderMiner) then begin
            TLog.NewLog(ltdebug,Classname,'Sending new Operations to miner '+inttostr(i+1)+'/'+inttostr(mtl.Count));
            operationscomp := TMinerThread(mtl[i]).MinerLockOperations;
            try
              operationscomp.CopyFromExceptAddressKey(FOperations);
            finally
              TMinerThread(mtl[i]).MinerUnLockOperations(true);
            end;
          end else begin
            //
          end;
        end;
      finally
        FMinerThreads.UnlockList;
      end;
      // Notify to clients
      netConnectionsList := TNetData.NetData.ConnectionsLock;
      Try
        for i:=0 to netConnectionsList.Count-1 do begin
          nc := netConnectionsList[i];
          if (SenderConnection<>nc) then begin
            TThreadNodeNotifyNewBlock.Create(nc);
          end;
        end;
      Finally
        TNetData.NetData.ConnectionsUnlock;
      End;
    end else begin
      // If error is on a SenderMiner its a hole
      FOperations.SanitizeOperations;
      if Assigned(SenderMiner) then begin
        TLog.NewLog(lterror,SenderMiner.Classname,'Invalid calculated PoW... reseting from Node Operations: '+TPCOperationsComp.OperationBlockToText(FOperations.OperationBlock));
        operationscomp := SenderMiner.MinerLockOperations;
        try
          operationscomp.CopyFromExceptAddressKey(FOperations);
        finally
          SenderMiner.MinerUnLockOperations(true);
        end;
        // Reset others:
        mtl := FMinerThreads.LockList;
        try
          for i := 0 to mtl.Count - 1 do begin
            if (TMinerThread(mtl[i])<>SenderMiner) then begin
              operationscomp := TMinerThread(mtl[i]).MinerLockOperations;
              try
                operationscomp.CopyFromExceptAddressKey(FOperations);
              finally
                TMinerThread(mtl[i]).MinerUnLockOperations(true);
              end;
            end;
          end;
        finally
          FMinerThreads.UnlockList;
        end;
      end;
    end;
  finally
    FLockNodeOperations.Release;
    TLog.NewLog(ltdebug,Classname,Format('Finalizing AddNewBlockChain Miner:%s Connection:%s NewBlock:%s',[Inttohex(Integer(SenderMiner),8),
      Inttohex(Integer(SenderConnection),8),TPCOperationsComp.OperationBlockToText(NewBlockOperations.OperationBlock) ]));
  End;
  if Result then begin
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
    Result := AddOperations(SenderConnection,ops,errors)=1;
  Finally
    ops.Free;
  End;
end;

function TNode.AddOperations(SenderConnection : TNetConnection; Operations : TOperationsHashTree; var errors: AnsiString): Integer;
Var
  i,j : Integer;
  operationscomp : TPCOperationsComp;
  valids_operations : TOperationsHashTree;
  nc : TNetConnection;
  e : AnsiString;
  mtl : TList;
  netConnectionsList : TList;
  s : String;
begin
  Result := -1;
  if FDisabledsNewBlocksCount>0 then begin
    errors := Format('Cannot Add Operations due is adding disabled - OpCount:%d',[Operations.OperationsCount]);
    TLog.NewLog(ltinfo,Classname,errors);
    exit;
  end;
  TLog.NewLog(ltdebug,Classname,Format('AddOperations Connection:%s Operations:%d',[
    Inttohex(Integer(SenderConnection),8),Operations.OperationsCount]));
  if Not TPCThread.TryProtectEnterCriticalSection(Self,4000,FLockNodeOperations) then begin
    s := 'Cannot AddOperations due blocking lock operations node';
    TLog.NewLog(lterror,Classname,s);
    if TThread.CurrentThread.ThreadID=MainThreadID then raise Exception.Create(s) else exit;
  end;
  try
    Result := 0;
    errors := '';
    valids_operations := TOperationsHashTree.Create;
    try
      for j := 0 to Operations.OperationsCount-1 do begin
        if (FOperations.AddOperation(true,Operations.GetOperation(j),e)) then begin
          inc(Result);
          valids_operations.AddOperationToHashTree(Operations.GetOperation(j));
          TLog.NewLog(ltdebug,Classname,Format('AddOperation %d/%d: %s',[(j+1),Operations.OperationsCount,Operations.GetOperation(j).ToString]));
        end else begin
          if (errors<>'') then errors := errors+' ';
          errors := errors+'Op '+IntToStr(j+1)+'/'+IntToStr(Operations.OperationsCount)+':'+e;
          TLog.NewLog(ltdebug,Classname,Format('AddOperation failed %d/%d: %s  - Error:%s',
            [(j+1),Operations.OperationsCount,Operations.GetOperation(j).ToString,e]));
        end;
      end;
      if Result=0 then exit;
      // Send to miners
      mtl := FMinerThreads.LockList;
      Try
        for i := 0 to mtl.Count - 1 do begin
          operationscomp := TMinerThread(mtl[i]).MinerLockOperations;
          try
            operationscomp.CopyFromExceptAddressKey(FOperations);
          finally
            TMinerThread(mtl[i]).MinerUnLockOperations(false);
          end;
        end;
      Finally
        FMinerThreads.UnlockList;
      End;
      // Send to other nodes
      netConnectionsList := TNetData.NetData.ConnectionsLock;
      Try
        for i:=0 to netConnectionsList.Count-1 do begin
          nc := netConnectionsList[i];
          if (nc<>SenderConnection) then begin
            TThreadNodeNotifyOperations.Create(nc,valids_operations);
          end;
        end;
      Finally
        TNetData.NetData.ConnectionsUnlock;
      End;
    finally
      valids_operations.Free;
    end;
  finally
    FLockNodeOperations.Release;
    TLog.NewLog(ltdebug,Classname,Format('Finalizing AddOperations Connection:%s Operations:%d',[
      Inttohex(Integer(SenderConnection),8),Operations.OperationsCount ]));
  end;
  // Notify it!
  for i := 0 to FNotifyList.Count-1 do begin
    TNodeNotifyEvents( FNotifyList[i] ).NotifyOperationsChanged;
  end;
end;

procedure TNode.AutoDiscoverNodes(Const ips : AnsiString);
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
  if Assigned(_Node) then raise Exception.Create('Duplicate nodes protection');
  TLog.NewLog(ltInfo,ClassName,'TNode.Create');
  inherited;
  FDisabledsNewBlocksCount := 0;
  FLockNodeOperations := TCriticalSection.Create;
  FBank := TPCBank.Create(Self);
  FBCBankNotify := TPCBankNotify.Create(Self);
  FBCBankNotify.Bank := FBank;
  FBCBankNotify.OnNewBlock := OnBankNewBlock;
  FNetServer := TNetServer.Create;
  FMinerThreads := TPCThreadList.Create;
  FOperations := TPCOperationsComp.Create(Self);
  FOperations.bank := FBank;
  FNotifyList := TList.Create;
  if Not Assigned(_Node) then _Node := Self;
end;

class procedure TNode.DecodeIpStringToNodeServerAddressArray(Const Ips: AnsiString;
  var NodeServerAddressArray: TNodeServerAddressArray);
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

procedure TNode.DeleteMiner(index: Integer);
Var m : TMinerThread;
  mtl : TList;
begin
  mtl := FMinerThreads.LockList;
  Try
    m := TMinerThread(mtl[index]);
    m.Suspended := false;
    m.Paused := false;
    mtl.Delete(index);
  Finally
    FMinerThreads.UnlockList;
  End;
  m.Terminate;
  m.WaitFor;
  m.Free;
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

    step := 'Deleting miners';
    while (MinersCount>0) do DeleteMiner(0);

    step := 'Destroying NetServer';
    FreeAndNil(FNetServer);
    step := 'Destroying MinerThreads';
    FreeAndNil(FMinerThreads);

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

function TNode.MinersCount : Integer;
Var mtl : TList;
begin
  mtl := FMinerThreads.LockList;
  Try
    Result := mtl.Count;
  Finally
    FMinerThreads.UnlockList;
  End;
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

procedure TNode.OnMinerThreadTerminate(Sender: TObject);
begin
  FMinerThreads.Remove(Sender);
end;

function TNode.SendNodeMessage(Target: TNetConnection; TheMessage: AnsiString; var errors: AnsiString): Boolean;
Var i : Integer;
  nc : TNetConnection;
  netConnectionsList : TList;
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
      netConnectionsList := TNetData.NetData.ConnectionsLock;
      Try
        for i:=0 to netConnectionsList.Count-1 do begin
          nc := netConnectionsList[i];
          nc.Send_Message(TheMessage);
        end;
      Finally
        TNetData.NetData.ConnectionsUnlock;
      End;
    end;
    result := true;
  finally
    FLockNodeOperations.Release;
  end;
end;

{ TNodeNotifyEvents }

constructor TNodeNotifyEvents.Create(AOwner: TComponent);
begin
  inherited;
  FOnOperationsChanged := Nil;
  FOnBlocksChanged := Nil;
  FOnNodeMessageEvent := Nil;
  FMessages := TStringList.Create;
  FPendingNotificationsList := TPCThreadList.Create;
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
  If (Terminated) Or (Not Assigned(FNodeNotifyEvents)) then exit;
  if FNotifyBlocksChanged then begin
    FNotifyBlocksChanged := false;
    if Assigned(FNodeNotifyEvents) And (Assigned(FNodeNotifyEvents.FOnBlocksChanged)) then
      FNodeNotifyEvents.FOnBlocksChanged(FNodeNotifyEvents);
  end;
  if FNotifyOperationsChanged then begin
    FNotifyOperationsChanged := false;
    if Assigned(FNodeNotifyEvents) And (Assigned(FNodeNotifyEvents.FOnOperationsChanged)) then
      FNodeNotifyEvents.FOnOperationsChanged(FNodeNotifyEvents);
  end;
  if FNodeNotifyEvents.FMessages.Count>0 then begin
    if Assigned(FNodeNotifyEvents) And (Assigned(FNodeNotifyEvents.FOnNodeMessageEvent)) then begin
      for i := 0 to FNodeNotifyEvents.FMessages.Count - 1 do begin
        FNodeNotifyEvents.FOnNodeMessageEvent(TNetConnection(FNodeNotifyEvents.FMessages.Objects[i]),FNodeNotifyEvents.FMessages.Strings[i]);
      end;
    end;
    FNodeNotifyEvents.FMessages.Clear;
  end;
end;

{ TThreadNodeNotifyNewBlock }

procedure TThreadNodeNotifyNewBlock.BCExecute;
begin
  if TNetData.NetData.ConnectionLock(Self,FNetConnection) then begin
    try
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
begin
  if TNetData.NetData.ConnectionLock(Self, FNetConnection) then begin
    try
      if FOperationsHashTree.OperationsCount<=0 then exit;
      TLog.NewLog(ltdebug,ClassName,'Sending '+inttostr(FOperationsHashTree.OperationsCount)+' Operations to '+FNetConnection.ClientRemoteAddr);
      FNetConnection.Send_AddOperations(FOperationsHashTree);
    finally
      TNetData.NetData.ConnectionUnlock(FNetConnection);
    end;
  end;
end;

constructor TThreadNodeNotifyOperations.Create(NetConnection: TNetConnection;
  MakeACopyOfOperationsHashTree: TOperationsHashTree);
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
