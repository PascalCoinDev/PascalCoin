unit UMinerTemplateUpgradeChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>
  Distributed under the MIT software license; see the accompanying LICENSE.

  Exercise the actual inactive mining pool's template admission. No listener,
  peer, wallet key or synthetic replacement of the production fill is used. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses UNode;

procedure CheckMinerTemplateUpgrades(ANode: TNode);

implementation

uses Classes, SysUtils, SyncObjs, UThread, UPoolMining, UBlockChain, UPCDataTypes,
  UNetProtocol;

type
  TMinerPoolAccess = class(TPoolMiningServer)
  public
    procedure FillTemplate;
  end;

  THeldMempoolReader = class(TPCThread)
  private
    FNode: TNode;
  protected
    procedure BCExecute; override;
  public
    Entered, ReleaseRead, Completed: TEvent;
    ErrorMessage: String;
    constructor Create(ANode: TNode);
    destructor Destroy; override;
  end;

  TMinerFillThread = class(TPCThread)
  private
    FPool: TMinerPoolAccess;
  protected
    procedure BCExecute; override;
  public
    Attempted, Completed: TEvent;
    ErrorMessage: String;
    constructor Create(APool: TMinerPoolAccess);
    destructor Destroy; override;
  end;

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

procedure TMinerPoolAccess.FillTemplate;
begin
  inherited FillMinerOperations;
end;

constructor THeldMempoolReader.Create(ANode: TNode);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FNode := ANode;
  Entered := TEvent.Create(nil, True, False, '');
  ReleaseRead := TEvent.Create(nil, True, False, '');
  Completed := TEvent.Create(nil, True, False, '');
end;

destructor THeldMempoolReader.Destroy;
begin
  Terminate;
  ReleaseRead.SetEvent;
  if Suspended then Start;
  WaitFor;
  Completed.Free;
  ReleaseRead.Free;
  Entered.Free;
  inherited;
end;

procedure THeldMempoolReader.BCExecute;
begin
  try
    try
      if Terminated then Exit;
      FNode.LockMempoolRead;
      try
        Entered.SetEvent;
        while not Terminated do
          if ReleaseRead.WaitFor(10) = wrSignaled then Break;
      finally
        FNode.UnlockMempoolRead;
      end;
    except
      on E: Exception do ErrorMessage := E.ClassName + ': ' + E.Message;
    end;
  finally
    Completed.SetEvent;
  end;
end;

constructor TMinerFillThread.Create(APool: TMinerPoolAccess);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FPool := APool;
  Attempted := TEvent.Create(nil, True, False, '');
  Completed := TEvent.Create(nil, True, False, '');
end;

destructor TMinerFillThread.Destroy;
begin
  Terminate;
  if Suspended then Start;
  WaitFor;
  Completed.Free;
  Attempted.Free;
  inherited;
end;

procedure TMinerFillThread.BCExecute;
begin
  try
    try
      if Terminated then Exit;
      Attempted.SetEvent;
      FPool.FillTemplate;
    except
      on E: Exception do ErrorMessage := E.ClassName + ': ' + E.Message;
    end;
  finally
    Completed.SetEvent;
  end;
end;

procedure CheckFillAgainstReader(ANode: TNode; APool: TMinerPoolAccess;
  ARequiresWrite: Boolean);
var
  LReader: THeldMempoolReader;
  LFill: TMinerFillThread;
begin
  LReader := nil;
  LFill := nil;
  try
    LReader := THeldMempoolReader.Create(ANode);
    LReader.Start;
    Require(LReader.Entered.WaitFor(3000) = wrSignaled,
      'Ordinary reader did not acquire the actual mempool');
    LFill := TMinerFillThread.Create(APool);
    LFill.Start;
    Require(LFill.Attempted.WaitFor(3000) = wrSignaled,
      'Actual miner fill did not start');
    if ARequiresWrite then begin
      Require(LFill.Completed.WaitFor(250) = wrTimeout,
        'Changed miner template rebuilt while another reader held the mempool');
      LReader.ReleaseRead.SetEvent;
      Require(LReader.Completed.WaitFor(3000) = wrSignaled,
        'Ordinary reader did not release the mempool');
      Require(LFill.Completed.WaitFor(3000) = wrSignaled,
        'Actual miner template did not complete after promotion became possible');
    end else begin
      Require(LFill.Completed.WaitFor(3000) = wrSignaled,
        'Unchanged miner template unnecessarily requested exclusive ownership');
      Require(LReader.Completed.WaitFor(0) = wrTimeout,
        'Unchanged-template check did not retain a concurrent ordinary reader');
    end;
    Require(LFill.ErrorMessage = '', LFill.ErrorMessage);
    Require(LReader.ErrorMessage = '', LReader.ErrorMessage);
  finally
    // Release the prerequisite reader before joining a possibly promoting fill.
    LReader.Free;
    LFill.Free;
  end;
end;

procedure CheckMinerTemplateUpgrades(ANode: TNode);
var
  LPool: TMinerPoolAccess;
  LMempool: TPCOperationsComp;
  LOriginal, LChanged: TOperationBlock;
begin
  Require(Assigned(ANode) and (TNode.Node = ANode),
    'Mining check requires the existing isolated node singleton');
  Require(not ANode.NetServer.Active,
    'Mining check must not run with a network listener');
  LMempool := ANode.LockMempoolRead;
  try
    Require((LMempool.Count = 0) and (ANode.Bank.SafeBox.AccountsCount = 0),
      'Mining check requires generated empty node data');
    LOriginal := LMempool.OperationBlock;
  finally
    ANode.UnlockMempoolRead;
  end;
  LPool := nil;
  try
    LPool := TMinerPoolAccess.Create;
    Require(not LPool.Active and (LPool.ClientsCount = 0),
      'Mining check must keep the actual pool inactive');
    LPool.FillTemplate;
    CheckFillAgainstReader(ANode, LPool, False);
    LMempool := ANode.LockMempoolWrite;
    try
      LChanged := LMempool.OperationBlock;
      LChanged.block_payload := TEncoding.ASCII.GetBytes('isolated miner upgrade check');
      LMempool.SetOperationBlock(LChanged);
    finally
      ANode.UnlockMempoolWrite;
    end;
    CheckFillAgainstReader(ANode, LPool, True);
    CheckFillAgainstReader(ANode, LPool, False);
  finally
    LPool.Free;
    LMempool := ANode.LockMempoolWrite;
    try
      LMempool.SetOperationBlock(LOriginal);
    finally
      ANode.UnlockMempoolWrite;
    end;
  end;
end;

end.
