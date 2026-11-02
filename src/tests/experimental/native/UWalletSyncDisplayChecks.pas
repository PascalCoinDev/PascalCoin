unit UWalletSyncDisplayChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>
  Native first-show regression for the real synchronization control.
  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

{$IFDEF FPC}{$mode delphi}{$ENDIF}
{$I ..\..\..\config.inc}

interface

uses UFRMMainForm;

// The caller supplies the fresh offline wallet main form, already in wallet
// mode. Only the real queued mode handler is delivered; no periodic timer is
// pumped between seeding the old design captions and checking the first frame.
function CheckSyncFirstDisplay(AMain: TFRMMainForm): String;
function CheckSyncWriterIndependence(AMain: TFRMMainForm): String;

implementation

uses Windows, SysUtils, Classes, Forms, SyncObjs, UCommon, UUserInterface, UNode,
  UThread, UBlockChain, UCTRLSyncronization;

type
  TGatedSync = class(TCTRLSyncronization)
  protected
    procedure ReadSecondarySnapshot(ANode: TNode; out ASnapshot: TSyncNodeSnapshot); override;
  public
    Entered, ReleaseRead: TEvent;
    ReadCalls: LongInt;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  TCoreWriter = class(TPCThread)
  private FNode: TNode;
  protected procedure BCExecute; override;
  public
    Completed: TEvent;
    ErrorMessage: String;
    constructor Create(ANode: TNode);
    destructor Destroy; override;
  end;

constructor TGatedSync.Create(AOwner: TComponent);
begin
  inherited;
  Entered := TEvent.Create(nil, True, False, '');
  ReleaseRead := TEvent.Create(nil, True, False, '');
end;

destructor TGatedSync.Destroy;
begin
  ReleaseRead.SetEvent;
  inherited;
  Entered.Free;
  ReleaseRead.Free;
end;

procedure TGatedSync.ReadSecondarySnapshot(ANode: TNode; out ASnapshot: TSyncNodeSnapshot);
begin
  InterlockedIncrement(ReadCalls);
  Entered.SetEvent;
  while ReleaseRead.WaitFor(10) <> wrSignaled do
    if TThread.CheckTerminated then Exit;
  inherited;
end;

constructor TCoreWriter.Create(ANode: TNode);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FNode := ANode;
  Completed := TEvent.Create(nil, True, False, '');
end;

destructor TCoreWriter.Destroy;
begin
  WaitFor;
  Completed.Free;
  inherited;
end;

procedure TCoreWriter.BCExecute;
var L: TPCOperationsComp;
begin
  try
    try
      L := FNode.LockMempoolWrite;
      try
        if not Assigned(L) then raise Exception.Create('No actual mempool');
      finally FNode.UnlockMempoolWrite; end;
    except on E: Exception do ErrorMessage := E.ClassName + ': ' + E.Message; end;
  finally Completed.SetEvent; end;
end;

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create('Sync first display: ' + AMessage);
end;

function CheckSyncFirstDisplay(AMain: TFRMMainForm): String;
var LStarted, LDeadline: UInt64; LAverage: String; S: TNodeSynchronizationStatus;
begin
  Require(AMain.Mode = wmWallet, 'requires the hidden synchronization view');
  {$IFDEF USE_ABSTRACTMEM}
  Require(TUserInterface.Node.Bank.SafeBox.PCAbstractMem.AccountCache.MaxRegisters = 250000,
    'experimental startup did not apply its bounded account-cache budget');
  {$ENDIF}
  S := TUserInterface.SynchronizationStatus;
  Require((S.LocalBlockCount = 0) and S.HasMempoolSummary, 'requires fresh initialized cached data');
  LAverage := AMain.SyncControl.lblTimeAverage.Caption;
  AMain.SyncControl.lblTotalBlocksValue.Caption := '000';
  AMain.SyncControl.lblTotalAccountsValue.Caption := '000';
  AMain.SyncControl.lblPendingOperationsValue.Caption := '000';
  AMain.SyncControl.lblBlockTargetValue.Caption := '000';
  AMain.SyncControl.lblTimeAverage.Caption := '000';
  LStarted := GetTickCount64;
  TUserInterface.ShowSyncDialog;
  AMain.Perform(CM_PC_ModeChanged, 0, 0);
  Require(GetTickCount64 - LStarted < 500, 'show blocked instead of rendering the available snapshot');
  Require(AMain.SyncControl.IsVisible, 'the real mode handler did not show the control');
  Require(AMain.SyncControl.lblTotalBlocksValue.Caption = '(none)', 'first frame retained the block designer caption');
  Require(AMain.SyncControl.lblTotalAccountsValue.Caption = '0', 'first frame retained the account designer caption');
  Require(AMain.SyncControl.lblPendingOperationsValue.Caption = '0', 'first frame retained the pending designer caption');
  Require(Length(AMain.SyncControl.lblBlockTargetValue.Caption) = 8, 'first frame did not render the real compact target');
  Require((AMain.SyncControl.lblTimeAverage.Caption = LAverage) or
    (AMain.SyncControl.lblTimeAverage.Caption = 'Calculating...'),
    'first frame did not retain cached or pending secondary statistics');
  // The first frame above was checked without pumping. A missing initial
  // secondary result must then really publish, rather than remain Calculating.
  LDeadline := GetTickCount64 + 2000;
  while (Pos('Last 100:', AMain.SyncControl.lblTimeAverage.Caption) <> 1) and
    (GetTickCount64 < LDeadline) do begin
    CheckSynchronize(0);
    Application.ProcessMessages;
    Sleep(1);
  end;
  Require(Pos('Last 100:', AMain.SyncControl.lblTimeAverage.Caption) = 1,
    'Initial secondary statistics never published');
  LAverage := AMain.SyncControl.lblTimeAverage.Caption;
  AMain.SyncControl.lblTimeAverage.Caption := '000';
  TUserInterface.ShowWallet;
  AMain.Perform(CM_PC_ModeChanged, 0, 0);
  TUserInterface.ShowSyncDialog;
  AMain.Perform(CM_PC_ModeChanged, 0, 0);
  Require(AMain.SyncControl.lblTimeAverage.Caption = LAverage,
    'Re-entering the view did not immediately restore its completed averages');
  Result := Format('SYNC FIRST FRAME: synchronous actual mode/show, %d ms; blocks=%s accounts=%s pending=%s target=%s',
    [GetTickCount64 - LStarted, AMain.SyncControl.lblTotalBlocksValue.Caption,
     AMain.SyncControl.lblTotalAccountsValue.Caption, AMain.SyncControl.lblPendingOperationsValue.Caption,
     AMain.SyncControl.lblBlockTargetValue.Caption]);
end;

function CheckSyncWriterIndependence(AMain: TFRMMainForm): String;
var C: TGatedSync; W: TCoreWriter; T, Deadline: UInt64;
begin
  C := TGatedSync.Create(AMain);
  W := nil;
  try
    C.RefreshNodeState;
    Require(C.Entered.WaitFor(2000) = wrSignaled, 'Actual secondary worker did not reach its read delegate');
    W := TCoreWriter.Create(TUserInterface.Node);
    T := GetTickCount64;
    W.Start;
    Require(W.Completed.WaitFor(1000) = wrSignaled,
      'Actual core writer waited for the gated UI snapshot to release its mempool read');
    Require(W.ErrorMessage = '', W.ErrorMessage);
    Require(C.ReleaseRead.WaitFor(0) <> wrSignaled, 'Snapshot gate was released before core progress');
    Result := Format('REVERSE INDEPENDENCE: actual core writer completed in %d ms while actual secondary snapshot remained gated',
      [GetTickCount64 - T]);
    C.ReleaseRead.SetEvent;
    Deadline := GetTickCount64 + 2000;
    repeat
      CheckSynchronize(0);
      Application.ProcessMessages;
      if Pos('Last 100:', C.lblTimeAverage.Caption) = 1 then Break;
      Sleep(1);
    until GetTickCount64 >= Deadline;
    Require(Pos('Last 100:', C.lblTimeAverage.Caption) = 1, 'Real bank read did not publish after gate release');
    C.RefreshNodeState;
    TUserInterface.SynchronizationChanged.Invoke(nil);
    Deadline := GetTickCount64 + 200;
    repeat CheckSynchronize(0); Application.ProcessMessages; Sleep(1);
    until GetTickCount64 >= Deadline;
    Require(InterlockedCompareExchange(C.ReadCalls, 0, 0) = 1,
      'Same-tip status or view entry repeated secondary bank reads');
  finally
    // Release prerequisites before joining on failed RED checks as well.
    C.ReleaseRead.SetEvent;
    W.Free;
    C.Free;
  end;
end;

end.
