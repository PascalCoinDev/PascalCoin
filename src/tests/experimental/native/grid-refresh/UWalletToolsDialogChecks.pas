unit UWalletToolsDialogChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Runs actual wallet menu handlers and modal wizard presentation on the private
  QA desktop. Its fresh node can contact loopback only and never broadcasts. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses UWalletGridMocks;

procedure SeedWalletToolsSettings(const ADirectory: String);
procedure CheckWalletToolsDialogs(AMain: TMockMainForm; const ALogPath: String);

implementation

uses Windows, Messages, Classes, SysUtils, Types, Math, Variants, Generics.Collections, Forms, Controls,
  StdCtrls, ExtCtrls, Menus, Graphics, Grids, {$IFDEF FPC}LMessages,{$ENDIF}
  UAppParams, USettings, UMemory, UCommon, UGridUtils, Buttons,
  UNode, UNetProtocol, UUserInterface, UFRMMainForm, UVisualGrid, UWizard,
  UFRMAccountExplorer, UFRMPendingOperations, UFRMBlockExplorer, UFRMOperationExplorer,
  UFRMOperationsExplorer, UFRMRPCCalls, UFRMLogs, UFRMNodes, UFRMMessages, UFRMPayloadDecoder,
  UBlockChain, UFileStorage, UCrypto, UConst, UCoreObjects, UCoreUtils, UWIZOperation,
  UWIZChangeKey, UWIZOperationSelected, UWIZOperationConfirmation,
  UWalletOperationDetailsChecks, UWalletOperationInfoLayoutChecks;

type
  TGroupBoxAccess = class(TGroupBox);

  TMockWalletPopupAccess = class(TMockWallet)
  public
    function NativeAccountPopupHandler: TPreparePopupMenuEvent;
  end;

  TChangeKeyWizardAccess = class(TWIZChangeKeyWizard)
  public
    procedure PresentConfirmation;
    procedure PresentSelectedAgain;
  end;

  TProbeStage = (psIdle, psModal, psPopup, psWizard, psOperationInput, psDone);

  TWalletToolsChecks = class(TComponent)
  private
    FMain: TMockMainForm;
    FTimer: TTimer;
    FLog: TStringList;
    FLogPath, FExpectedClass, FError: String;
    FExpectedForm: TFormClass;
    FStage: TProbeStage;
    FDeadline: UInt64;
    FSeen, FPopupPrepared, FWizardCancelled, FLoggedModal: Boolean;
    FOperationDialogCount, FExpectedOperation: Integer;
    FLastDecodedOperation: Cardinal;
    FInputCase: String;
    FObservedOperationDialog: TForm;
    FObservingOperationDialog: Boolean;
    FPreviousException: TExceptionEvent;
    FPreviousPrepare: TPreparePopupMenuEvent;
    procedure Require(AValue: Boolean; const AMessage: String);
    procedure Log(const AMessage: String);
    procedure Pump(AMilliseconds: Cardinal);
    procedure CheckErrors;
    procedure LogModalWindows(const ACheckpoint: String);
    procedure OnException(Sender: TObject; E: Exception);
    procedure TimerTick(Sender: TObject);
    procedure PreparePopup(Sender: TObject;
      {$IFDEF FPC}constref{$ELSE}const{$ENDIF} ASelection: TVisualGridSelection;
      out APopupMenu: TPopupMenu);
    procedure CloseProbeDialogs;
    procedure CheckConnections;
    procedure LoadGenesis;
    procedure CheckMenu(AItem: TMenuItem; AFormClass: TFormClass; AModal: Boolean);
    procedure CheckMenus;
    procedure CheckBlockTools;
    procedure CheckOperationInput;
    procedure CheckHistoryDepth;
    procedure CheckWizard;
    procedure CheckWizardGrid(AWizard: TWIZChangeKeyWizard; const ACheckpoint: String);
    procedure CheckWizardPaint(AGrid: TVisualGrid; const ACheckpoint: String);
    procedure CheckOverview(const ACheckpoint: String);
    function PaintControl(AControl: TWinControl): TBitmap;
    procedure Capture(AControl: TWinControl; const ASuffix: String);
    function VisibleForm(AFormClass: TFormClass): TForm;
  protected
    procedure Notification(AComponent: Classes.TComponent;
      AOperation: Classes.TOperation); override;
  public
    constructor Create(AMain: TMockMainForm; const ALogPath: String); reintroduce;
    destructor Destroy; override;
    procedure Run;
    procedure RunBlockTools;
    procedure RunOperationInput;
    procedure RunHistoryDepth;
  end;

procedure SeedWalletToolsSettings(const ADirectory: String);
var
  LBag: TDisposables;
  LParams: TAppParams;
begin
  LParams := LBag.AddObject(TAppParams.Create(nil)) as TAppParams;
  LParams.FileName := IncludeTrailingPathDelimiter(ADirectory) + 'AppParams.prm';
  // RPC's form requires online policy, but discovery remains strictly private.
  LParams.ParamByName[CT_PARAM_TryToConnectOnlyWithThisFixedServers].SetAsString('127.0.0.1:1');
  LParams.ParamByName[CT_PARAM_PeerCache].SetAsString('');
  LParams.ParamByName[CT_PARAM_InternetServerPort].SetAsInteger(0);
  LParams.ParamByName[CT_PARAM_JSONRPCEnabled].SetAsBoolean(False);
  LParams.ParamByName[CT_PARAM_JSONRPCMinerServerActive].SetAsBoolean(False);
  LParams.ParamByName[CT_PARAM_JSONRPCMinerServerPort].SetAsInteger(0);
  LParams.ParamByName[CT_PARAM_MinerPrivateKeyType].SetAsInteger(Ord(mpk_Random));
  LParams.ParamByName[CT_PARAM_ShowLogs].SetAsBoolean(True);
  LParams.ParamByName[CT_PARAM_SaveLogFiles].SetAsBoolean(False);
end;

procedure TChangeKeyWizardAccess.PresentConfirmation;
begin
  PresentScreen(CreateScreen(TWIZOperationConfirmation));
end;

procedure TChangeKeyWizardAccess.PresentSelectedAgain;
begin
  PresentScreen(CurrentScreen);
end;

function TMockWalletPopupAccess.NativeAccountPopupHandler: TPreparePopupMenuEvent;
begin
  Result := OnPrepareAccountPopupMenu;
end;

constructor TWalletToolsChecks.Create(AMain: TMockMainForm; const ALogPath: String);
begin
  inherited Create(nil);
  FMain := AMain;
  FLogPath := ALogPath;
  FLog := TStringList.Create;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 20;
  FTimer.OnTimer := TimerTick;
  FPreviousException := Application.OnException;
  Application.OnException := OnException;
end;

destructor TWalletToolsChecks.Destroy;
begin
  FTimer.Enabled := False;
  Application.OnException := FPreviousException;
  FLog.Free;
  inherited Destroy;
end;

procedure TWalletToolsChecks.Notification(AComponent: Classes.TComponent;
  AOperation: Classes.TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if (AOperation = opRemove) and (AComponent = FObservedOperationDialog) then
    FObservedOperationDialog := nil;
end;

procedure TWalletToolsChecks.Require(AValue: Boolean; const AMessage: String);
begin
  if not AValue then
    raise Exception.Create(AMessage);
end;

procedure TWalletToolsChecks.Log(const AMessage: String);
begin
  FLog.Add(FormatDateTime('hh:nn:ss.zzz', Now) + ' ' + AMessage);
  FLog.SaveToFile(FLogPath);
end;

procedure TWalletToolsChecks.CheckErrors;
begin
  Require(FError = '', FError);
end;

procedure TWalletToolsChecks.LogModalWindows(const ACheckpoint: String);
var
  I: Integer;
  LForm: TForm;
  LNativeVisible: Boolean;
begin
  Log(Format('MODAL %s expected=%s forms=%d active=%s',
    [ACheckpoint, FExpectedClass, Screen.FormCount, IntToHex(GetActiveWindow, SizeOf(Pointer) * 2)]));
  for I := 0 to Screen.FormCount - 1 do begin
    LForm := Screen.Forms[I];
    LNativeVisible := LForm.HandleAllocated and IsWindowVisible(LForm.Handle);
    Log(Format('MODAL FORM %s name=%s visible=%s showing=%s native=%s modal=%s caption=%s',
      [LForm.ClassName, LForm.Name, BoolToStr(LForm.Visible, True),
       BoolToStr(LForm.Showing, True), BoolToStr(LNativeVisible, True),
       BoolToStr(fsModal in LForm.FormState, True), LForm.Caption]));
  end;
end;

procedure TWalletToolsChecks.Pump(AMilliseconds: Cardinal);
var
  LUntil: UInt64;
begin
  LUntil := GetTickCount64 + AMilliseconds;
  repeat
    Application.ProcessMessages;
    CheckSynchronize(0);
    CheckErrors;
    Sleep(1);
  until GetTickCount64 >= LUntil;
end;

procedure TWalletToolsChecks.OnException(Sender: TObject; E: Exception);
begin
  if FError = '' then
    FError := E.ClassName + ': ' + E.Message;
  CloseProbeDialogs;
end;

function TWalletToolsChecks.VisibleForm(AFormClass: TFormClass): TForm;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do
    // GNUgettext's !dx proxy keeps the original class as its parent. Match the
    // actual form type so translation cannot hide a legitimate native window.
    if Screen.Forms[I].InheritsFrom(AFormClass) and Screen.Forms[I].Visible then
      Exit(Screen.Forms[I]);
end;

procedure TWalletToolsChecks.CloseProbeDialogs;
var
  I: Integer;
begin
  EndMenu;
  for I := Screen.FormCount - 1 downto 0 do
    if (Screen.Forms[I] <> FMain) and Screen.Forms[I].Visible and
      (fsModal in Screen.Forms[I].FormState) then
      Screen.Forms[I].ModalResult := mrCancel;
end;

procedure TWalletToolsChecks.CheckConnections;
var
  LConnections: TList<TNetConnection>;
  LConnection: TNetConnection;
  LAddress: String;
  I: Integer;
begin
  Require(TSettings.TryConnectOnlyWithThisFixedServers = '127.0.0.1:1',
    'Tools fixture lost its loopback-only discovery setting');
  Require(not TSettings.JsonRpcPortEnabled and not TSettings.JsonRpcMinerServerActive,
    'Tools fixture unexpectedly enabled an RPC or miner service');
  LConnections := TNetData.NetData.NetConnections.LockList;
  try
    for I := 0 to LConnections.Count - 1 do begin
      LConnection := LConnections[I];
      LAddress := LConnection.ClientRemoteAddr;
      Require((LAddress = '') or (LAddress = 'NIL') or (LAddress = '127.0.0.1') or
        (Pos('127.0.0.1:', LAddress) = 1), 'Tools fixture attempted an external peer: ' + LAddress);
      Require(not LConnection.Connected, 'Tools fixture connected to a peer');
    end;
  finally
    TNetData.NetData.NetConnections.UnlockList;
  end;
  Require(TNetData.NetData.NetStatistics.ActiveConnections = 0,
    'Tools fixture has an active peer');
end;

procedure TWalletToolsChecks.LoadGenesis;
var
  LBag: TDisposables;
  LStream: TFileStream;
  LBlock: TPCOperationsComp;
  LCount: Cardinal;
  LMessage: String;
  LUntil: UInt64;
begin
  TNode.Node.BroadcastData := False;
  TNode.Node.UpdateBlockchain := False;
  Require(TNode.Node.Bank.BlocksCount = 0, 'Tools fixture requires a fresh empty bank');
  TNode.Node.Bank.StorageClass := TFileStorage;
  LStream := LBag.AddObject(TFileStream.Create(ExtractFilePath(ParamStr(0)) +
    'mainnet-blocks-0-4.bin', fmOpenRead or fmShareDenyNone)) as TFileStream;
  LBlock := LBag.AddObject(TPCOperationsComp.Create(nil)) as TPCOperationsComp;
  LStream.ReadBuffer(LCount, SizeOf(LCount));
  Require(LCount = 5, 'Unexpected public genesis fixture');
  LBlock.Bank := TNode.Node.Bank;
  Require(LBlock.LoadBlockFromStream(LStream, LMessage), LMessage);
  Require(LBlock.OperationBlock.block = 0, 'Fixture must begin with public genesis');
  Require(TNode.Node.AddNewBlockChain(nil, LBlock, LMessage), LMessage);
  Require(TNode.Node.Bank.BlocksCount = 1, 'Public genesis did not pass normal validation');
  LUntil := GetTickCount64 + 8000;
  while not TNode.Node.GetSynchronizationStatus.PendingOperationsAvailable and
    (GetTickCount64 < LUntil) do
    Pump(5);
  Require(TNode.Node.GetSynchronizationStatus.PendingOperationsAvailable,
    'Genesis pending-state reconciliation did not finish');
  Pump(100);
  Log('GENESIS validated public block 0, five real accounts; broadcasting disabled');
end;

procedure TWalletToolsChecks.CheckMenu(AItem: TMenuItem; AFormClass: TFormClass; AModal: Boolean);
var
  LForm: TForm;
  LUntil: UInt64;
  LClassName: String;
begin
  LClassName := AFormClass.ClassName;
  Require(AItem.Enabled and Assigned(AItem.OnClick), AItem.Name + ' is not an enabled menu action');
  Log('MENU ENTER ' + AItem.Name + ' expected=' + LClassName);
  if AModal then begin
    FExpectedClass := LClassName;
    FExpectedForm := AFormClass;
    FSeen := False;
    FLoggedModal := False;
    FStage := psModal;
    FDeadline := GetTickCount64 + 6000;
    FTimer.Enabled := True;
    try
      AItem.Click;
      Log('MENU RETURN ' + AItem.Name);
    finally
      FTimer.Enabled := False;
      FStage := psIdle;
    end;
    CheckErrors;
    Require(FSeen, AItem.Name + ' did not open its expected modal form');
  end else begin
    AItem.Click;
    LUntil := GetTickCount64 + 6000;
    repeat
      Pump(10);
      LForm := VisibleForm(AFormClass);
    until Assigned(LForm) or (GetTickCount64 >= LUntil);
    Require(Assigned(LForm), AItem.Name + ' did not open ' + LClassName);
    Require(LForm.HandleAllocated and IsWindowVisible(LForm.Handle), LClassName + ' has no visible native window');
    Capture(LForm, '.' + AItem.Name + '.bmp');
    LForm.Close;
    Pump(30);
    Require(not Assigned(VisibleForm(AFormClass)), LClassName + ' remained visible after Close');
  end;
  Log('MENU ' + AItem.Name + ' opened ' + LClassName + ' and closed normally');
end;

procedure TWalletToolsChecks.CheckBlockTools;
var
  LForm: TFRMBlockExplorer;
  LGrid: TBlockChainGrid;
  LUntil: UInt64;
  LMenu: HMENU;
  LHashRateBlocks: Integer;
  LUnits: TShowHashRateAs;
  I: Integer;

  procedure AwaitRealRefresh;
  begin
    LUntil := GetTickCount64+3000;
    repeat
      Pump(10);
    until (LForm.dgBlockChainExplorer.RowCount = 2) or (GetTickCount64 >= LUntil);
    Require(LForm.dgBlockChainExplorer.RowCount = 2,
      'Block Tools did not repopulate the actual grid from the validated genesis block');
  end;

begin
  Require(FMain.miBlockExplorer.Enabled,'The real Blocks menu is disabled');
  FMain.miBlockExplorer.Click;
  Pump(50);
  LForm := TFRMBlockExplorer(VisibleForm(TFRMBlockExplorer));
  Require(Assigned(LForm),'The actual Blocks action did not open Block Explorer');
  try
    Require((LForm.miTools.Count = 2) and Assigned(LForm.miRefresh.OnClick) and
      Assigned(LForm.miResetBlockFilters.OnClick),'Block Explorer Tools is empty or unwired');
    LMenu := GetSubMenu(GetMenu(LForm.Handle),0);
    Require((LMenu <> 0) and (GetMenuItemCount(LMenu) = 2),
      'The native Tools submenu does not contain both actions');
    LGrid := nil;
    for I := 0 to LForm.ComponentCount-1 do
      if LForm.Components[I] is TBlockChainGrid then
        LGrid := TBlockChainGrid(LForm.Components[I]);
    Require(Assigned(LGrid),'The actual block-grid controller is missing');
    LHashRateBlocks := LGrid.HashRateAverageBlocksCount;
    LUnits := LGrid.HashRateAs;
    LForm.ebBlockChainBlockStart.Text := '0';
    LForm.ebBlockChainBlockEnd.Text := '0';
    LForm.ebBlockChainBlockStartExit(nil);
    AwaitRealRefresh;
    // A real worker must replace this sentinel; an Invalidate alone cannot pass.
    LForm.dgBlockChainExplorer.RowCount := 6;
    LForm.miRefresh.Click;
    AwaitRealRefresh;
    Require((LGrid.BlockStart = 0) and (LGrid.BlockEnd = 0) and
      (LForm.ebBlockChainBlockStart.Text = '0') and (LForm.ebBlockChainBlockEnd.Text = '0'),
      'Refresh changed the selected block range');
    LForm.dgBlockChainExplorer.RowCount := 6;
    LForm.miResetBlockFilters.Click;
    AwaitRealRefresh;
    Require((LGrid.BlockStart < 0) and (LGrid.BlockEnd < 0) and
      (LForm.ebBlockChainBlockStart.Text = '') and (LForm.ebBlockChainBlockEnd.Text = ''),
      'Reset did not clear both displayed and applied range bounds');
    Require((LGrid.HashRateAverageBlocksCount = LHashRateBlocks) and (LGrid.HashRateAs = LUnits),
      'Reset changed the hash-rate settings');
    LForm.dgBlockChainExplorer.RowCount := 6;
    LForm.miResetBlockFilters.Click;
    AwaitRealRefresh;
    Capture(LForm,'.block-tools.bmp');
    Log('PASS Block Explorer Tools native submenu, Refresh, reset both bounds, repeated reset and preserved hash-rate settings');
  finally
    LForm.Close;
    Pump(50);
  end;
end;

procedure TWalletToolsChecks.CheckOperationInput;
var
  LForm: TFRMOperationExplorer;
  LGrid: Grids.TDrawGrid;
  LController: TOperationsGrid;
  LResume: TOperationResume;
  LPoint, LScreenPoint, LOtherPoint: TPoint;
  LCell: TRect;
  LBeforeDialogs, LBeforeTop, LBeforeRow, I: Integer;

  function PointParam(const APoint: TPoint): LPARAM;
  begin
    Result := LPARAM(Cardinal(APoint.X and $FFFF) or
      (Cardinal(APoint.Y and $FFFF) shl 16));
  end;

  procedure Arm(const ACase: String; AExpectedCount: Integer;
    AExpectedOperation: Integer = -1);
  begin
    FInputCase := ACase;
    FExpectedOperation := AExpectedOperation;
    LBeforeDialogs := FOperationDialogCount;
    FStage := psOperationInput;
    FDeadline := GetTickCount64 + 4000;
    FTimer.Enabled := True;
    Log(Format('INPUT %s expected_dialogs=%d expected_operation=%d',
      [ACase, AExpectedCount, AExpectedOperation]));
  end;

  procedure Complete(AExpectedCount: Integer);
  begin
    try
      Pump(120);
      Require(FOperationDialogCount - LBeforeDialogs = AExpectedCount,
        Format('%s opened %d operation dialogs; expected %d',
          [FInputCase, FOperationDialogCount - LBeforeDialogs, AExpectedCount]));
      if AExpectedCount > 0 then
        Require(FLastDecodedOperation = Cardinal(FExpectedOperation),
          FInputCase + ' opened the wrong operation');
      Require(not Assigned(FObservedOperationDialog),
        FInputCase + ' left its operation dialog alive after modal cancellation');
      Log('PASS operation input ' + FInputCase);
    finally
      FTimer.Enabled := False;
      FStage := psIdle;
    end;
  end;

  procedure ClickAt(const APoint: TPoint; ARightButton: Boolean);
  begin
    if ARightButton then begin
      SendMessage(LGrid.Handle, WM_RBUTTONDOWN, MK_RBUTTON, PointParam(APoint));
      SendMessage(LGrid.Handle, WM_RBUTTONUP, 0, PointParam(APoint));
    end else begin
      SendMessage(LGrid.Handle, WM_LBUTTONDOWN, MK_LBUTTON, PointParam(APoint));
      SendMessage(LGrid.Handle, WM_LBUTTONUP, 0, PointParam(APoint));
    end;
  end;

begin
  FMain.miOperationsExplorer.Click;
  Pump(50);
  LForm := TFRMOperationExplorer(VisibleForm(TFRMOperationExplorer));
  Require(Assigned(LForm), 'The actual Operations Explorer menu did not open its form');
  try
    LController := nil;
    for I := 0 to LForm.ComponentCount - 1 do
      if LForm.Components[I] is TOperationsGrid then
        LController := TOperationsGrid(LForm.Components[I]);
    Require(Assigned(LController), 'The real operation-grid controller is missing');
    LGrid := LForm.dgOperationsExplorer;
    // Only presentation records are needed. Detaching refresh prevents node
    // events replacing them; no fabricated transaction is signed or submitted.
    LController.Node := nil;
    LController.OperationsResume.Clear;
    for I := 0 to 39 do begin
      LResume := CT_TOperationResume_NUL;
      LResume.valid := True;
      LResume.Block := 0;
      LResume.NOpInsideBlock := I;
      LResume.AffectedAccount := I mod 5;
      LResume.n_operation := 100 + I;
      LResume.OperationTxt := 'Native operation input ' + IntToStr(I);
      LController.OperationsResume.Add(LResume);
    end;
    LGrid.RowCount := LController.OperationsResume.Count + LGrid.FixedRows;
    LGrid.SetFocus;
    Arm('programmatic-selection', 0);
    LGrid.Row := 12;
    LGrid.TopRow := 10;
    Complete(0);
    Require(LGrid.HandleAllocated and IsWindowVisible(LGrid.Handle),
      'Operation input requires the actual visible native grid');
    Require(Assigned(LGrid.OnDrawCell), 'The operation grid lost its real drawing handler');

    LPoint := Point(LGrid.ClientWidth div 3, LGrid.ClientHeight div 2);
    LScreenPoint := LGrid.ClientToScreen(LPoint);
    LBeforeTop := LGrid.TopRow;
    LBeforeRow := LGrid.Row;
    Arm('wheel-down', 0);
    SendMessage(LGrid.Handle, WM_MOUSEWHEEL, WPARAM(Cardinal($FF880000)),
      PointParam(LScreenPoint));
    Complete(0);
    Require((LGrid.TopRow > LBeforeTop) or (LGrid.Row > LBeforeRow),
      'Native wheel-down did not scroll or advance the operation selection');

    LBeforeTop := LGrid.TopRow;
    LBeforeRow := LGrid.Row;
    Arm('wheel-up', 0);
    SendMessage(LGrid.Handle, WM_MOUSEWHEEL, WPARAM(Cardinal($00780000)),
      PointParam(LScreenPoint));
    Complete(0);
    Require((LGrid.TopRow < LBeforeTop) or (LGrid.Row < LBeforeRow),
      'Native wheel-up did not scroll or move the operation selection back');

    LBeforeRow := LGrid.Row;
    Arm('keyboard-down', 0);
    SendMessage(LGrid.Handle, WM_KEYDOWN, VK_DOWN, 1);
    SendMessage(LGrid.Handle, WM_KEYUP, VK_DOWN, LPARAM($C0000001));
    Complete(0);
    Require(LGrid.Row = LBeforeRow + 1, 'Native Down did not change operation selection');
    Arm('keyboard-up', 0);
    SendMessage(LGrid.Handle, WM_KEYDOWN, VK_UP, 1);
    SendMessage(LGrid.Handle, WM_KEYUP, VK_UP, LPARAM($C0000001));
    Complete(0);
    Require(LGrid.Row = LBeforeRow, 'Native Up did not restore operation selection');

    LBeforeTop := LGrid.TopRow;
    Arm('scrollbar-down-command', 0);
    // Exercise the native scrollbar command path without moving the user's
    // pointer or dispatching process-wide keyboard input on their desktop.
    SendMessage(LGrid.Handle, WM_VSCROLL, SB_LINEDOWN, 0);
    Complete(0);
    Require(LGrid.TopRow > LBeforeTop, 'Native scrollbar command did not scroll');

    LGrid.TopRow := 1;
    LGrid.Row := 2;
    Pump(30);
    LCell := LGrid.CellRect(0, 3);
    LPoint := Point((LCell.Left + LCell.Right) div 2,
      (LCell.Top + LCell.Bottom) div 2);
    Arm('right-click-row', 0);
    ClickAt(LPoint, True);
    Complete(0);
    Arm('orphan-mouse-up', 0);
    SendMessage(LGrid.Handle, WM_LBUTTONUP, 0, PointParam(LPoint));
    Complete(0);
    Arm('drag-to-another-row', 0);
    SendMessage(LGrid.Handle, WM_LBUTTONDOWN, MK_LBUTTON, PointParam(LPoint));
    LCell := LGrid.CellRect(0, 4);
    LOtherPoint := Point((LCell.Left + LCell.Right) div 2,
      (LCell.Top + LCell.Bottom) div 2);
    SendMessage(LGrid.Handle, WM_MOUSEMOVE, MK_LBUTTON, PointParam(LOtherPoint));
    SendMessage(LGrid.Handle, WM_LBUTTONUP, 0, PointParam(LOtherPoint));
    Complete(0);
    Arm('drag-within-same-cell', 0);
    LOtherPoint := Point(LPoint.X + GetSystemMetrics(SM_CXDRAG) + 2, LPoint.Y);
    Require(LOtherPoint.X < LCell.Right, 'Same-cell drag fixture left its column');
    SendMessage(LGrid.Handle, WM_LBUTTONDOWN, MK_LBUTTON, PointParam(LPoint));
    SendMessage(LGrid.Handle, WM_MOUSEMOVE, MK_LBUTTON, PointParam(LOtherPoint));
    SendMessage(LGrid.Handle, WM_LBUTTONUP, 0, PointParam(LOtherPoint));
    Complete(0);
    Arm('left-click-row', 1, 102);
    ClickAt(LPoint, False);
    Complete(1);
    Require(LGrid.Row = 3, 'Left-click did not select the clicked operation row');
    Arm('repeat-left-click-selected-row', 1, 102);
    ClickAt(LPoint, False);
    Complete(1);

    LCell := LGrid.CellRect(0, 0);
    LPoint := Point((LCell.Left + LCell.Right) div 2,
      (LCell.Top + LCell.Bottom) div 2);
    Arm('header-click', 0);
    ClickAt(LPoint, False);
    Complete(0);

    Arm('empty-grid-reset', 0);
    LController.OperationsResume.Clear;
    LGrid.RowCount := 2;
    LGrid.TopRow := 1;
    LGrid.Row := 1;
    Complete(0);
    Arm('empty-placeholder-row', 0);
    LCell := LGrid.CellRect(0, 1);
    ClickAt(Point((LCell.Left + LCell.Right) div 2,
      (LCell.Top + LCell.Bottom) div 2), False);
    Complete(0);
    Arm('blank-client-area', 0);
    ClickAt(Point(20, LGrid.ClientHeight - 8), False);
    Complete(0);
    Capture(LForm, '.operation-input-empty.bmp');
    Log('PASS Operations Explorer native wheel, keyboard and scrollbar do not open details; only populated-row left clicks do');
  finally
    FTimer.Enabled := False;
    FStage := psIdle;
    LForm.Close;
    Pump(50);
  end;
end;

procedure TWalletToolsChecks.RunOperationInput;
begin
  CheckConnections;
  LoadGenesis;
  CheckOperationInput;
  CheckWalletOperationDetails(FLogPath);
  CheckWalletOperationInfoLayout(FLogPath);
  CheckConnections;
  CheckErrors;
  Log('PASS Operations Explorer click-only details and explicit main-wallet Details checks');
end;

procedure TWalletToolsChecks.CheckHistoryDepth;
const
  CDepths: array[0..2] of Cardinal = (2016, 8640, High(Cardinal));
var
  LWallet: TMockWallet;
  LGrid: TVisualGrid;
  LDuration: TComboBox;
  LCell: TRect;
  LPoint: LPARAM;
  LUntil: UInt64;
  LCompleted, LHitCol, LHitRow, I: Integer;
  LAccounts: TArray<Cardinal>;

  procedure LogSelection(const AStage: String);
  var
    LSelection: TVisualGridSelection;
    LSelected, LSource: TArray<Cardinal>;
    LSelectedText, LSourceText: String;
    J: Integer;
  begin
    LSelection := LGrid.Selection;
    LSelected := LWallet.SelectedAccounts;
    LSource := LWallet.OperationsSource.Accounts;
    LSelectedText := '';
    LSourceText := '';
    for J := 0 to High(LSelected) do
      LSelectedText := LSelectedText + UIntToStr(LSelected[J]) + ',';
    for J := 0 to High(LSource) do
      LSourceText := LSourceText + UIntToStr(LSource[J]) + ',';
    Log(Format('HISTORY SELECTION %s index=%d page=%d row=%d count=%d selected=[%s] source=[%s] all_wallet=%s mode=%d deselection=%d',
      [AStage, I, LSelection.Page, LSelection.Row, LSelection.RowCount,
       LSelectedText, LSourceText, BoolToStr(LWallet.OperationsSource.UseWalletAccounts, True),
       Ord(LWallet.OperationsMode), Ord(LGrid.DeselectionType)]));
  end;

  procedure WaitForOperationQuery(ABefore: Integer);
  begin
    LUntil := GetTickCount64 + 8000;
    repeat
      Pump(5);
    until ((FMain.Model.Metrics(mdkOperations).CompletedCount > ABefore) and
      not LWallet.OperationsGrid.IsUpdating) or (GetTickCount64 >= LUntil);
    Require((FMain.Model.Metrics(mdkOperations).CompletedCount > ABefore) and
      not LWallet.OperationsGrid.IsUpdating,
      'The actual main-wallet history query did not complete');
  end;

begin
  LWallet := FMain.MockWallet;
  FMain.Mode := wmWallet;
  FMain.Model.SetData(7401, 5);
  LGrid := LWallet.AccountsGrid;
  // This scenario needs all five public accounts, independently of the native
  // viewport. Auto capacity is covered by the separate grid component tests.
  LGrid.PageMode := vpmSingle;
  LGrid.RefreshThrottle.Cancel;
  LGrid.RefreshGrid;
  LUntil := GetTickCount64 + 8000;
  while (LGrid.IsUpdating or (LGrid.RowCount <> 5)) and
    (GetTickCount64 < LUntil) do
    Pump(5);
  Require(not LGrid.IsUpdating and (LGrid.RowCount = 5),
    'History picker requires the actual five-row account grid');
  for I := 0 to 4 do
    Require(Cardinal(LGrid.Rows[I].__KEY) = Cardinal(I),
      'The history picker must use all five public account IDs');
  Require(LWallet.OperationsGrid.FindComponent('OperationsHistory') is TComboBox,
    'The actual operations-grid duration widget is missing');
  LDuration := TComboBox(LWallet.OperationsGrid.FindComponent('OperationsHistory'));
  Require(Assigned(LDuration.OnChange), 'The actual history picker is unwired');
  for I := 0 to High(CDepths) do begin
    // Reset a preceding request without replacing production callbacks;
    // the operations picker still waits for its configured 250 ms silence.
    LWallet.OperationsGrid.RefreshThrottle.Cancel;
    LCompleted := FMain.Model.Metrics(mdkOperations).CompletedCount;
    LDuration.ItemIndex := I;
    LDuration.OnChange(LDuration);
    Require(LWallet.OperationsSource.BlockDepth = CDepths[I],
      'History picker configured the wrong block depth');
    WaitForOperationQuery(LCompleted);
    Require(LWallet.OperationsSource.LastFetchBlockDepth = CDepths[I],
      'The actual operation query did not receive the picker depth');

    LWallet.OperationsGrid.RefreshThrottle.Cancel;
    LCompleted := FMain.Model.Metrics(mdkOperations).CompletedCount;
    // A small native viewport may scroll even this complete five-row result.
    // Keep the actual target visible before checking its hit test and click.
    LGrid.InternalDrawGrid.TopRow := I + 2;
    LCell := LGrid.InternalDrawGrid.CellRect(0, I + 2);
    Require((LCell.Left >= 0) and (LCell.Top >= 0) and
      (LCell.Right <= LGrid.InternalDrawGrid.ClientWidth) and
      (LCell.Bottom <= LGrid.InternalDrawGrid.ClientHeight),
      'The intended history account cell is not fully visible');
    LGrid.InternalDrawGrid.MouseToCell((LCell.Left + LCell.Right) div 2,
      (LCell.Top + LCell.Bottom) div 2, LHitCol, LHitRow);
    Require((LHitCol = 0) and (LHitRow = I + 2),
      'The native account hit test differs from the intended row');
    Log(Format('HISTORY HIT index=%d cell=%d,%d,%d,%d client=%dx%d hit=%d,%d top_row=%d expected_account=%d actual_row_key=%s',
      [I, LCell.Left, LCell.Top, LCell.Right, LCell.Bottom,
       LGrid.InternalDrawGrid.ClientWidth, LGrid.InternalDrawGrid.ClientHeight,
       LHitCol, LHitRow, LGrid.InternalDrawGrid.TopRow, I + 1,
       VarToStr(LGrid.Rows[I + 1].__KEY)]));
    LogSelection('before');
    LPoint := LPARAM(Cardinal(((LCell.Left + LCell.Right) div 2) and $FFFF) or
      (Cardinal(((LCell.Top + LCell.Bottom) div 2) and $FFFF) shl 16));
    SendMessage(LGrid.InternalDrawGrid.Handle, WM_MOUSEMOVE, 0, LPoint);
    SendMessage(LGrid.InternalDrawGrid.Handle, WM_LBUTTONDOWN, MK_LBUTTON, LPoint);
    SendMessage(LGrid.InternalDrawGrid.Handle, WM_LBUTTONUP, 0, LPoint);
    WaitForOperationQuery(LCompleted);
    LogSelection('after');
    LAccounts := LWallet.OperationsSource.Accounts;
    Require((Length(LAccounts) = 1) and (LAccounts[0] = Cardinal(I + 1)),
      'The actual account-row click did not reach the operations source');
    Require(not LWallet.OperationsSource.UseWalletAccounts,
      'Selected-account history unexpectedly queried all wallet accounts');
    Require((LWallet.OperationsSource.BlockDepth = CDepths[I]) and
      (LWallet.OperationsSource.LastFetchBlockDepth = CDepths[I]),
      'Changing the selected account reset the requested history depth');
    Log(Format('PASS actual history picker index=%d configured=%s fetched=%s selected_account=%d',
      [I, UIntToStr(LWallet.OperationsSource.BlockDepth),
       UIntToStr(LWallet.OperationsSource.LastFetchBlockDepth), I + 1]));
  end;
  Capture(LWallet, '.history-depth-maximum.bmp');
  Log('PASS actual main-wallet 7 Days, 30 Days and Maximum query depths survive account selection');
end;

procedure TWalletToolsChecks.RunHistoryDepth;
begin
  CheckConnections;
  LoadGenesis;
  CheckHistoryDepth;
  CheckConnections;
  CheckErrors;
  Log('PASS actual main-wallet history-depth picker checks');
end;

procedure TWalletToolsChecks.RunBlockTools;
begin
  CheckConnections;
  LoadGenesis;
  CheckBlockTools;
  CheckConnections;
  CheckErrors;
  Log('PASS actual Block Explorer Tools refresh/reset checks');
end;

procedure TWalletToolsChecks.CheckMenus;
begin
  CheckMenu(FMain.miAccountExplorer, TFRMAccountExplorer, False);
  CheckMenu(FMain.miPendingOperations, TFRMPendingOperations, False);
  CheckMenu(FMain.miBlockExplorer, TFRMBlockExplorer, False);
  CheckMenu(FMain.miOperationsExplorer, TFRMOperationExplorer, False);
  CheckMenu(FMain.miOperationsWorkbench, TFRMOperationsExplorer, True);
  CheckMenu(FMain.miRPCCalls, TFRMRPCCalls, True);
  CheckMenu(FMain.miLogs, TFRMLogs, False);
  CheckMenu(FMain.miNodes, TFRMNodes, False);
  CheckMenu(FMain.miMessages, TFRMMessages, False);
  Require(FMain.miSyncDialog.Enabled, 'Synchronization menu is disabled');
  FMain.miSyncDialog.Click;
  Pump(50);
  Require((FMain.Mode = wmSync) and FMain.SyncControl.Visible and FMain.paSyncPanel.Visible,
    'Synchronization menu did not show the usual embedded synchronization view');
  Capture(FMain.SyncControl, '.miSyncDialog.bmp');
  FMain.Mode := wmWallet;
  Pump(50);
  Log('MENU miSyncDialog opened the embedded synchronization view and returned to wallet');
end;

procedure TWalletToolsChecks.PreparePopup(Sender: TObject;
  {$IFDEF FPC}constref{$ELSE}const{$ENDIF} ASelection: TVisualGridSelection;
  out APopupMenu: TPopupMenu);
begin
  FPreviousPrepare(Sender, ASelection, APopupMenu);
  Require(ASelection.RowCount = 2, 'Native right-click lost the two selected accounts');
  Require(APopupMenu = FMain.MockWallet.mnuAccountsPopup, 'Native right-click prepared the wrong popup');
  Require(FMain.MockWallet.miChangeKey.Caption = 'Change All Key', 'Native popup did not offer Change All Key');
  FPopupPrepared := True;
end;

procedure TWalletToolsChecks.CheckWizardGrid(AWizard: TWIZChangeKeyWizard; const ACheckpoint: String);
var
  LGrid: TVisualGrid;
  LControl: TWinControl;
  LMode: TBitBtn;
  LModeMenu: TPopupMenu;
  LUntil: UInt64;
  LReady: Boolean;
  I: Integer;
begin
  LGrid := nil;
  for I := 0 to AWizard.CurrentScreen.ComponentCount - 1 do
    if AWizard.CurrentScreen.Components[I] is TVisualGrid then
      LGrid := TVisualGrid(AWizard.CurrentScreen.Components[I]);
  Require(Assigned(LGrid), ACheckpoint + ': actual wizard screen has no grid');
  LUntil := GetTickCount64 + 1500;
  repeat
    LReady := AWizard.CurrentScreen.Showing and LGrid.Showing and
      LGrid.HandleAllocated and LGrid.InternalDrawGrid.HandleAllocated;
    if LReady then
      LReady := IsWindowVisible(LGrid.Handle) and IsWindowVisible(LGrid.InternalDrawGrid.Handle);
    if not LReady then
      Pump(10);
  until LReady or (GetTickCount64 >= LUntil);
  LControl := LGrid.InternalDrawGrid;
  while Assigned(LControl) do begin
    Log(Format('WIZARD CHAIN %s class=%s visible=%s showing=%s bounds=%d,%d,%d,%d',
      [ACheckpoint, LControl.ClassName, BoolToStr(LControl.Visible, True),
       BoolToStr(LControl.Showing, True), LControl.Left, LControl.Top, LControl.Width, LControl.Height]));
    LControl := LControl.Parent;
  end;
  Require(LReady, ACheckpoint + ': current screen and native grid did not become visible');
  Require(Assigned(LGrid.Parent) and LGrid.HandleAllocated and
    IsWindow(LGrid.InternalDrawGrid.Handle), ACheckpoint + ': wizard grid has no native parent');
  Require((LGrid.ColCount >= 2) and (LGrid.InternalDrawGrid.ColCount = LGrid.ColCount) and
    (LGrid.InternalDrawGrid.Columns.Count = LGrid.ColCount), ACheckpoint + ': native columns are missing');
  Require(LGrid.RowCount = 2, ACheckpoint + ': wizard did not fetch both selected accounts');
  Require(LGrid.InternalDrawGrid.RowCount >= 3, ACheckpoint + ': native grid has no header and two rows');
  Require(LGrid.FindComponent('PageModeButton') is TBitBtn, ACheckpoint + ': mode button is missing');
  Require(LGrid.FindComponent('PageModeMenu') is TPopupMenu, ACheckpoint + ': mode menu is missing');
  LMode := TBitBtn(LGrid.FindComponent('PageModeButton'));
  LModeMenu := TPopupMenu(LGrid.FindComponent('PageModeMenu'));
  Log(Format('WIZARD MODE %s items=%d mode=%d hint=%s metadata_columns=%d native_columns=%d native_rows=%d',
    [ACheckpoint, LModeMenu.Items.Count, Ord(LGrid.PageMode), LMode.Hint,
     LGrid.ColCount, LGrid.InternalDrawGrid.ColCount, LGrid.InternalDrawGrid.RowCount]));
  Require((LModeMenu.Items.Count = 3) and LModeMenu.Items[Ord(LGrid.PageMode)].Checked,
    ACheckpoint + ': native mode menu does not reflect PageMode');
  for I := 0 to 1 do begin
    Require(Cardinal(LGrid.Rows[I].AccountNumber) = Cardinal(I), ACheckpoint + ': incorrect account row');
    Require(UInt64(LGrid.Rows[I].Balance) = TNode.Node.GetAccount(I).balance,
      ACheckpoint + ': account balance differs from the real public genesis account');
  end;
  CheckWizardPaint(LGrid, ACheckpoint);
  Capture(LGrid, '.' + ACheckpoint + '-composite-grid.bmp');
  Capture(AWizard.CurrentScreen, '.' + ACheckpoint + '.bmp');
  Log('WIZARD ' + ACheckpoint + ': parented native grid contains both public genesis accounts');
end;

procedure TWalletToolsChecks.CheckWizardPaint(AGrid: TVisualGrid; const ACheckpoint: String);
var
  LBag: TDisposables;
  LBitmap: TBitmap;
  LRect: TRect;
  LBackground, LPixel: TColor;
  LHeaderPixels, LRowPixels, LCount, LRow, X, Y: Integer;
begin
  // Paint the actual native draw grid directly, independently of recursive
  // form capture, and inspect cell interiors so grid lines cannot pass alone.
  LBitmap := LBag.AddObject(PaintControl(AGrid.InternalDrawGrid)) as TBitmap;
  LBitmap.SaveToFile(ChangeFileExt(FLogPath, '.' + ACheckpoint + '-native-grid.bmp'));
  LHeaderPixels := 0;
  LRowPixels := 0;
  for LRow := 0 to 2 do begin
    LRect := AGrid.InternalDrawGrid.CellRect(0, LRow);
    Require((LRect.Right > LRect.Left + 8) and (LRect.Bottom > LRect.Top + 4) and
      (LRect.Left >= 0) and (LRect.Top >= 0) and
      (LRect.Right <= AGrid.InternalDrawGrid.ClientWidth) and
      (LRect.Bottom <= AGrid.InternalDrawGrid.ClientHeight),
      ACheckpoint + ': header or row cell does not fit the visible native client');
    Log(Format('WIZARD CELL %s row=%d rect=%d,%d,%d,%d',
      [ACheckpoint, LRow, LRect.Left, LRect.Top, LRect.Right, LRect.Bottom]));
    InflateRect(LRect, -3, -2);
    LBackground := ColorToRGB(LBitmap.Canvas.Pixels[LRect.Right - 1, LRect.Bottom - 1]);
    LCount := 0;
    for Y := LRect.Top to LRect.Bottom - 1 do
      for X := LRect.Left to LRect.Right - 1 do begin
        LPixel := ColorToRGB(LBitmap.Canvas.Pixels[X, Y]);
        if (Abs(Integer(GetRValue(LPixel)) - Integer(GetRValue(LBackground))) +
          Abs(Integer(GetGValue(LPixel)) - Integer(GetGValue(LBackground))) +
          Abs(Integer(GetBValue(LPixel)) - Integer(GetBValue(LBackground)))) > 90 then
          Inc(LCount);
      end;
    if LRow = 0 then
      LHeaderPixels := LCount
    else
      Inc(LRowPixels, LCount);
    Require(LCount >= 3, ACheckpoint + ': native header or account row painted no cell text');
  end;
  Log(Format('WIZARD PIXELS %s header=%d account_rows=%d',
    [ACheckpoint, LHeaderPixels, LRowPixels]));
end;

procedure TWalletToolsChecks.TimerTick(Sender: TObject);
var
  LForm: TForm;
  LWizard: TWIZChangeKeyWizard;
  LKey: TECPrivateKey;
  LMessage: String;
  LBag: TDisposables;
begin
  if FStage in [psIdle, psDone] then
    Exit;
  try
    if FStage = psOperationInput then begin
      if FObservingOperationDialog then begin
        Log('OPERATION OBSERVER reentry while capturing ' + FInputCase);
        Exit;
      end;
      Require(GetTickCount64 < FDeadline,
        'Timed out observing native operation input ' + FInputCase);
      LForm := VisibleForm(TFRMPayloadDecoder);
      if Assigned(LForm) then begin
        Log(Format('OPERATION OBSERVER %s instance=%s hwnd=%s modal_result=%d observed=%s',
          [FInputCase, IntToHex(NativeUInt(LForm), SizeOf(Pointer) * 2),
           IntToHex(NativeUInt(LForm.Handle), SizeOf(Pointer) * 2),
           LForm.ModalResult, BoolToStr(LForm = FObservedOperationDialog, True)]));
        if LForm = FObservedOperationDialog then
          Exit;
        FObservingOperationDialog := True;
        try
          Require((fsModal in LForm.FormState) and LForm.HandleAllocated and
            IsWindowVisible(LForm.Handle), 'Operation details is not a visible native modal form');
          // A native paint may pump another timer tick before ShowModal hides
          // this same form. Count its lifetime once, including allocator reuse.
          FObservedOperationDialog := LForm;
          LForm.FreeNotification(Self);
          Inc(FOperationDialogCount);
          Require(FOperationDialogCount < 8, 'Operation input repeatedly reopened its dialog');
          FLastDecodedOperation := TFRMPayloadDecoder(LForm).OpResume.n_operation;
          Require(TFRMPayloadDecoder(LForm).OpResume.valid,
            FInputCase + ' opened details for an empty operation');
          Capture(LForm, '.operation-input-' + FInputCase + '.bmp');
          LForm.ModalResult := mrCancel;
        finally
          FObservingOperationDialog := False;
        end;
      end;
      Exit;
    end;
    if (FStage = psModal) and not FLoggedModal then begin
      FLoggedModal := True;
      LogModalWindows('first timer observation');
    end;
    if (FStage = psModal) and (GetTickCount64 >= FDeadline) then
      LogModalWindows('deadline');
    Require(GetTickCount64 < FDeadline, 'Timed out waiting for the actual menu or wizard window');
    case FStage of
      psModal:
        begin
          LForm := VisibleForm(FExpectedForm);
          if not Assigned(LForm) then
            Exit;
          Require((fsModal in LForm.FormState) and LForm.HandleAllocated and
            IsWindowVisible(LForm.Handle), FExpectedClass + ' is not a visible native modal form');
          Capture(LForm, '.' + FExpectedClass + '.bmp');
          FSeen := True;
          FStage := psDone;
          LForm.ModalResult := mrCancel;
        end;
      psPopup:
        begin
          if not FPopupPrepared then
            Exit;
          EndMenu;
          FStage := psIdle;
        end;
      psWizard:
        begin
          LForm := VisibleForm(TWizardHostForm);
          if not Assigned(LForm) then
            Exit;
          Require(LForm.Owner is TWIZChangeKeyWizard, 'Change All Key did not open its actual wizard');
          LWizard := TWIZChangeKeyWizard(LForm.Owner);
          FStage := psDone;
          Require(LWizard.CurrentScreen is TWIZOperationSelected, 'Wizard did not begin on selected accounts');
          CheckWizardGrid(LWizard, 'wizard-selected');
          TChangeKeyWizardAccess(LWizard).PresentSelectedAgain;
          CheckWizardGrid(LWizard, 'wizard-selected-repeat');
          // Disposable destination material is never imported into the wallet
          // or used for signing; only its public key reaches confirmation.
          LKey := LBag.AddObject(TECPrivateKey.Create) as TECPrivateKey;
          LKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
          LWizard.Model.ChangeKey.ChangeKeyMode := akaTransferAccountOwnership;
          LWizard.Model.TransferAccount.AccountKey := LKey.PublicKey;
          LWizard.Model.Fee.SingleOperationFee := 0;
          LWizard.Model.Signer.OperationSigningMode := akaPrimary;
          LWizard.Model.Signer.SignerAccount := LWizard.Model.Account.SelectedAccounts[0];
          TChangeKeyWizardAccess(LWizard).PresentConfirmation;
          CheckWizardGrid(LWizard, 'wizard-confirmation');
          Require(LWizard.Cancel(LMessage), 'Wizard Cancel failed: ' + LMessage);
          Require(LWizard.State = wsCancelled, 'Wizard did not enter its cancelled state');
          FWizardCancelled := True;
        end;
    end;
  except
    on E: Exception do begin
      if FError = '' then
        FError := E.ClassName + ': ' + E.Message;
      FStage := psDone;
      CloseProbeDialogs;
    end;
  end;
end;

procedure TWalletToolsChecks.CheckWizard;
var
  LGrid: TVisualGrid;
  LCell: TRect;
  LPoint: LPARAM;
  LUntil: UInt64;
  I: Integer;
  LPreviousPopup: TPreparePopupMenuEvent;
  LPreviousEnabled: Boolean;
begin
  FMain.Model.SetData(7301, 5);
  LGrid := FMain.MockWallet.AccountsGrid;
  // Native popup/wizard checks need these five IDs, not an Auto-sized slice.
  LGrid.PageMode := vpmSingle;
  LGrid.RefreshThrottle.Cancel;
  LGrid.RefreshGrid;
  LUntil := GetTickCount64 + 8000;
  while (LGrid.IsUpdating or LGrid.RefreshThrottle.Waiting or (LGrid.RowCount <> 5)) and
    (GetTickCount64 < LUntil) do
    Pump(5);
  Require(not LGrid.IsUpdating and (LGrid.RowCount = 5), 'Mock wallet did not display five public account IDs');
  for I := 0 to 4 do
    Require(Cardinal(LGrid.Rows[I].__KEY) = Cardinal(I),
      'The native wizard must use all five public account IDs');
  LCell := LGrid.InternalDrawGrid.CellRect(0, 1);
  LPoint := LPARAM(((LCell.Top + LCell.Bottom) div 2) shl 16) or
    LPARAM((LCell.Left + LCell.Right) div 2);
  SendMessage(LGrid.InternalDrawGrid.Handle, WM_MOUSEMOVE, 0, LPoint);
  SendMessage(LGrid.InternalDrawGrid.Handle, WM_LBUTTONDOWN, MK_LBUTTON, LPoint);
  SendMessage(LGrid.InternalDrawGrid.Handle, WM_LBUTTONUP, 0, LPoint);
  // LCL exposes a rectangle setter; the VCL adapter extends its native range.
  {$IFDEF FPC}
  LGrid.InternalDrawGrid.Selection := Types.Rect(0, 1, 0, 2);
  {$ELSE}
  LGrid.InternalDrawGrid.MoveExtend(True, 0, 2);
  {$ENDIF}
  Require(Length(LGrid.SelectedRows) = 2, 'Native account selection did not contain two rows');
  LPreviousPopup := LGrid.OnPreparePopupMenu;
  LPreviousEnabled := FMain.MockWallet.miChangeKey.Enabled;
  // The ordinary synthetic demo intentionally disables monetary popups. Borrow
  // the actual wallet method only for this genesis-backed, cancel-only check.
  FPreviousPrepare := TMockWalletPopupAccess(FMain.MockWallet).NativeAccountPopupHandler();
  Require(Assigned(FPreviousPrepare) and
    (TMethod(FPreviousPrepare).Data = Pointer(FMain.MockWallet)), 'Actual account popup handler is not bound to this wallet');
  LGrid.OnPreparePopupMenu := PreparePopup;
  FMain.MockWallet.miChangeKey.Enabled := True;
  FPopupPrepared := False;
  FWizardCancelled := False;
  FStage := psPopup;
  FDeadline := GetTickCount64 + 6000;
  FTimer.Enabled := True;
  try
    // Dismiss the native popup before invoking its actual action; the timer is
    // then free to observe the wizard inside ShowModal on both VCL and LCL.
    SendMessage(LGrid.InternalDrawGrid.Handle, WM_RBUTTONUP, 0, LPoint);
    CheckErrors;
    Require(FPopupPrepared, 'Native right-click did not prepare the account popup');
    FStage := psWizard;
    FDeadline := GetTickCount64 + 6000;
    FMain.MockWallet.miChangeKey.Click;
  finally
    FTimer.Enabled := False;
    FStage := psIdle;
    LGrid.OnPreparePopupMenu := LPreviousPopup;
    FMain.MockWallet.miChangeKey.Enabled := LPreviousEnabled;
  end;
  CheckErrors;
  Require(FPopupPrepared and FWizardCancelled, 'Native Change All Key did not present and cancel both wizard screens');
  Require(TNode.Node.Bank.BlocksCount = 1, 'Wizard inspection changed confirmed chain state');
  Require(TNode.Node.MempoolOperationsCount = 0, 'Wizard inspection submitted an operation');
  Log('WIZARD actual native two-account popup, Change All Key, selected/repeated/confirmation screens, Cancel; no Finish or broadcast');
end;

function TWalletToolsChecks.PaintControl(AControl: TWinControl): TBitmap;
begin
  Result := TBitmap.Create;
  try
    Result.PixelFormat := pf24bit;
    Result.SetSize(AControl.ClientWidth, AControl.ClientHeight);
  {$IFDEF FPC}
  // Native group-box WM_PRINT returns no pixels on the private LCL desktop.
  // Dispatch its real paint handler, including graphic labels, into the DC.
    Result.Canvas.Brush.Color := AControl.GetRGBColorResolvingParent;
    Result.Canvas.FillRect(AControl.ClientRect);
    AControl.Perform(LM_ERASEBKGND, WPARAM(Result.Canvas.Handle), 0);
    AControl.Perform(LM_PAINT, WPARAM(Result.Canvas.Handle), 0);
  {$ELSE}
    AControl.PaintTo(Result.Canvas.Handle, 0, 0);
  {$ENDIF}
  except
    Result.Free;
    raise;
  end;
end;

procedure TWalletToolsChecks.Capture(AControl: TWinControl; const ASuffix: String);
var
  LBag: TDisposables;
  LBitmap: TBitmap;
begin
  LBitmap := LBag.AddObject(PaintControl(AControl)) as TBitmap;
  LBitmap.SaveToFile(ChangeFileExt(FLogPath, ASuffix));
end;

procedure TWalletToolsChecks.CheckOverview(const ACheckpoint: String);
var
  LGroup: TGroupBox;
  LContent: TRect;
  LTop, LBottom, LUpperGap, LLowerGap: Integer;
begin
  LGroup := FMain.MockWallet.GroupBox1;
  Require(LGroup.HandleAllocated, ACheckpoint + ': Overview group has no native window');
  // Measure the native usable content rather than repeating the layout formula.
  LContent := LGroup.ClientRect;
  TGroupBoxAccess(LGroup).AdjustClientRect(LContent);
  with FMain.MockWallet do begin
    Require(Label1.Top = lblTotalPASC.Top, ACheckpoint + ': PASC label pair is misaligned');
    Require(Label2.Top = lblTotalPASA.Top, ACheckpoint + ': PASA label pair is misaligned');
    LTop := Min(Label1.Top, lblTotalPASC.Top);
    LBottom := Max(Label2.Top + Label2.Height, lblTotalPASA.Top + lblTotalPASA.Height);
    Require(Max(Label1.Top + Label1.Height, lblTotalPASC.Top + lblTotalPASC.Height) <
      Min(Label2.Top, lblTotalPASA.Top), ACheckpoint + ': Overview rows overlap');
  end;
  LUpperGap := LTop - LContent.Top;
  LLowerGap := LContent.Bottom - LBottom;
  Require((LUpperGap >= 0) and (LLowerGap >= 0) and (Abs(LUpperGap - LLowerGap) <= 1),
    Format('%s: Overview labels are not centered in native content (gaps %d/%d)',
      [ACheckpoint, LUpperGap, LLowerGap]));
  Capture(LGroup, '.overview-' + ACheckpoint + '.bmp');
  Log(Format('OVERVIEW %s native content=%d..%d labels=%d..%d gaps=%d/%d',
    [ACheckpoint, LContent.Top, LContent.Bottom, LTop, LBottom, LUpperGap, LLowerGap]));
end;

procedure TWalletToolsChecks.Run;
var
  LBounds: TRect;
begin
  CheckConnections;
  LoadGenesis;
  CheckMenus;
  CheckOverview('default');
  LBounds := FMain.BoundsRect;
  FMain.ScaleBy(3, 2);
  try
    Pump(100);
    CheckOverview('scaled-150');
  finally
    FMain.ScaleBy(2, 3);
    FMain.BoundsRect := LBounds;
    Pump(100);
  end;
  CheckWizard;
  CheckConnections;
  CheckErrors;
  Log('PASS actual Tools menu forms, native Change All Key wizard/cancel and Overview geometry');
end;

procedure CheckWalletToolsDialogs(AMain: TMockMainForm; const ALogPath: String);
var
  LBag: TDisposables;
  LChecks: TWalletToolsChecks;
begin
  LChecks := LBag.AddObject(TWalletToolsChecks.Create(AMain, ALogPath)) as TWalletToolsChecks;
  if SameText(GetEnvironmentVariable('PASCAL_WALLET_HISTORY_DEPTH_ONLY'),'1') then
    LChecks.RunHistoryDepth
  else if SameText(GetEnvironmentVariable('PASCAL_WALLET_OPERATION_INPUT_ONLY'),'1') then
    LChecks.RunOperationInput
  else if SameText(GetEnvironmentVariable('PASCAL_WALLET_BLOCK_TOOLS_ONLY'),'1') then
    LChecks.RunBlockTools
  else
    LChecks.Run;
end;

end.
