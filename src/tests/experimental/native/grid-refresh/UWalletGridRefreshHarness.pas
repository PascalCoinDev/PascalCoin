unit UWalletGridRefreshHarness;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>

  Interactive and automatic tests of the real experimental wallet composition.
  Every run creates fresh temporary data. Ordinary checks run offline; Tools
  checks permit loopback discovery only so they can open the real RPC dialog.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

function RunWalletGridRefreshDemo: Integer;

implementation

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, ExtCtrls,
  Graphics, Variants, {$IFNDEF FPC}Vcl.Imaging.pngimage,{$ENDIF}
  UThread, UNode, UUserInterface, UWalletStartup,
  UFRMMainForm, UCommon.Data, UMemory, UWalletGridMocks, UVisualGrid,
  UCTRLWallet, UWalletSelectionLoadingChecks, UWalletConsecutiveSelectionChecks, USyncFlickerChecks, UWalletToolsDialogChecks,
  UWalletSendPresentationChecks, UWalletOperationMessageChecks, UWalletAdvancedPresentationChecks,
  UWalletKeyNotificationChecks;

type
  TGridInputAccess = class(TVisualGrid)
  public
    procedure Search(const AExpression: String);
  end;

  TMockBlockProducer = class(TPCThread)
  private
    FNode: TNode;
    FModel: TMockWalletData;
    FCycleRows: Boolean;
    FCount: LongInt;
  protected
    procedure BCExecute; override;
  public
    constructor Create(ANode: TNode; AModel: TMockWalletData; ACycleRows: Boolean);
    procedure StopAndJoin;
    property Count: LongInt read FCount;
  end;

  TAutomaticPhase = (apStartup, apGapHold, apGapResume, apFinished);

  TGridRefreshHarness = class(TComponent)
  private
    FMain: TMockMainForm;
    FProducer: TMockBlockProducer;
    FTimer: TTimer;
    FPanel: TPanel;
    FLogView: TMemo;
    FStatus, FExpectation: TLabel;
    FCycleRows, FSlowFetch: TCheckBox;
    FRows: TComboBox;
    FAllLog: TStringList;
    FPreviousDestroy: TNotifyEvent;
    FAutomatic, FPassed, FWired, FWorkActive, FInTick, FManualGap: Boolean;
    FWaitingImageSaved: Boolean;
    FLogPath: String;
    FPhase: TAutomaticPhase;
    FNotifications, FFinalRevision: Integer;
    FStarted, FLastChange, FStopTick, FHoldStarted: UInt64;
    FLastAccounts, FLastOperations: Integer;
    procedure Tick(Sender: TObject);
    procedure MainDestroy(Sender: TObject);
    procedure Log(const AMessage: String);
    procedure DrainLog;
    procedure UpdateCounters;
    procedure Require(ACondition: Boolean; const AMessage: String);
    procedure NotifyBlock;
    procedure StopProducer;
    procedure EndWork;
    procedure StartContinuous(Sender: TObject);
    procedure StopContinuous(Sender: TObject);
    procedure StartBusyGap(Sender: TObject);
    procedure ToggleFetchGate(Sender: TObject);
    procedure ReleaseFetchGate(Sender: TObject);
    procedure ResetDemo(Sender: TObject);
    procedure ConfigureDelay(Sender: TObject);
    procedure ConfigureRows(Sender: TObject);
    procedure CheckNodeLoading(const ACheckpoint: String);
    procedure CaptureWaitingDemo;
    procedure AutomaticTick;
    function GridsIdle: Boolean;
    function CreateButton(const ACaption: String; ALeft, ATop, AWidth: Integer;
      AOnClick: TNotifyEvent): TButton;
  public
    constructor Create(AMain: TMockMainForm; AAutomatic: Boolean; const ALogPath: String); reintroduce;
    destructor Destroy; override;
  end;

var
  AutomaticPassed: Boolean;

procedure TGridInputAccess.Search(const AExpression: String);
begin
  FSearchEdit.Text := AExpression;
  SearchButtonClick(Self);
end;

constructor TMockBlockProducer.Create(ANode: TNode; AModel: TMockWalletData; ACycleRows: Boolean);
begin
  inherited Create(true);
  FNode := ANode;
  FModel := AModel;
  FCycleRows := ACycleRows;
end;

procedure TMockBlockProducer.BCExecute;
const CSizes: array[0..3] of Integer = (0, 1, 2, 200);
var LRows: Integer;
begin
  while not Terminated and not FModel.ShuttingDown do begin
    if FCycleRows then LRows := CSizes[(FCount div 4) mod Length(CSizes)] else LRows := -1;
    FModel.Advance(LRows);
    InterlockedIncrement(FCount);
    FNode.NotifyBlocksChanged;
    Sleep(100);
  end;
end;

procedure TMockBlockProducer.StopAndJoin;
begin
  Terminate;
  WaitFor;
end;

constructor TGridRefreshHarness.Create(AMain: TMockMainForm; AAutomatic: Boolean; const ALogPath: String);
begin
  inherited Create(AMain);
  FMain := AMain;
  FAutomatic := AAutomatic;
  FLogPath := ALogPath;
  FStarted := GetTickCount64;
  FAllLog := TStringList.Create;
  FPreviousDestroy := FMain.OnDestroy;
  FMain.OnDestroy := MainDestroy;

  FPanel := TPanel.Create(Self);
  FPanel.Parent := FMain.paWalletPanel;
  FPanel.Align := alTop;
  FPanel.Height := 126;
  FPanel.BevelOuter := bvNone;
  FPanel.Caption := '';
  // The host panel keeps a large startup-placeholder font. Use the root form's
  // normal UI font for the actual test controls instead of inheriting it.
  FPanel.ParentFont := false;
  FPanel.Font.Assign(FMain.Font);
  CreateButton('Start sync (100 ms)', 8, 4, 180, StartContinuous);
  CreateButton('Stop', 194, 4, 70, StopContinuous);
  CreateButton('Busy block batch (6.5 s)', 270, 4, 175, StartBusyGap);
  CreateButton('Gate next fetch', 451, 4, 130, ToggleFetchGate);
  CreateButton('Release fetch', 587, 4, 120, ReleaseFetchGate);
  CreateButton('Reset', 713, 4, 80, ResetDemo);

  FCycleRows := TCheckBox.Create(Self);
  FCycleRows.Parent := FPanel;
  FCycleRows.SetBounds(8, 38, 220, 24);
  FCycleRows.Caption := 'Cycle row counts: 0 / 1 / 2 / 200';
  FCycleRows.Checked := true;
  FSlowFetch := TCheckBox.Create(Self);
  FSlowFetch.Parent := FPanel;
  FSlowFetch.SetBounds(240, 38, 210, 24);
  FSlowFetch.Caption := 'Slow provider (7 seconds)';
  FSlowFetch.OnClick := ConfigureDelay;
  FRows := TComboBox.Create(Self);
  FRows.Parent := FPanel;
  FRows.Style := csDropDownList;
  FRows.SetBounds(465, 38, 120, 24);
  FRows.Items.CommaText := '0,1,2,200,1000';
  FRows.ItemIndex := 3;
  FRows.OnChange := ConfigureRows;
  FStatus := TLabel.Create(Self);
  FStatus.Parent := FPanel;
  FStatus.SetBounds(8, 68, 1120, 24);
  FStatus.AutoSize := false;
  FExpectation := TLabel.Create(Self);
  FExpectation.Parent := FPanel;
  FExpectation.SetBounds(8, 94, 1120, 24);
  FExpectation.AutoSize := false;
  FExpectation.Caption := 'Node synchronization pauses queries; finishing it immediately loads the latest data.';
  FLogView := TMemo.Create(Self);
  FLogView.Parent := FMain.paWalletPanel;
  FLogView.Align := alBottom;
  FLogView.Height := 116;
  FLogView.ReadOnly := true;
  FLogView.ScrollBars := ssVertical;
  FLogView.ParentFont := false;
  FLogView.Font.Assign(FMain.Font);
  FLogView.Font.Name := 'Consolas';
  FLogView.Font.Size := 9;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 50;
  FTimer.OnTimer := Tick;
  Log('START mock wallet: fresh disposable data, isolated discovery; actual factories, typed sources and inherited rendering');
end;

destructor TGridRefreshHarness.Destroy;
begin
  FTimer.Enabled := false;
  StopProducer;
  EndWork;
  FMain.Model.ReleaseFetches;
  FAllLog.Free;
  inherited;
end;

procedure TGridRefreshHarness.MainDestroy(Sender: TObject);
begin
  // Stop the producer before the root frees its wallet subscriptions/sources.
  FTimer.Enabled := false;
  StopProducer;
  EndWork;
  FMain.Model.ReleaseFetches;
  if Assigned(FPreviousDestroy) then FPreviousDestroy(Sender);
end;

function TGridRefreshHarness.CreateButton(const ACaption: String; ALeft, ATop, AWidth: Integer;
  AOnClick: TNotifyEvent): TButton;
begin
  Result := TButton.Create(Self);
  Result.Parent := FPanel;
  Result.SetBounds(ALeft, ATop, AWidth, 28);
  Result.Caption := ACaption;
  Result.OnClick := AOnClick;
  Result.Enabled := not FAutomatic;
end;

procedure TGridRefreshHarness.Log(const AMessage: String);
begin
  FMain.Model.LogEvent(AMessage);
end;

procedure TGridRefreshHarness.DrainLog;
var LScope: TDisposables; LLines: TStringList;
begin
  LLines := LScope.AddObject(TStringList.Create) as TStringList;
  FMain.Model.DrainEvents(LLines);
  if LLines.Count = 0 then Exit;
  FAllLog.AddStrings(LLines);
  FAllLog.SaveToFile(FLogPath);
  FLogView.Lines.AddStrings(LLines);
  while FLogView.Lines.Count > 100 do FLogView.Lines.Delete(0);
  FLogView.SelStart := Length(FLogView.Text);
  FLogView.Perform(EM_SCROLLCARET, 0, 0);
end;

procedure TGridRefreshHarness.Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

procedure TGridRefreshHarness.NotifyBlock;
begin
  Inc(FNotifications);
  TUserInterface.Node.NotifyBlocksChanged;
end;

procedure TGridRefreshHarness.StopProducer;
begin
  if not Assigned(FProducer) then Exit;
  FProducer.StopAndJoin;
  Inc(FNotifications, FProducer.Count);
  FreeAndNil(FProducer);
end;

procedure TGridRefreshHarness.EndWork;
begin
  if not FWorkActive then Exit;
  TUserInterface.Node.EndBlockChainUpdate;
  FWorkActive := false;
end;

procedure TGridRefreshHarness.StartContinuous(Sender: TObject);
begin
  if not FWired or Assigned(FProducer) or FWorkActive or FManualGap then
    Exit;
  ConfigureDelay(Self);
  TUserInterface.Node.BeginBlockChainUpdate;
  FWorkActive := True;
  FProducer := TMockBlockProducer.Create(TUserInterface.Node, FMain.Model, FCycleRows.Checked);
  FProducer.Start;
  Log('SYNC producer started: actual node lifecycle pauses queries while notifications arrive every100ms');
end;

procedure TGridRefreshHarness.StopContinuous(Sender: TObject);
begin
  StopProducer;
  EndWork;
  FManualGap := False;
  FPhase := apStartup;
  FStopTick := GetTickCount64;
  Log('SYNC producer stopped: actual lifecycle completion resumes the latest data without a quiet timer');
end;

procedure TGridRefreshHarness.StartBusyGap(Sender: TObject);
begin
  if not FWired or FWorkActive or FManualGap or Assigned(FProducer) then
    Exit;
  FManualGap := True;
  TUserInterface.Node.BeginBlockChainUpdate;
  FWorkActive := True;
  FMain.Model.Advance(200);
  FFinalRevision := FMain.Model.Revision;
  NotifyBlock;
  FHoldStarted := GetTickCount64;
  FPhase := apGapHold;
  Log('BUSY BATCH: actual node synchronization remains active for6.5s without provider queries');
end;

procedure TGridRefreshHarness.ToggleFetchGate(Sender: TObject);
begin
  FMain.Model.GateFetches := not FMain.Model.GateFetches;
  Log('GATE enabled=' + BoolToStr(FMain.Model.GateFetches, true));
end;

procedure TGridRefreshHarness.ReleaseFetchGate(Sender: TObject);
begin
  FMain.Model.ReleaseFetches;
  Log('GATE released');
end;

procedure TGridRefreshHarness.ResetDemo(Sender: TObject);
begin
  FManualGap := false;
  FPhase := apStartup;
  StopProducer;
  EndWork;
  FMain.Model.Reset;
  FSlowFetch.Checked := false;
  FRows.ItemIndex := 3;
  NotifyBlock;
  Log('RESET: 200 rows, no provider delay, gates open; counters remain cumulative');
end;

procedure TGridRefreshHarness.ConfigureDelay(Sender: TObject);
begin
  if FSlowFetch.Checked then FMain.Model.DelayMilliseconds := 7000
  else FMain.Model.DelayMilliseconds := 0;
end;

procedure TGridRefreshHarness.ConfigureRows(Sender: TObject);
begin
  FMain.Model.Advance(StrToInt(FRows.Text));
  NotifyBlock;
end;

function TGridRefreshHarness.GridsIdle: Boolean;
begin
  Result := Assigned(FMain.MockWallet) and
    not FMain.MockWallet.AccountsGrid.IsUpdating and
    not FMain.MockWallet.OperationsGrid.IsUpdating;
end;

procedure TGridRefreshHarness.UpdateCounters;
var A, O: TMockFetchMetrics; LNotifications: Integer;
begin
  A := FMain.Model.Metrics(mdkAccounts);
  O := FMain.Model.Metrics(mdkOperations);
  LNotifications := FNotifications;
  if Assigned(FProducer) then Inc(LNotifications, FProducer.Count);
  if Assigned(FMain.MockWallet) then
    FStatus.Caption := Format('Notifications %d | admitted A/O %d/%d | page fetches A/O %d/%d | account page / balance reads %d/%d | operation raw reads %d',
      [LNotifications, FMain.MockWallet.AccountRefreshCount, FMain.MockWallet.OperationRefreshCount, A.FetchCount, O.FetchCount,
       A.PageAccountReadCount, A.BalanceAccountReadCount, O.RawReadCount]);
  if FWorkActive then
    FExpectation.Caption := 'Actual node synchronization owns both Please Wait / Synchronizing cards and pauses queries.'
  else
    FExpectation.Caption := Format('Revision %d, rows %d | Waiting A/O %s/%s | gate %s | delay %d ms',
      [FMain.Model.Revision, FMain.Model.RowCount, BoolToStr(FMain.MockWallet.AccountsGrid.RefreshThrottle.Waiting, true),
       BoolToStr(FMain.MockWallet.OperationsGrid.RefreshThrottle.Waiting, true),
       BoolToStr(FMain.Model.GateFetches, true), FMain.Model.DelayMilliseconds]);
end;

procedure TGridRefreshHarness.CheckNodeLoading(const ACheckpoint: String);
var LSync: Boolean;
begin
  LSync := TUserInterface.Node.GetSynchronizationStatus.State = nssSynchronizing;
  Require((FMain.MockWallet.AccountsGrid.RefreshSuspended = LSync) and
    (FMain.MockWallet.OperationsGrid.RefreshSuspended = LSync),
    ACheckpoint + ': actual grid suspension differs from node synchronization');
  Require((FMain.MockWallet.AccountsGrid.LoadingVisible = LSync) and
    (FMain.MockWallet.OperationsGrid.LoadingVisible = LSync),
    ACheckpoint + ': actual manual cards differ from node synchronization');
end;

procedure TGridRefreshHarness.CaptureWaitingDemo;
{$IFNDEF FPC}
var LScope: TDisposables; LBitmap: TBitmap; LPNG: TPngImage; LPath: String;
{$ENDIF}
begin
  if FWaitingImageSaved then Exit;
  FWaitingImageSaved := true;
  {$IFNDEF FPC}
  LBitmap := LScope.AddObject(FMain.GetFormImage) as TBitmap;
  LBitmap.PixelFormat := pf24bit;
  LPNG := LScope.AddObject(TPngImage.Create) as TPngImage;
  LPNG.Assign(LBitmap);
  LPath := IncludeTrailingPathDelimiter(ExtractFilePath(FLogPath)) + 'grid-refresh-waiting.png';
  LPNG.SaveToFile(LPath);
  Log('SCREENSHOT actual native synchronization pause: ' + LPath);
  {$ENDIF}
end;

procedure TGridRefreshHarness.AutomaticTick;
var
  LNow: UInt64;
  A, O: TMockFetchMetrics;
  LMarker: String;
begin
  LNow := GetTickCount64;
  if FAutomatic then
    Require(LNow - FStarted < 117000, 'Automatic rendered-wallet deadline exceeded');
  case FPhase of
    apStartup:
      begin
        A := FMain.Model.Metrics(mdkAccounts);
        O := FMain.Model.Metrics(mdkOperations);
        if (FLastAccounts <> A.FetchCount) or (FLastOperations <> O.FetchCount) then
        begin
          FLastAccounts := A.FetchCount;
          FLastOperations := O.FetchCount;
          FLastChange := LNow;
        end;
        if not GridsIdle or (LNow - FLastChange < 1500) then
          Exit;
        if SameText(GetEnvironmentVariable('PASCAL_WALLET_KEY_NOTIFICATIONS_ONLY'), '1') then
        begin
          CheckWalletKeyNotifications(FMain, ChangeFileExt(FLogPath, '.key-notifications.log'));
          LMarker := 'PASS actual wallet and explorer background key notification checks';
        end
        else if SameText(GetEnvironmentVariable('PASCAL_WALLET_CONSECUTIVE_SELECTION_ONLY'), '1') then
        begin
          CheckConsecutiveNativeSelections(FMain, ChangeFileExt(FLogPath, '.consecutive-selection.log'));
          LMarker := 'PASS actual wallet 20 consecutive native account selection checks';
        end
        else if SameText(GetEnvironmentVariable('PASCAL_WALLET_REFRESH_LIFECYCLE_ONLY'), '1') then
        begin
          CheckWalletRefreshLifecycle(FMain, ChangeFileExt(FLogPath, '.refresh-lifecycle.log'));
          LMarker := 'PASS actual wallet node lifecycle and 250 ms operations refresh checks';
        end
        else if SameText(GetEnvironmentVariable('PASCAL_WALLET_SELECTION_COMPLETION_ONLY'), '1') then
        begin
          CheckWalletSelectionCompletion(FMain, ChangeFileExt(FLogPath, '.selection-completion.log'));
          LMarker := 'PASS actual wallet selected-account completion loading checks';
        end
        else if SameText(GetEnvironmentVariable('PASCAL_WALLET_SELECTION_ONLY'), '1') then
        begin
          CheckWalletSelectionLoadingAndBanner(FMain, ChangeFileExt(FLogPath, '.selection.log'));
          LMarker := 'PASS actual wallet selection/loading and banner checks';
        end
        else
        begin
          CheckWalletKeyNotifications(FMain, ChangeFileExt(FLogPath, '.key-notifications.log'));
          CheckWalletSelectionCompletion(FMain, ChangeFileExt(FLogPath, '.selection-completion.log'));
          CheckConsecutiveNativeSelections(FMain, ChangeFileExt(FLogPath, '.consecutive-selection.log'));
          CheckWalletRefreshLifecycle(FMain, ChangeFileExt(FLogPath, '.refresh-lifecycle.log'));
          LMarker := 'PASS actual wallet 250 ms operations admission, background coalescing and synchronization lifecycle checks';
        end;
        FPassed := True;
        AutomaticPassed := True;
        FPhase := apFinished;
        FTimer.Enabled := False;
        Log(LMarker);
        DrainLog;
        PostMessage(FMain.Handle, CM_PC_Terminate, 0, 0);
      end;
    apGapHold:
      begin
        if LNow - FHoldStarted >= 250 then
        begin
          CheckNodeLoading('Interactive active node work');
          CaptureWaitingDemo;
        end;
        if LNow - FHoldStarted >= 6500 then
        begin
          FFinalRevision := FMain.Model.Revision;
          EndWork;
          FStopTick := LNow;
          FPhase := apGapResume;
          Log('BUSY BATCH finished: the actual node lifecycle now resumes current local data');
        end;
      end;
    apGapResume:
      begin
        Require(LNow - FStopTick < 15000, 'Interactive resumed provider did not finish');
        if FMain.MockWallet.AccountsGrid.RefreshSuspended or
          FMain.MockWallet.OperationsGrid.RefreshSuspended or not GridsIdle then
          Exit;
        CheckNodeLoading('Interactive resumed data');
        if FMain.Model.RowCount > 0 then
        begin
          Require((FMain.MockWallet.AccountsGrid.RowCount > 0) and
            (FMain.MockWallet.OperationsGrid.RowCount > 0), 'Interactive resume lost current data');
          Require(Pos('mock revision=' + IntToStr(FFinalRevision) + ' ',
            String(FMain.MockWallet.AccountsGrid.Rows[0].Name)) = 1,
            'Interactive account resume retained an old revision');
          Require(Pos('mock revision=' + IntToStr(FFinalRevision) + ' ',
            String(FMain.MockWallet.OperationsGrid.Rows[0].Description)) = 1,
            'Interactive history resume retained an old revision');
        end;
        FManualGap := False;
        FPhase := apStartup;
        Log('PASS interactive node lifecycle: queries paused during work and resumed without a quiet timer');
      end;
    apFinished: ;
  end;
end;

procedure TGridRefreshHarness.Tick(Sender: TObject);
var LExpectedSource: IDataSource;
begin
  if FInTick then Exit;
  FInTick := true;
  try
    try
      if not FWired then begin
        if TUserInterface.State = uisLoading then Exit;
        Require(TUserInterface.State <> uisError, 'Fresh offline startup failed');
        FMain.Mode := wmWallet;
        if not Assigned(FMain.MockWallet) then Exit;
        LExpectedSource := FMain.MockWallet.AccountsSource;
        Require(FMain.MockWallet.AccountsGrid.DataSource = LExpectedSource,
          'Accounts grid bypassed the factory-created private wallet source');
        LExpectedSource := FMain.MockWallet.OperationsSource;
        Require(FMain.MockWallet.OperationsGrid.DataSource = LExpectedSource,
          'Operations grid bypassed the factory-created private wallet source');
        FMain.SetBounds(FMain.Left, FMain.Top, 1220, 940);
        Require(FMain.Height >= 850, 'Startup height lock was not released before laying out the mock demo');
        FWired := true; FLastChange := GetTickCount64;
        Log('READY actual root wallet and its two typed private providers; monetary mock actions disabled');
      end;
      if FAutomatic and SameText(GetEnvironmentVariable('PASCAL_WALLET_SAFEBOX_LOG_ONLY'),'1') then begin
        CheckSafeboxProgressLogging(FMain,ChangeFileExt(FLogPath,'.safebox-info.log'));
        FPassed := True;
        AutomaticPassed := True;
        FPhase := apFinished;
        FTimer.Enabled := False;
        Log('PASS actual safebox progress Info logging checks');
        DrainLog;
        PostMessage(FMain.Handle,CM_PC_Terminate,0,0);
        Exit;
      end;
      if FAutomatic and SameText(GetEnvironmentVariable('PASCAL_WALLET_SYNC_PAINT_ONLY'),'1') then begin
        CheckSynchronizationPainting(FMain,ChangeFileExt(FLogPath,'.sync-paint.log'));
        FPassed := True;
        AutomaticPassed := True;
        FPhase := apFinished;
        FTimer.Enabled := False;
        Log('PASS actual synchronization event/paint checks');
        DrainLog;
        PostMessage(FMain.Handle,CM_PC_Terminate,0,0);
        Exit;
      end;
      if FAutomatic and SameText(GetEnvironmentVariable('PASCAL_WALLET_TOOLS_ONLY'),'1') then begin
        CheckWalletToolsDialogs(FMain,ChangeFileExt(FLogPath,'.tools.log'));
        FPassed := True;
        AutomaticPassed := True;
        FPhase := apFinished;
        FTimer.Enabled := False;
        Log('PASS actual Tools forms, Change All Key wizard and Overview geometry checks');
        DrainLog;
        PostMessage(FMain.Handle,CM_PC_Terminate,0,0);
        Exit;
      end;
      if FAutomatic and SameText(GetEnvironmentVariable('PASCAL_WALLET_CHANGE_INFO_APPROVAL_ONLY'),'1') then begin
        CheckChangeAccountInfoApproval(FMain,ChangeFileExt(FLogPath,'.change-info-approval.log'));
        FPassed := True;
        AutomaticPassed := True;
        FPhase := apFinished;
        FTimer.Enabled := False;
        Log('PASS native Change Info approval: six cases, explicit skipped-target consent and optional payer');
        DrainLog;
        PostMessage(FMain.Handle,CM_PC_Terminate,0,0);
        Exit;
      end;
      if FAutomatic and SameText(GetEnvironmentVariable('PASCAL_WALLET_CHANGE_INFO_UI_ONLY'),'1') then begin
        CheckChangeAccountInfoPresentation(FMain,ChangeFileExt(FLogPath,'.change-info-ui.log'));
        FPassed := True;
        AutomaticPassed := True;
        FPhase := apFinished;
        FTimer.Enabled := False;
        Log('PASS native Change Account Info presentation: single/batch Selected and Details at 100/150 percent');
        DrainLog;
        PostMessage(FMain.Handle,CM_PC_Terminate,0,0);
        Exit;
      end;
      if FAutomatic and SameText(GetEnvironmentVariable('PASCAL_WALLET_SEND_UI_ONLY'),'1') then begin
        CheckSendPascPresentation(FMain,ChangeFileExt(FLogPath,'.send-ui.log'));
        FPassed := True;
        AutomaticPassed := True;
        FPhase := apFinished;
        FTimer.Enabled := False;
        Log('PASS native Send PASC presentation: six routes, all reachable screens at 100/150 percent');
        DrainLog;
        PostMessage(FMain.Handle,CM_PC_Terminate,0,0);
        Exit;
      end;
      if FAutomatic or FManualGap then AutomaticTick;
      UpdateCounters;
      DrainLog;
    except
      on E: Exception do begin
        StopProducer; EndWork; FMain.Model.ReleaseFetches;
        Log('FAIL ' + E.ClassName + ': ' + E.Message);
        DrainLog;
        FTimer.Enabled := false;
        if FAutomatic then PostMessage(FMain.Handle, CM_PC_Terminate, 0, 0)
        else FExpectation.Caption := 'FAILED: ' + E.Message;
      end;
    end;
  finally FInTick := false; end;
end;

function DesktopName(AHandle: THandle): String;
var LNeeded: DWORD; LBuffer: array[0..255] of Char;
begin
  if not GetUserObjectInformation(AHandle, UOI_NAME, @LBuffer[0], SizeOf(LBuffer), LNeeded) then
    RaiseLastOSError;
  Result := StrPas(PChar(@LBuffer[0]));
end;

procedure RequirePrivateDesktop;
var LInput: HDESK;
begin
  LInput := OpenInputDesktop(0, false, DESKTOP_READOBJECTS);
  if LInput = 0 then RaiseLastOSError;
  try
    if SameText(DesktopName(GetThreadDesktop(GetCurrentThreadId)), DesktopName(LInput)) then
      raise Exception.Create('Automatic UI checks require the existing private-desktop test broker');
  finally CloseDesktop(LInput); end;
end;

function RunWalletGridRefreshDemo: Integer;
var
  LMain: TMockMainForm;
  LOptions: TWalletStartupOptions;
  LArguments: array of String;
  LAutomatic, LToolsOnly: Boolean;
  LDirectory, LLogPath, LError: String;
  LID: TGUID;
begin
  Result := 1;
  LAutomatic := ParamCount <> 0;
  LToolsOnly := LAutomatic and SameText(GetEnvironmentVariable('PASCAL_WALLET_TOOLS_ONLY'),'1');
  if ParamCount > 1 then raise EArgumentException.Create('Use no arguments for the interactive demo, or one automatic log filename');
  if LAutomatic then RequirePrivateDesktop;
  CreateGUID(LID);
  LDirectory := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) +
    'PascalCoinWalletQA_GridRefresh_' + StringReplace(StringReplace(GUIDToString(LID), '{', '', []), '}', '', []);
  if DirectoryExists(LDirectory) then raise Exception.Create('Refusing an existing mock wallet directory');
  ForceDirectories(LDirectory);
  if not LAutomatic or (ParamStr(1) = '--auto') then LLogPath := LDirectory + PathDelim + 'grid-refresh.log'
  else if Pos('--auto=', ParamStr(1)) = 1 then LLogPath := ExpandFileName(Copy(ParamStr(1), 8, MaxInt))
  else if Pos('--', ParamStr(1)) = 1 then raise EArgumentException.Create('Unsupported demo argument')
  else LLogPath := ExpandFileName(ParamStr(1));
  if FileExists(LLogPath) then raise Exception.Create('Refusing an existing test log');
  if not DirectoryExists(ExtractFilePath(LLogPath)) then raise Exception.Create('The test output directory must exist');
  if LAutomatic and SameText(GetEnvironmentVariable('PASCAL_WALLET_OPERATION_MESSAGE_ONLY'), '1') then
  begin
    // Message-only checks enter before wallet startup, mock providers or a node.
    Application.Initialize;
    Application.MainFormOnTaskBar := True;
    Application.Title := 'Pascal Coin Wallet, JSON-RPC Miner & Explorer';
    try
      CheckWalletOperationMessages(nil, LLogPath);
      Result := 0;
    except
      Result := 1;
    end;
    Exit;
  end;
  if LAutomatic and SameText(GetEnvironmentVariable('PASCAL_WALLET_ADVANCED_UI_ONLY'), '1') then
  begin
    // Advanced presentation also runs without starting a wallet or node.
    Application.Initialize;
    Application.MainFormOnTaskBar := True;
    Application.Title := 'Pascal Coin Wallet, JSON-RPC Miner & Explorer';
    try
      CheckAdvancedOperationPresentation(LLogPath);
      Result := 0;
    except
      Result := 1;
    end;
    Exit;
  end;
  if LToolsOnly then begin
    SeedWalletToolsSettings(LDirectory);
    SetLength(LArguments, 2);
    LArguments[1] := '--no-startup-dialogs';
  end else begin
    SetLength(LArguments, 3);
    LArguments[1] := '--offline';
    LArguments[2] := '--no-startup-dialogs';
  end;
  LArguments[0] := '--data-dir=' + LDirectory;
  if not TryParseWalletStartupOptions(LArguments, LOptions, LError) then raise Exception.Create(LError);
  TUserInterface.ConfigureStartup(LOptions);
  Application.Initialize;
  Application.MainFormOnTaskBar := true;
  Application.CreateForm(TMockMainForm, LMain);
  TGridRefreshHarness.Create(LMain, LAutomatic, LLogPath);
  try
    TUserInterface.StartApplication(LMain);
    if TUserInterface.Started then Application.Run;
    if not LAutomatic or AutomaticPassed then Result := 0;
  finally TUserInterface.ExitApplication; end;
end;

end.
