program WalletStartupProbe;

{ Copyright (c) 2026 by Herman Schoenfeld

  Actual wallet startup checks on a private Windows desktop with fresh temp data.
  The probe refuses the input desktop, normal wallet folders and online mode.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

{$mode delphi}

uses
  Interfaces, Forms, Controls, ExtCtrls, StdCtrls, ComCtrls, Graphics, Windows, SysUtils, Classes, StrUtils,
  UWalletStartup, UWalletStartupTask, UUserInterface, UFRMMainForm, UCTRLWallet, UCTRLSyncronization,
  USettings, UWallet, UNode, UNetProtocol, UAppParams, UConst, UCommon, UMemory, UThread, SyncObjs,
  UFRMMessages, UFRMLogs, UFRMBlockExplorer, UFRMAccountExplorer, UFRMOperationsExplorer,
  UCTRLBanner, UVisualGrid, UWalletKeysIconChecks, UWalletFooterChecks, UWalletSyncDisplayChecks,
  UAccounts, UProgressDisplay;

type

  TPanelAccess = class(TPanel)
  public
    function ContentRect : TRect;
  end;

  TVisualGridAccess = class(TVisualGrid);
  TMainFormAccess = class(TFRMMainForm);

  TSlowStartupTask = class(TWalletStartupTask)
  private
    FEntered : TEvent;
  protected
    procedure ExecuteLoad; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TBusyMempoolThread = class(TPCThread)
  private
    FNode : TNode;
    FWatchdogReleased : LongInt;
  protected
    procedure BCExecute; override;
  public
    Entered, ReleaseLock, Released : TEvent;
    constructor Create(ANode : TNode);
    destructor Destroy; override;
    function WatchdogReleased : Boolean;
  end;

  TStartupHarness = class(TComponent)
  private
    FDisposables : TDisposables;
    FLog : TStringList;
    FLogPath, FMode, FDataDirectory : String;
    FMainForm : TFRMMainForm;
    FTimer : TTimer;
    FModalTimer : TTimer;
    FModalSeen : Boolean;
    FModalDeadline : QWord;
    FModalFailure : String;
    FLoadedCount, FPhase : Integer;
    FProgressSeen : Boolean;
    FLastProgress : TProgressInfo;
    FSnapshotNotifications : Integer;
    FDeadline : QWord;
    FInsideTick, FExited : Boolean;
    FFailure : String;
    FHost : HWND;
    FHostLeft, FHostTop : Integer;
    FOriginalMinHeight, FOriginalMaxHeight : Integer;
    FOriginalMinWidth, FOriginalMaxWidth : Integer;
    FOriginalBorderIcons : TBorderIcons;
    FSlowTask : TSlowStartupTask;
    FQuitBusy : TBusyMempoolThread;
    FFooterConnectivityClick, FFooterSyncClick : TNotifyEvent;
    FFooterConnectivityClicks, FFooterSyncClicks : Integer;
    procedure FooterConnectivityClick(Sender : TObject);
    procedure FooterSyncClick(Sender : TObject);
    procedure Log(const AText : String);
    procedure Require(ACondition : Boolean; const AMessage : String);
    procedure OnLoaded(Sender : TObject);
    procedure OnLoading(Sender : TObject; const AProgress : TProgressInfo);
    procedure OnAppStarted(Sender : TObject);
    procedure OnException(Sender : TObject; E : Exception);
    procedure Tick(Sender : TObject);
    procedure InspectWorkbench(Sender : TObject);
    procedure InspectQuit(Sender : TObject);
    procedure CheckOffline(const ACheckpoint : String);
    procedure CheckNative(const ACheckpoint : String; AMode : TFRMMainFormMode;
      ARequireRestoredPosition : Boolean = true);
    procedure CheckFrameLayout(const ACheckpoint : String; ARequireRestoredPosition : Boolean);
    procedure CheckFooterActions;
    procedure CheckFooterDpiCycle;
    procedure CheckResizeCycle(const ACheckpoint : String; AMode : TFRMMainFormMode;
      ALoading : Boolean = false);
    procedure PumpWindowChanges;
    procedure CheckModelessFocus;
    procedure CheckBusyQuit;
    procedure CheckClassicIntegration;
    procedure CheckBusyStatisticsRefresh;
    procedure CheckEventDrivenStatuses;
    procedure OnSyncSnapshotChanged(Sender: TObject);
    procedure CheckShutdown;
    procedure SeedSettings;
    procedure StopWallet;
  public
    constructor Create(const AMode, ALogPath, ADataDirectory : String);
    destructor Destroy; override;
    procedure Run(const AOptions : TWalletStartupOptions);
  end;

var
  GAllowedWindow, GUnexpectedWindow : HWND;
  GUnexpectedClass, GUnexpectedCaption : String;
  GSlowLoadCancelled : Boolean;

constructor TSlowStartupTask.Create;
begin
  inherited Create;
  FEntered := TEvent.Create(nil, true, false, '');
end;

destructor TSlowStartupTask.Destroy;
begin
  CancelAndWait;
  FEntered.Free;
  inherited;
end;

procedure TSlowStartupTask.ExecuteLoad;
var
  LNode : TNode;
begin
  LNode := TUserInterface.Node;
  try
    LNode.LockMempoolWrite;
    try
      LNode.Bank.SafeBox.StartThreadSafe;
      try
        ReportProgress(TProgressInfo.Create(psReadingBlockchain, 1, 100));
        FEntered.SetEvent;
        while not Cancelled do Sleep(10);
        CheckCancelled;
      finally
        LNode.Bank.SafeBox.EndThreadSave;
      end;
    finally
      LNode.UnlockMempoolWrite;
    end;
  finally
    GSlowLoadCancelled := Cancelled;
  end;
end;

constructor TBusyMempoolThread.Create(ANode : TNode);
begin
  FNode := ANode;
  Entered := TEvent.Create(nil, true, false, '');
  ReleaseLock := TEvent.Create(nil, true, false, '');
  Released := TEvent.Create(nil, true, false, '');
  inherited Create(true);
  FreeOnTerminate := false;
  Start;
end;

destructor TBusyMempoolThread.Destroy;
begin
  ReleaseLock.SetEvent;
  WaitFor;
  Entered.Free;
  ReleaseLock.Free;
  Released.Free;
  inherited;
end;

function TBusyMempoolThread.WatchdogReleased : Boolean;
begin
  Result := InterlockedCompareExchange(FWatchdogReleased, 0, 0) <> 0;
end;

procedure TBusyMempoolThread.BCExecute;
begin
  if not FNode.TryLockNode(1000) then begin
    Released.SetEvent;
    Exit;
  end;
  try
    Entered.SetEvent;
    // A broken UI refresh must fail the probe instead of hanging its desktop.
    if ReleaseLock.WaitFor(2000) <> wrSignaled then
      InterlockedExchange(FWatchdogReleased, 1);
  finally
    FNode.UnlockNode;
    Released.SetEvent;
  end;
end;

function DesktopName(ADesktop : HDESK) : String;
var
  LName : array[0..255] of Char;
  LNeeded : DWORD;
begin
  FillChar(LName, SizeOf(LName), 0);
  if not GetUserObjectInformation(ADesktop, UOI_NAME, @LName[0], SizeOf(LName), LNeeded) then
    RaiseLastOSError;
  Result := StrPas(@LName[0]);
end;

function FindUnexpectedWindow(AWindow : HWND; AParameter : LPARAM) : WINBOOL; stdcall;
var
  LProcessId : DWORD;
  LWindowClass : array[0..127] of Char;
  LWindowCaption : array[0..255] of Char;
begin
  Result := true;
  LProcessId := 0;
  GetWindowThreadProcessId(AWindow, @LProcessId);
  if (LProcessId <> GetCurrentProcessId) or (not Windows.IsWindowVisible(AWindow)) or
    (AWindow = GAllowedWindow) then Exit;
  FillChar(LWindowClass, SizeOf(LWindowClass), 0);
  GetClassName(AWindow, @LWindowClass[0], Length(LWindowClass));
  // Windows may create its own IME overlay in a GUI process on a private desktop.
  if (StrPas(@LWindowClass[0]) = 'UAC_InputIndicatorOverlayWnd') or
    (StrPas(@LWindowClass[0]) = 'UAC Input Indicator') then Exit;
  GUnexpectedWindow := AWindow;
  GUnexpectedClass := StrPas(@LWindowClass[0]);
  FillChar(LWindowCaption, SizeOf(LWindowCaption), 0);
  GetWindowText(AWindow, @LWindowCaption[0], Length(LWindowCaption));
  GUnexpectedCaption := StrPas(@LWindowCaption[0]);
  Result := false;
end;

function TPanelAccess.ContentRect : TRect;
begin
  Result := ClientRect;
  AdjustClientRect(Result);
end;

constructor TStartupHarness.Create(const AMode, ALogPath, ADataDirectory : String);
begin
  inherited Create(nil);
  FMode := AMode;
  FLogPath := ALogPath;
  FDataDirectory := ADataDirectory;
  FLog := FDisposables.AddObject(TStringList.Create) as TStringList;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := false;
  FTimer.Interval := 50;
  FTimer.OnTimer := Tick;
  FModalTimer := TTimer.Create(Self);
  FModalTimer.Enabled := false;
  FModalTimer.Interval := 25;
  FModalTimer.OnTimer := InspectWorkbench;
end;

destructor TStartupHarness.Destroy;
begin
  FTimer.Enabled := false;
  FModalTimer.Enabled := false;
  TUserInterface.Loaded.Remove(OnLoaded);
  TUserInterface.AppStarted.Remove(OnAppStarted);
  TUserInterface.Loading.Remove(OnLoading);
  inherited;
end;

procedure TStartupHarness.InspectWorkbench(Sender : TObject);
var
  LWorkbench : TFRMOperationsExplorer;
  i : Integer;
begin
  LWorkbench := nil;
  for i := 0 to Screen.FormCount - 1 do
    if (Screen.Forms[i] is TFRMOperationsExplorer) and Screen.Forms[i].Visible then
      LWorkbench := TFRMOperationsExplorer(Screen.Forms[i]);
  if not Assigned(LWorkbench) then begin
    if GetTickCount64 >= FModalDeadline then begin
      FModalFailure := 'The composed RAW workbench did not enter its modal loop';
      FModalTimer.Enabled := false;
      Application.Terminate;
    end;
    Exit;
  end;
  FModalTimer.Enabled := false;
  try
    Require(LWorkbench.SourceNode = TUserInterface.Node, 'Workbench received the wrong node');
    Require(LWorkbench.SourceWalletKeys = TWallet.Keys, 'Workbench received the wrong wallet keys');
    Require(LWorkbench.PopupParent = FMainForm, 'Workbench is not owned by its main popup host');
    Require(LWorkbench.Offline and (not LWorkbench.MiRPCCalls.Enabled) and
      (not LWorkbench.ActExecuteOperation.Enabled), 'Workbench outbound actions bypassed offline policy');
    // A state refresh during a modal loop must not globally re-enable its host.
    TUserInterface.StateChanged.Invoke(nil);
    Require(not Windows.IsWindowEnabled(FMainForm.Handle), 'UI refresh broke modal host input ownership');
    FModalSeen := true;
  except
    on E : Exception do FModalFailure := E.Message;
  end;
  LWorkbench.ModalResult := mrCancel;
end;

procedure TStartupHarness.Log(const AText : String);
begin
  FLog.Add(IntToStr(GetTickCount64) + ' ' + AText);
  FLog.SaveToFile(FLogPath);
end;

procedure TStartupHarness.Require(ACondition : Boolean; const AMessage : String);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

procedure TStartupHarness.OnLoaded(Sender : TObject);
begin
  Inc(FLoadedCount);
  Log('EVENT Loaded ' + IntToStr(FLoadedCount));
end;

procedure TStartupHarness.OnLoading(Sender : TObject; const AProgress : TProgressInfo);
begin
  Require(GetCurrentThreadID = MainThreadID, 'Typed progress escaped main-thread dispatch');
  FLastProgress := AProgress;
  FProgressSeen := true;
end;

procedure TStartupHarness.OnAppStarted(Sender : TObject);
begin
  if FMode = 'close-before-load' then begin
    Require(TUserInterface.State = uisLoading, 'Expected loading state in AppStarted');
    StopWallet;
  end;
end;

procedure TStartupHarness.OnException(Sender : TObject; E : Exception);
begin
  FFailure := E.ClassName + ': ' + E.Message;
  Log('UNHANDLED ' + FFailure);
  FTimer.Enabled := false;
  StopWallet;
end;

procedure TStartupHarness.CheckOffline(const ACheckpoint : String);
begin
  Require(TUserInterface.Offline, ACheckpoint + ': offline option lost');
  Require(not TNetData.NetData.NetConnectionsActive, ACheckpoint + ': connections enabled');
  Require(not TUserInterface.Node.NetServer.Active, ACheckpoint + ': peer listener enabled');
  Require(not TNetData.NetData.IsDiscoveringServers, ACheckpoint + ': discovery enabled');
  Require(TNetData.NetData.ConnectionsCountAll = 0, ACheckpoint + ': network connection created');
  Require(TNetData.NetData.NodeServersAddresses.Count = 0, ACheckpoint + ': cached peers supplied offline');
  Require(not TUserInterface.RPCServer.Active, ACheckpoint + ': RPC enabled');
  Require(Assigned(TUserInterface.PoolMiningServer) and
    (not TUserInterface.PoolMiningServer.Active), ACheckpoint + ': mining enabled');
  Require(TSettings.JsonRpcPortEnabled, ACheckpoint + ': saved RPC preference overwritten');
  Require(TSettings.JsonRpcMinerServerActive, ACheckpoint + ': saved mining preference overwritten');
  Require(TSettings.PeerCache = '127.0.0.1:1', ACheckpoint + ': saved peers overwritten');
  Require(TWallet.Keys.Count = 0, ACheckpoint + ': offline startup generated a wallet key');
  Require(not FMainForm.tbtnConnectivity.Enabled, ACheckpoint + ': connectivity button enabled offline');
  Log('OFFLINE ' + ACheckpoint + ': peers=0, connections=0, listeners/RPC/mining inactive, saved preferences preserved');
end;

procedure TStartupHarness.CheckNative(const ACheckpoint : String; AMode : TFRMMainFormMode;
  ARequireRestoredPosition : Boolean);
var
  LActive, LInactive : TCustomForm;
  LPanel : TPanel;
  LHostRect, LChildRect, LContent : TRect;
  LOrigin : TPoint;
begin
  Require(FMainForm.Visible and FMainForm.Enabled, ACheckpoint + ': main form hidden or disabled');
  Require(GetWindowRect(FMainForm.Handle, LHostRect), ACheckpoint + ': no host rectangle');
  if FHost = 0 then begin
    FHost := FMainForm.Handle;
    FHostLeft := LHostRect.Left;
    FHostTop := LHostRect.Top;
  end;
  Require(FMainForm.Handle = FHost, ACheckpoint + ': main HWND changed');
  if ARequireRestoredPosition then
    Require((LHostRect.Left = FHostLeft) and (LHostRect.Top = FHostTop),
      ACheckpoint + ': main form did not preserve its restored top-left');
  Require(FMainForm.Mode = AMode, ACheckpoint + ': wrong application mode');
  Require(not FMainForm.SyncControl.ShowSplash, ACheckpoint + ': synchronization reopened splash');
  Require(FMainForm.paWalletPanel.ControlCount = 1, ACheckpoint + ': wallet was not created once');
  if AMode = wmWallet then begin
    LPanel := FMainForm.paWalletPanel;
    LActive := LPanel.Controls[0] as TCTRLWallet;
    LInactive := FMainForm.SyncControl;
  end else begin
    LPanel := FMainForm.paSyncPanel;
    LActive := FMainForm.SyncControl;
    LInactive := FMainForm.paWalletPanel.Controls[0] as TCTRLWallet;
  end;
  Require(Windows.IsWindowVisible(LActive.Handle), ACheckpoint + ': active screen hidden');
  Require(not Windows.IsWindowVisible(LInactive.Handle), ACheckpoint + ': inactive screen remained visible');
  Require(LActive.Parent = LPanel, ACheckpoint + ': wrong LCL screen parent');
  Require(Windows.GetParent(LActive.Handle) = LPanel.Handle, ACheckpoint + ': wrong native screen parent');
  Require((GetWindowLongPtr(LActive.Handle, GWL_STYLE) and WS_CHILD) <> 0,
    ACheckpoint + ': active screen became a standalone popup');
  LContent := TPanelAccess(LPanel).ContentRect;
  Require((LActive.Left = LContent.Left) and (LActive.Top = LContent.Top) and
    (LActive.Width = LContent.Right - LContent.Left) and
    (LActive.Height = LContent.Bottom - LContent.Top), ACheckpoint + ': logical screen bounds do not fill panel');
  LOrigin := Point(LContent.Left, LContent.Top);
  Require(Windows.ClientToScreen(LPanel.Handle, LOrigin), ACheckpoint + ': no native panel origin');
  Require(GetWindowRect(LActive.Handle, LChildRect), ACheckpoint + ': no native child bounds');
  Require((LChildRect.Left = LOrigin.X) and (LChildRect.Top = LOrigin.Y),
    ACheckpoint + ': native screen moved outside panel');
  Require((LChildRect.Right - LChildRect.Left = LContent.Right - LContent.Left) and
    (LChildRect.Bottom - LChildRect.Top = LContent.Bottom - LContent.Top),
    ACheckpoint + ': native screen size does not fill panel');
  GAllowedWindow := FHost;
  GUnexpectedWindow := 0;
  EnumWindows(@FindUnexpectedWindow, 0);
  Require(GUnexpectedWindow = 0, ACheckpoint + ': unexpected visible top-level window ' +
    IntToStr(GUnexpectedWindow) + ' class=' + GUnexpectedClass + ' caption=' + GUnexpectedCaption);
  CheckFrameLayout(ACheckpoint, ARequireRestoredPosition);
  Log(Format('NATIVE %s: stable HWND=%d at %d,%d; child=%d,%d,%d,%d; inactive hidden',
    [ACheckpoint, FHost, LHostRect.Left, LHostRect.Top, LChildRect.Left, LChildRect.Top,
     LChildRect.Right, LChildRect.Bottom]));
end;

procedure TStartupHarness.PumpWindowChanges;
var LUntil: QWord;
begin
  // Native size messages and existing delayed layout timers must both settle.
  LUntil := GetTickCount64 + 100;
  repeat
    Application.ProcessMessages;
    CheckSynchronize;
    Sleep(1);
  until GetTickCount64 >= LUntil;
end;

procedure TStartupHarness.CheckFrameLayout(const ACheckpoint : String;
  ARequireRestoredPosition : Boolean);
var
  LBanner: TCTRLBanner;
  LHostRect, LBannerRect, LHeaderContent, LStatusRect, LToolbarRect: TRect;
  LOrigin, LStatusOrigin: TPoint;
  LStatusClient: TRect;
  LButton: TToolButton;
  LButtons: array[0..2] of TToolButton;
  LFooterGeometry: TWalletFooterGeometry;
  i: Integer;
begin
  Require(GetWindowRect(FMainForm.Handle, LHostRect), ACheckpoint + ': no native main bounds');
  if FHost = 0 then begin
    FHost := FMainForm.Handle;
    FHostLeft := LHostRect.Left;
    FHostTop := LHostRect.Top;
  end;
  Require(FMainForm.Handle = FHost, ACheckpoint + ': main HWND changed while resizing');
  if ARequireRestoredPosition then
    Require((LHostRect.Left = FHostLeft) and (LHostRect.Top = FHostTop),
      ACheckpoint + ': restored main position changed');
  Require((FMainForm.paLogoPanel.Left = 0) and (FMainForm.paLogoPanel.Top = 0) and
    (FMainForm.paLogoPanel.Width = FMainForm.ClientWidth),
    ACheckpoint + ': header does not span the form client width');
  Require(FMainForm.paLogoPanel.ControlCount = 1, ACheckpoint + ': missing or duplicate banner');
  Require(FMainForm.paLogoPanel.Controls[0] is TCTRLBanner, ACheckpoint + ': wrong header control');
  LBanner := TCTRLBanner(FMainForm.paLogoPanel.Controls[0]);
  LHeaderContent := TPanelAccess(FMainForm.paLogoPanel).ContentRect;
  Require((LBanner.Left = LHeaderContent.Left) and (LBanner.Top = LHeaderContent.Top) and
    (LBanner.Width = LHeaderContent.Right - LHeaderContent.Left) and
    (LBanner.Height = LHeaderContent.Bottom - LHeaderContent.Top),
    ACheckpoint + ': banner leaves an uncovered header region');
  LOrigin := Point(LHeaderContent.Left, LHeaderContent.Top);
  Require(Windows.ClientToScreen(FMainForm.paLogoPanel.Handle, LOrigin),
    ACheckpoint + ': no native header origin');
  Require(GetWindowRect(LBanner.Handle, LBannerRect), ACheckpoint + ': no native banner bounds');
  Require((Windows.GetParent(LBanner.Handle) = FMainForm.paLogoPanel.Handle) and
    (LBannerRect.Left = LOrigin.X) and (LBannerRect.Top = LOrigin.Y) and
    (LBannerRect.Right - LBannerRect.Left = LBanner.Width) and
    (LBannerRect.Bottom - LBannerRect.Top = LBanner.Height),
    ACheckpoint + ': native banner does not fill its header host');
  Require((LBanner.imBackground.Left = 0) and (LBanner.imBackground.Top = 0) and
    (LBanner.imBackground.Width = LBanner.ClientWidth) and
    (LBanner.imBackground.Height = LBanner.ClientHeight),
    ACheckpoint + ': background image leaves a blank header region');
  Require(Abs(LBanner.ClientWidth - LBanner.cbLanguage.Left - LBanner.cbLanguage.Width -
    LBanner.imLogo.Left) <= 1, ACheckpoint + ': language selector lost its right inset');
  Require((LBanner.cbLanguage.Top >= 0) and
    (LBanner.cbLanguage.Top + LBanner.cbLanguage.Height <= LBanner.ClientHeight) and
    (Abs(2 * LBanner.cbLanguage.Top + LBanner.cbLanguage.Height - LBanner.ClientHeight) <= 1),
    ACheckpoint + ': language selector is outside or off-center in header');
  Require((FMainForm.sbStatusBar.Parent = FMainForm) and
    (FMainForm.sbStatusBar.Left = 0) and
    (FMainForm.sbStatusBar.Width = FMainForm.ClientWidth) and
    (FMainForm.sbStatusBar.Top + FMainForm.sbStatusBar.Height = FMainForm.ClientHeight),
    ACheckpoint + ': status bar is detached from the form bottom');
  Require((FMainForm.tbStatusToolBar.Parent = FMainForm.sbStatusBar) and
    (Windows.GetParent(FMainForm.tbStatusToolBar.Handle) = FMainForm.sbStatusBar.Handle),
    ACheckpoint + ': footer icons do not belong to the status bar');
  LStatusClient := FMainForm.sbStatusBar.ClientRect;
  LStatusOrigin := Point(LStatusClient.Left, LStatusClient.Top);
  Require(Windows.ClientToScreen(FMainForm.sbStatusBar.Handle, LStatusOrigin),
    ACheckpoint + ': no native status client origin');
  LStatusRect := Rect(LStatusOrigin.X, LStatusOrigin.Y,
    LStatusOrigin.X + FMainForm.sbStatusBar.ClientWidth,
    LStatusOrigin.Y + FMainForm.sbStatusBar.ClientHeight);
  Require(GetWindowRect(FMainForm.tbStatusToolBar.Handle, LToolbarRect),
    ACheckpoint + ': no native footer toolbar bounds');
  Require((LToolbarRect.Left >= LStatusRect.Left) and (LToolbarRect.Top >= LStatusRect.Top) and
    (LToolbarRect.Right <= LStatusRect.Right) and (LToolbarRect.Bottom <= LStatusRect.Bottom),
    ACheckpoint + ': footer icons float outside the native status bar');
  Require(LStatusRect.Right - LToolbarRect.Right <= 32,
    ACheckpoint + ': footer icons did not follow the right edge');
  LFooterGeometry := ReadWalletFooterGeometry(FMainForm.sbStatusBar, FMainForm.tbStatusToolBar);
  Log('FOOTER CONTENT ' + ACheckpoint + ': ' + LFooterGeometry.Description);
  RequireWalletFooterContent(LFooterGeometry, ACheckpoint);
  Require(FMainForm.tbStatusToolBar.ButtonCount = 3, ACheckpoint + ': unexpected footer button count');
  LButtons[0] := FMainForm.tbtnWalletLock;
  LButtons[1] := FMainForm.tbtnConnectivity;
  LButtons[2] := FMainForm.tbtnSync;
  for i := 0 to High(LButtons) do begin
    LButton := LButtons[i];
    Require(FMainForm.tbStatusToolBar.Buttons[i] = LButton,
      ACheckpoint + ': footer order differs from its visible positions');
    Require((LButton.Left >= 0) and (LButton.Top >= 0) and
      (LButton.Left + LButton.Width <= FMainForm.tbStatusToolBar.ClientWidth) and
      (LButton.Top + LButton.Height <= FMainForm.tbStatusToolBar.ClientHeight),
      ACheckpoint + ': footer button is clipped');
    Require((LButton.Width >= FMainForm.ilSmallIcons.Width) and
      (LButton.Height >= FMainForm.ilSmallIcons.Height + 2),
      ACheckpoint + ': footer clips the original icon');
    Require(FMainForm.tbStatusToolBar.ControlAtPos(Point(LButton.Left + LButton.Width div 2,
      LButton.Top + LButton.Height div 2), true) = LButton,
      ACheckpoint + ': footer icon center identifies a different button');
  end;
  Log(Format('LAYOUT %s: header=%dx%d language=%d,%d; status=%d,%d,%d,%d toolbar=%d,%d,%d,%d',
    [ACheckpoint, LBanner.Width, LBanner.Height, LBanner.cbLanguage.Left, LBanner.cbLanguage.Top,
     LStatusRect.Left, LStatusRect.Top, LStatusRect.Right, LStatusRect.Bottom,
     LToolbarRect.Left, LToolbarRect.Top, LToolbarRect.Right, LToolbarRect.Bottom]));
end;

procedure TStartupHarness.FooterConnectivityClick(Sender : TObject);
begin
  Require(Sender = FMainForm.tbtnConnectivity, 'Connectivity icon dispatched to a different button');
  Inc(FFooterConnectivityClicks);
  FFooterConnectivityClick(Sender);
end;

procedure TStartupHarness.FooterSyncClick(Sender : TObject);
begin
  Require(Sender = FMainForm.tbtnSync, 'Synchronization icon dispatched to a different button');
  Inc(FFooterSyncClicks);
  FFooterSyncClick(Sender);
end;

procedure TStartupHarness.CheckFooterActions;
var
  LConnectivityEnabled, LWasConnected: Boolean;
  LMode: TFRMMainFormMode;
  LExpected: TNotifyEvent;
  LOldNetworkImage: Integer;
  LOldNetworkHint: String;
  i: Integer;

  procedure NativeClick(AButton: TToolButton);
  var
    LPosition: LPARAM;
  begin
    LPosition := MakeLParam(AButton.Left + AButton.Width div 2,
      AButton.Top + AButton.Height div 2);
    SendMessage(FMainForm.tbStatusToolBar.Handle, WM_LBUTTONDOWN, MK_LBUTTON, LPosition);
    SendMessage(FMainForm.tbStatusToolBar.Handle, WM_LBUTTONUP, 0, LPosition);
    PumpWindowChanges;
  end;

begin
  Require(TUserInterface.Offline, 'Footer hit checks require an offline owned fixture');
  FFooterConnectivityClick := FMainForm.tbtnConnectivity.OnClick;
  FFooterSyncClick := FMainForm.tbtnSync.OnClick;
  LExpected := FMainForm.tbtnConnectivityClick;
  Require((TMethod(FFooterConnectivityClick).Code = TMethod(LExpected).Code) and
    (TMethod(FFooterConnectivityClick).Data = TMethod(LExpected).Data),
    'Connectivity button has the wrong production handler');
  LExpected := FMainForm.tbtnSyncClick;
  Require((TMethod(FFooterSyncClick).Code = TMethod(LExpected).Code) and
    (TMethod(FFooterSyncClick).Data = TMethod(LExpected).Data),
    'Synchronization button has the wrong production handler');
  Require((FMainForm.tbStatusToolBar.Images = FMainForm.ilSmallIcons) and
    (FMainForm.tbtnConnectivity.ImageIndex = 1) and (FMainForm.tbtnSync.ImageIndex = 4),
    'Disconnected/synchronization footer uses the wrong original artwork');
  LConnectivityEnabled := FMainForm.tbtnConnectivity.Enabled;
  LOldNetworkImage := FMainForm.tbtnConnectivity.ImageIndex;
  LOldNetworkHint := FMainForm.tbtnConnectivity.Hint;
  LWasConnected := TNetData.NetData.NetConnectionsActive;
  LMode := FMainForm.Mode;
  FMainForm.tbtnConnectivity.OnClick := FooterConnectivityClick;
  FMainForm.tbtnSync.OnClick := FooterSyncClick;
  try
    // Both original connection glyphs must keep the same native hit target.
    // Only presentation is simulated; the real offline handler is delegated.
    FMainForm.tbtnConnectivity.Enabled := true;
    for i := 0 to 1 do begin
      FMainForm.tbtnConnectivity.ImageIndex := i;
      if i = 0 then FMainForm.tbtnConnectivity.Hint := 'Network is active. Click to deactivate.'
      else FMainForm.tbtnConnectivity.Hint := 'Network is inactive. Click to activate.';
      PumpWindowChanges;
      FFooterConnectivityClicks := 0;
      FFooterSyncClicks := 0;
      NativeClick(FMainForm.tbtnConnectivity);
      Require((FFooterConnectivityClicks = 1) and (FFooterSyncClicks = 0),
        'Clicking a connection glyph invoked synchronization');
      Require(FMainForm.Mode = LMode, 'Connection glyph changed the displayed screen');
      Require(TNetData.NetData.NetConnectionsActive = LWasConnected,
        'Footer probe changed offline connectivity');
    end;
    FFooterConnectivityClicks := 0;
    FFooterSyncClicks := 0;
    NativeClick(FMainForm.tbtnSync);
    Require((FFooterSyncClicks = 1) and (FFooterConnectivityClicks = 0),
      'Clicking the synchronization glyph invoked connectivity');
    Require(FMainForm.Mode <> LMode, 'Synchronization glyph did not switch the screen');
    Require(TNetData.NetData.NetConnectionsActive = LWasConnected,
      'Synchronization glyph changed connectivity');
    NativeClick(FMainForm.tbtnSync);
    Require((FFooterSyncClicks = 2) and (FFooterConnectivityClicks = 0) and
      (FMainForm.Mode = LMode), 'Second synchronization click did not restore the screen');
  finally
    FMainForm.tbtnConnectivity.OnClick := FFooterConnectivityClick;
    FMainForm.tbtnSync.OnClick := FFooterSyncClick;
    FMainForm.tbtnConnectivity.ImageIndex := LOldNetworkImage;
    FMainForm.tbtnConnectivity.Hint := LOldNetworkHint;
    FMainForm.tbtnConnectivity.Enabled := LConnectivityEnabled;
  end;
  CheckOffline('after native footer icon clicks');
  Log('FOOTER: original 16px icons fit; connected/inactive glyph hit targets match connectivity; synchronization native clicks switch views twice without changing networking');
end;

procedure TStartupHarness.CheckResizeCycle(const ACheckpoint : String;
  AMode : TFRMMainFormMode; ALoading : Boolean);
var
  LRestored, LAfter: TRect;
  LHeight, LWidth: Integer;
  LMinMax: TMinMaxInfo;
begin
  Require(FMainForm.WindowState = wsNormal, ACheckpoint + ': expected restored starting state');
  Require(GetWindowRect(FMainForm.Handle, LRestored), ACheckpoint + ': cannot save restored bounds');
  CheckFrameLayout(ACheckpoint + '-normal', true);
  Require(CheckWalletFooterPaint(FMainForm.sbStatusBar, FMainForm.tbStatusToolBar,
    ChangeFileExt(FLogPath, '') + '-' + ACheckpoint + '-footer.bmp') = 0,
    ACheckpoint + ': toolbar changes painted status-panel border pixels');
  if ALoading then begin
    LHeight := FMainForm.Height;
    LWidth := FMainForm.Width;
    Require((FMainForm.Constraints.MinHeight = LHeight) and
      (FMainForm.Constraints.MaxHeight = LHeight), ACheckpoint + ': startup height is not fixed');
    Require((FMainForm.Constraints.MinWidth = FOriginalMinWidth) and
      (FMainForm.Constraints.MaxWidth = FOriginalMaxWidth), ACheckpoint + ': startup changed width constraints');
    Require(not (biMaximize in FMainForm.BorderIcons), ACheckpoint + ': startup maximize command is enabled');
    FillChar(LMinMax, SizeOf(LMinMax), 0);
    SendMessage(FMainForm.Handle, WM_GETMINMAXINFO, 0, LPARAM(@LMinMax));
    Require((LMinMax.ptMinTrackSize.Y > 0) and
      (LMinMax.ptMinTrackSize.Y = LMinMax.ptMaxTrackSize.Y),
      ACheckpoint + ': native vertical tracking is not fixed');
    FMainForm.SetBounds(FMainForm.Left, FMainForm.Top, LWidth + 96, LHeight + 96);
    PumpWindowChanges;
    Require((FMainForm.Width = LWidth + 96) and (FMainForm.Height = LHeight),
      ACheckpoint + ': startup must permit width changes and reject height changes');
    Require(FMainForm.SyncControl.ShowSplash, ACheckpoint + ': width change dismissed controlled splash');
    CheckFrameLayout(ACheckpoint + '-wider-fixed-height', true);
    SendMessage(FMainForm.Handle, WM_SYSCOMMAND, SC_MAXIMIZE, 0);
    PumpWindowChanges;
    Require((not Windows.IsZoomed(FMainForm.Handle)) and (FMainForm.Height = LHeight),
      ACheckpoint + ': native maximize changed the startup height/state');
    FMainForm.SetBounds(LRestored.Left, LRestored.Top, LWidth, LHeight);
    PumpWindowChanges;
    CheckFrameLayout(ACheckpoint + '-restored', true);
    Log('STARTUP SIZING: horizontal resize allowed; height and native maximize locked');
    Exit;
  end;
  Require((FMainForm.Constraints.MinHeight = FOriginalMinHeight) and
    (FMainForm.Constraints.MaxHeight = FOriginalMaxHeight) and
    (FMainForm.BorderIcons = FOriginalBorderIcons), ACheckpoint + ': startup sizing policy was not restored');
  FMainForm.WindowState := wsMaximized;
  PumpWindowChanges;
  Require(Windows.IsZoomed(FMainForm.Handle), ACheckpoint + ': native host did not maximize');
  if ALoading then begin
    Require(FMainForm.SyncControl.ShowSplash, ACheckpoint + ': resize dismissed controlled splash');
    CheckFrameLayout(ACheckpoint + '-maximized', false);
  end else
    CheckNative(ACheckpoint + '-maximized', AMode, false);
  FMainForm.WindowState := wsNormal;
  PumpWindowChanges;
  Require(not Windows.IsZoomed(FMainForm.Handle), ACheckpoint + ': native host did not restore');
  Require(GetWindowRect(FMainForm.Handle, LAfter), ACheckpoint + ': cannot read restored bounds');
  Require((LAfter.Left = LRestored.Left) and (LAfter.Top = LRestored.Top) and
    (LAfter.Right = LRestored.Right) and (LAfter.Bottom = LRestored.Bottom),
    ACheckpoint + ': maximize/restore changed the saved normal rectangle');
  if ALoading then
    CheckFrameLayout(ACheckpoint + '-restored', true)
  else
    CheckNative(ACheckpoint + '-restored', AMode);
end;

procedure TStartupHarness.CheckFooterDpiCycle;
var
  LHeight, LWidth: Integer;
  LGeometry: TWalletFooterGeometry;
begin
  LHeight := FMainForm.sbStatusBar.Height;
  LWidth := FMainForm.Width;
  FMainForm.AutoAdjustLayout(lapAutoAdjustForDPI, 96, 144, LWidth, LWidth * 3 div 2);
  PumpWindowChanges;
  LGeometry := ReadWalletFooterGeometry(FMainForm.sbStatusBar, FMainForm.tbStatusToolBar);
  RequireWalletFooterContent(LGeometry, '144PPI');
  Require(CheckWalletFooterPaint(FMainForm.sbStatusBar, FMainForm.tbStatusToolBar,
    ChangeFileExt(FLogPath, '') + '-144PPI-footer.bmp') = 0,
    '144PPI toolbar changes painted status-panel border pixels');
  Log('FOOTER DPI144: ' + LGeometry.Description);
  FMainForm.AutoAdjustLayout(lapAutoAdjustForDPI, 144, 96, FMainForm.Width, LWidth);
  PumpWindowChanges;
  LGeometry := ReadWalletFooterGeometry(FMainForm.sbStatusBar, FMainForm.tbStatusToolBar);
  RequireWalletFooterContent(LGeometry, '96PPI-restored');
  Require(FMainForm.sbStatusBar.Height = LHeight, 'Footer remained oversized after 96->144->96 DPI cycle');
  Require(CheckWalletFooterPaint(FMainForm.sbStatusBar, FMainForm.tbStatusToolBar,
    ChangeFileExt(FLogPath, '') + '-96PPI-footer.bmp') = 0,
    'Restored toolbar changes painted status-panel border pixels');
  Log('FOOTER DPI96 RESTORED: ' + LGeometry.Description);
end;

procedure TStartupHarness.StopWallet;
begin
  if FExited then Exit;
  FExited := true;
  FTimer.Enabled := false;
  TUserInterface.ExitApplication;
  FMainForm := nil;
end;

procedure TStartupHarness.CheckEventDrivenStatuses;
var
  LDeadline: QWord;
begin
  // Exercise the actual node and network notifier threads. One producer input
  // must update the controls; pumping only delivers queued work, never data.
  FMainForm.SyncControl.lblTotalBlocksValue.Caption := 'Await node notification';
  FMainForm.SyncControl.lblPendingOperationsValue.Caption := 'Await account notification';
  FMainForm.sbStatusBar.Panels[1].Text := 'Await network notification';
  TUserInterface.Node.NotifyBlocksChanged;
  TNetData.NetData.NotifyStatisticsChanged;
  LDeadline := GetTickCount64 + 2000;
  repeat
    CheckSynchronize(10);
    Application.ProcessMessages;
    if (FMainForm.SyncControl.lblTotalBlocksValue.Caption = '(none)') and
      (Pos('Connections:', FMainForm.sbStatusBar.Panels[1].Text) = 1) then Break;
    Sleep(1);
  until GetTickCount64 >= LDeadline;
  Require(FMainForm.SyncControl.lblTotalBlocksValue.Caption = '(none)',
    'Actual node notification did not refresh the synchronization block count');
  Require(Pos('Connections:', FMainForm.sbStatusBar.Panels[1].Text) = 1,
    'Actual network notification did not refresh connection statistics');
  FMainForm.SyncControl.lblPendingOperationsValue.Caption := 'Await account notification';
  TUserInterface.SynchronizationChanged.Invoke(nil);
  LDeadline := GetTickCount64 + 2000;
  repeat
    CheckSynchronize(10);
    Application.ProcessMessages;
    if FMainForm.SyncControl.lblPendingOperationsValue.Caption = '0' then Break;
    Sleep(1);
  until GetTickCount64 >= LDeadline;
  Require(FMainForm.SyncControl.lblPendingOperationsValue.Caption = '0',
    'Synchronization status did not immediately publish cached pending operations');
  FMainForm.SyncControl.lblNodeStatus.Caption := 'Await state notification';
  TUserInterface.StateChanged.Invoke(nil);
  Require(FMainForm.SyncControl.lblNodeStatus.Caption = 'Offline mode',
    'State-change event did not refresh the visible synchronization status');
  FMainForm.SyncControl.lblMinersClientsValue.Caption := 'Await miner notification';
  FMainForm.SyncControl.lblBlocksFound.Caption := 'Await miner notification';
  Require(Assigned(TUserInterface.PoolMiningServer.OnStatusChanged),
    'Mining server status event is not bridged to the UI');
  TUserInterface.PoolMiningServer.OnStatusChanged(TUserInterface.PoolMiningServer);
  Require((FMainForm.SyncControl.lblMinersClientsValue.Caption = 'JSON-RPC server not active') and
    (FMainForm.SyncControl.lblBlocksFound.Caption = '0'),
    'Mining-status event did not refresh miner status and wins immediately');
  Log('EVENT-DRIVEN STATUS: actual node/network notifier threads updated native sync/footer; account/state/mining events updated their labels without a UI polling timer');
end;

procedure TStartupHarness.OnSyncSnapshotChanged(Sender: TObject);
begin
  Inc(FSnapshotNotifications);
end;

procedure TStartupHarness.CheckBusyStatisticsRefresh;
var
  LBusy : TBusyMempoolThread;
  LPreviousCount, LPreviousTarget : String;
  LDeadline: QWord;
  LPublished: Integer;
begin
  Require(FMainForm.SyncControl.IsVisible, 'Busy statistics test requires a visible sync screen');
  Require(FMainForm.SyncControl.HasNodeSnapshot, 'Busy statistics test requires a completed snapshot');
  LPreviousCount := FMainForm.SyncControl.lblPendingOperationsValue.Caption;
  LPreviousTarget := FMainForm.SyncControl.lblBlockTargetValue.Caption;
  FSnapshotNotifications := 0;
  FMainForm.SyncControl.SnapshotChanged.Add(OnSyncSnapshotChanged);
  LBusy := TBusyMempoolThread.Create(TUserInterface.Node);
  try
    Require(LBusy.Entered.WaitFor(2000) = wrSignaled, 'Statistics fixture did not acquire mempool');
    TUserInterface.ShowWallet;
    FMainForm.Perform(CM_PC_ModeChanged, 0, 0);
    TUserInterface.ShowSyncDialog;
    FMainForm.Perform(CM_PC_ModeChanged, 0, 0);
    Require((not LBusy.WatchdogReleased) and (LBusy.Released.WaitFor(0) <> wrSignaled),
      'Initial sync show waited for the busy mempool instead of retaining its snapshot');
    Require((FMainForm.SyncControl.lblPendingOperationsValue.Caption = LPreviousCount) and
      (FMainForm.SyncControl.lblBlockTargetValue.Caption = LPreviousTarget),
      'Initial sync show replaced the previous snapshot while core was busy');
    TUserInterface.AccountsChanged.Invoke(nil);
    TUserInterface.BlocksChanged.Invoke(nil);
    Require((not LBusy.WatchdogReleased) and (LBusy.Released.WaitFor(0) <> wrSignaled),
      'Visible sync data notifications waited for the busy mempool');
    Require((FMainForm.SyncControl.lblPendingOperationsValue.Caption = LPreviousCount) and
      (FMainForm.SyncControl.lblBlockTargetValue.Caption = LPreviousTarget),
      'Busy statistics refresh replaced the previous snapshot');
    // Basic facts are cached; view/account requests do not wait for this writer.
    // The explicit block event may schedule one independent secondary Bank read.
    Require(FSnapshotNotifications > 0, 'Cached basic status event was not published');
    LBusy.ReleaseLock.SetEvent;
    LBusy.WaitFor;
    Require(not LBusy.WatchdogReleased, 'Statistics fixture needed its fail-safe release');
    Require(FMainForm.sbStatusBar.Panels[2].Text = FMainForm.SyncControl.ChainStatusText,
      'Cached synchronization status did not update the main footer');
    // Allow the block-triggered secondary request to settle, then prove idle
    // and byte-statistics inputs do not restart it.
    LDeadline := GetTickCount64 + 200;
    repeat
      CheckSynchronize(10);
      Application.ProcessMessages;
      Sleep(1);
    until GetTickCount64 >= LDeadline;
    LPublished := FSnapshotNotifications;
    TUserInterface.NetStatisticsChanged.Invoke(nil);
    LDeadline := GetTickCount64 + 500;
    repeat
      CheckSynchronize(10);
      Application.ProcessMessages;
      Sleep(1);
    until GetTickCount64 >= LDeadline;
    Require(FSnapshotNotifications = LPublished,
      'Idle/unchanged byte statistics restarted a full node snapshot');
  finally
    FMainForm.SyncControl.SnapshotChanged.Remove(OnSyncSnapshotChanged);
    LBusy.Free;
  end;
  Log('BUSY STATISTICS: cached basic labels/footer and retained secondary values stayed available while mempool was held; idle/byte statistics caused no additional snapshot');
end;

procedure TStartupHarness.CheckModelessFocus;
var
  LWallet: TCTRLWallet;
  LGrid: TVisualGrid;
  LDialog: TForm;
  LEdit: TEdit;
  LOverlay: TPanel;
  I: Integer;
begin
  LWallet := FMainForm.paWalletPanel.Controls[0] as TCTRLWallet;
  LGrid := nil;
  for I := 0 to LWallet.ComponentCount - 1 do
    if LWallet.Components[I] is TVisualGrid then begin
      LGrid := TVisualGrid(LWallet.Components[I]);
      Break;
    end;
  Require(Assigned(LGrid), 'Modeless focus test found no real wallet visual grid');
  LDialog := TForm.CreateNew(Self);
  try
    LDialog.Caption := 'Disposable modeless focus check';
    LDialog.BorderStyle := bsDialog;
    LDialog.PopupMode := pmExplicit;
    LDialog.PopupParent := FMainForm;
    LDialog.Position := poMainFormCenter;
    LDialog.SetBounds(120, 130, 360, 160);
    LEdit := TEdit.Create(LDialog);
    LEdit.Parent := LDialog;
    LEdit.SetBounds(20, 20, 280, 24);
    LDialog.Show;
    Windows.SetActiveWindow(LDialog.Handle);
    LEdit.SetFocus;
    Require(Windows.GetActiveWindow = LDialog.Handle,
      'Modeless fixture did not become the owning thread active window');
    TUserInterface.StateChanged.Invoke(nil);
    TUserInterface.BlocksChanged.Invoke(nil);
    PumpWindowChanges;
    Require(Windows.GetActiveWindow = LDialog.Handle,
      'Background refresh activated main above its modeless dialog');
    TVisualGridAccess(LGrid).BeginLoading;
    Sleep(220); // Exercise the real loading card after its intentional delay.
    TVisualGridAccess(LGrid).UpdateLoading;
    LOverlay := LGrid.FindComponent('LoadingOverlay') as TPanel;
    Require(Assigned(LOverlay) and LOverlay.Visible,
      'Delayed loading-card focus check did not show the real overlay');
    Require(Windows.GetActiveWindow = LDialog.Handle,
      'Loading overlay activated main above its modeless dialog');
    TVisualGridAccess(LGrid).ShowLoadingError('Disposable controlled fetch failure');
    Require(LOverlay.Visible and (Windows.GetActiveWindow = LDialog.Handle),
      'Error overlay activated main above its modeless dialog');
    Require(Windows.GetAncestor(Windows.GetFocus, GA_ROOT) = LDialog.Handle,
      'Grid overlay moved keyboard focus away from the modeless dialog');
  finally
    TVisualGridAccess(LGrid).EndLoading;
    LDialog.Free;
  end;
  Log('MODELESS FOCUS: real backend/UI callbacks and delayed loading/error cards preserved dialog activation and keyboard focus');
end;

procedure TStartupHarness.InspectQuit(Sender : TObject);
var
  LDialog: TCustomForm;
  LYes: TCustomButton;
  I, J: Integer;
begin
  LDialog := nil;
  LYes := nil;
  for I := 0 to Screen.FormCount - 1 do
    if Screen.Forms[I].Visible and (Screen.Forms[I].Caption = 'Exit PascalCoin') then begin
      LDialog := Screen.Forms[I];
      Break;
    end;
  if not Assigned(LDialog) then begin
    if GetTickCount64 >= FModalDeadline then begin
      FModalFailure := 'Native Quit did not enter its confirmation dialog';
      FModalTimer.Enabled := false;
      Application.Terminate;
    end;
    Exit;
  end;
  FModalTimer.Enabled := false;
  try
    Require(Assigned(FQuitBusy) and
      (FQuitBusy.Released.WaitFor(0) <> wrSignaled), 'Quit fixture released its core lock early');
    TUserInterface.StateChanged.Invoke(nil);
    TUserInterface.BlocksChanged.Invoke(nil);
    Require((not FQuitBusy.WatchdogReleased) and
      (FQuitBusy.Released.WaitFor(0) <> wrSignaled),
      'Quit confirmation callbacks blocked on the busy core');
    Require(not Windows.IsWindowEnabled(FMainForm.Handle),
      'Backend refresh re-enabled the main window behind Quit confirmation');
    for J := 0 to LDialog.ComponentCount - 1 do
      if (LDialog.Components[J] is TCustomButton) and
        (TCustomButton(LDialog.Components[J]).ModalResult = mrYes) then begin
        LYes := TCustomButton(LDialog.Components[J]);
        Break;
      end;
    Require(Assigned(LYes), 'Quit confirmation has no real Yes button');
    // This fixture borrows the node. Join it before Yes permits node disposal;
    // slow-close separately verifies cancellation while the loader owns locks.
    FQuitBusy.ReleaseLock.SetEvent;
    FQuitBusy.WaitFor;
    Require(not FQuitBusy.WatchdogReleased, 'Busy Quit needed its fail-safe release');
    FModalSeen := true;
    LYes.Click;
  except
    on E: Exception do begin
      FModalFailure := E.Message;
      LDialog.ModalResult := mrNo;
    end;
  end;
end;

procedure TStartupHarness.CheckBusyQuit;
begin
  FQuitBusy := TBusyMempoolThread.Create(TUserInterface.Node);
  try
    Require(FQuitBusy.Entered.WaitFor(2000) = wrSignaled, 'Quit fixture did not acquire mempool');
    FModalSeen := false;
    FModalFailure := '';
    FModalDeadline := GetTickCount64 + 1500;
    FModalTimer.OnTimer := InspectQuit;
    FModalTimer.Enabled := true;
    // Actual Close -> confirmation -> real Yes -> posted controller shutdown.
    FMainForm.Close;
    Require(FModalFailure = '', FModalFailure);
    Require(FModalSeen, 'Quit confirmation was not inspected');
    FExited := true;
    FTimer.Enabled := false;
  finally
    FModalTimer.Enabled := false;
    FModalTimer.OnTimer := InspectWorkbench;
    FreeAndNil(FQuitBusy);
  end;
  Log('BUSY QUIT: real Close/Yes accepted while background callbacks were core-busy; borrowed fixture joined before node disposal');
end;

procedure TStartupHarness.CheckClassicIntegration;
var
  LMessages : TFRMMessages;
  LLogs : TFRMLogs;
  LBlocks : TFRMBlockExplorer;
  LAccounts : TFRMAccountExplorer;
  LMessageCount, i : Integer;
  LDeadline : QWord;
  LRejected : Boolean;
begin
  Require(Assigned(FMainForm.miAdvancedOperation) and Assigned(FMainForm.miOperationsWorkbench) and
    Assigned(FMainForm.miRPCCalls), 'Classic advanced tools are missing from the main menu');
  Require(not FMainForm.miRPCCalls.Enabled, 'RPC calls menu is enabled offline');
  Require(FMainForm.miOperationsWorkbench.Enabled, 'Offline workbench preparation is disabled');
  LRejected := false;
  try
    TUserInterface.ShowRPCCallsDialog(FMainForm);
  except
    on E : EUserInterface do LRejected := true;
  end;
  Require(LRejected, 'Programmatic RPC tool invocation bypassed offline policy');
  FModalSeen := false;
  FModalFailure := '';
  FModalDeadline := GetTickCount64 + 3000;
  FModalTimer.Enabled := true;
  TUserInterface.ShowOperationsWorkbench(FMainForm);
  Require(FModalFailure = '', FModalFailure);
  Require(FModalSeen, 'RAW workbench composition was not inspected');
  Require(Windows.IsWindowEnabled(FMainForm.Handle), 'Closing the workbench did not restore main input');

  TSettings.MinFutureBlocksToDownloadNewSafebox := 1234;
  TSettings.AllowDownloadNewCheckpointIfOlderThan := true;
  TSettings.Save;
  Require(TNetData.NetData.MinFutureBlocksToDownloadNewSafebox = 1234,
    'Checkpoint download threshold was not applied');
  TSettings.AllowDownloadNewCheckpointIfOlderThan := false;
  TSettings.Save;
  Require(TNetData.NetData.MinFutureBlocksToDownloadNewSafebox = 0,
    'Checkpoint downloads were not disabled');

  TSettings.ShowLogs := true;
  TSettings.Save;
  LLogs := nil;
  LMessages := nil;
  for i := 0 to Screen.FormCount - 1 do begin
    if Screen.Forms[i] is TFRMLogs then LLogs := TFRMLogs(Screen.Forms[i]);
    if Screen.Forms[i] is TFRMMessages then LMessages := TFRMMessages(Screen.Forms[i]);
  end;
  Require(FMainForm.miLogs.Visible and Assigned(LLogs) and Assigned(TUserInterface.Log.OnNewLog),
    'Show Logs did not enable the menu and log capture');
  TSettings.ShowLogs := false;
  TSettings.Save;
  Require((not FMainForm.miLogs.Visible) and (not LLogs.Visible) and
    (not Assigned(TUserInterface.Log.OnNewLog)), 'Hidden logs remained visible or subscribed');

  Require(Assigned(LMessages) and (not LMessages.Visible), 'Message history is not subscribed before first open');
  LMessageCount := LMessages.memoMessages.Lines.Count;
  TUserInterface.NodeMessageEvent.Invoke(nil, 'Disposable QA message before first open');
  Require(LMessages.memoMessages.Lines.Count = LMessageCount + 1, 'Hidden incoming message was lost');
  Require(FMainForm.SyncControl.MessagesNotificationText <> '', 'Hidden incoming message did not set unread state');
  TUserInterface.ShowMessagesForm;
  Require(LMessages.Visible and (FMainForm.SyncControl.MessagesNotificationText = ''),
    'Opening messages did not clear unread state');
  TUserInterface.NodeMessageEvent.Invoke(nil, 'Disposable QA message while open');
  Require(FMainForm.SyncControl.MessagesNotificationText = '', 'Visible messages left stale unread state');
  LMessages.Close;
  Require(not LMessages.Visible, 'Closing messages did not hide its persistent history');
  TUserInterface.NodeMessageEvent.Invoke(nil, 'Disposable QA message after closing');
  Require(LMessages.memoMessages.Lines.Count = LMessageCount + 3, 'Closing messages discarded later history');
  TUserInterface.NetConnectionsUpdated.Invoke(nil);
  Require(LMessages.lbNetConnections.Items.Count = 0, 'Offline message recipients were not refreshed');
  TUserInterface.ShowMessagesForm;
  Require(LMessages.memoMessages.Lines.Count = LMessageCount + 3, 'Reopening messages lost session history');
  Require(FMainForm.SyncControl.MessagesNotificationText = '', 'Reopening messages did not clear unread state');
  LMessages.Close;

  TUserInterface.ShowBlockExplorer;
  LBlocks := nil;
  for i := 0 to Screen.FormCount - 1 do
    if Screen.Forms[i] is TFRMBlockExplorer then LBlocks := TFRMBlockExplorer(Screen.Forms[i]);
  Require(Assigned(LBlocks) and LBlocks.Visible, 'Block explorer did not open');
  LBlocks.ebHashRateBackBlocks.Text := '0';
  LBlocks.ebHashRateBackBlocksExit(nil);
  Require((LBlocks.ebHashRateBackBlocks.Text = '1') and (TSettings.HashRateAvgBlocksCount = 1),
    'Block hashrate window did not clamp its lower bound');
  LBlocks.ebHashRateBackBlocks.Text := '2000';
  LBlocks.ebHashRateBackBlocksExit(nil);
  Require((LBlocks.ebHashRateBackBlocks.Text = '1000') and (TSettings.HashRateAvgBlocksCount = 1000),
    'Block hashrate window did not clamp its upper bound');
  LBlocks.cbHashRateUnits.ItemIndex := Ord(hr_Mega);
  LBlocks.cbHashRateUnitsChange(nil);
  LBlocks.Close;
  Application.ProcessMessages;
  TUserInterface.ShowBlockExplorer;
  LBlocks := nil;
  for i := 0 to Screen.FormCount - 1 do
    if Screen.Forms[i] is TFRMBlockExplorer then LBlocks := TFRMBlockExplorer(Screen.Forms[i]);
  Require(Assigned(LBlocks) and (LBlocks.ebHashRateBackBlocks.Text = '1000') and
    (LBlocks.cbHashRateUnits.ItemIndex = Ord(hr_Mega)), 'Reopened block explorer lost hashrate preferences');
  LBlocks.Close;
  Application.ProcessMessages;

  TUserInterface.ShowAccountExplorer;
  LAccounts := nil;
  for i := 0 to Screen.FormCount - 1 do
    if Screen.Forms[i] is TFRMAccountExplorer then LAccounts := TFRMAccountExplorer(Screen.Forms[i]);
  Require(Assigned(LAccounts) and Assigned(LAccounts.miFindAccount.OnClick), 'Account Find command is not wired');
  LAccounts.lblAccountsCount.Caption := 'stale';
  LAccounts.Refresh;
  LDeadline := GetTickCount64 + 3000;
  while (LAccounts.lblAccountsCount.Caption <> '0') and (GetTickCount64 < LDeadline) do begin
    Application.ProcessMessages;
    Sleep(1);
  end;
  Require(LAccounts.lblAccountsCount.Caption = '0', 'Account totals did not refresh for the empty safebox');
  LAccounts.Close;
  Application.ProcessMessages;
  Log('CLASSIC INTEGRATION: advanced tools exposed; offline RPC guarded; checkpoint/log settings applied; persistent message history/unread/recipients; block clamp/preferences/reopen; account Find/totals verified');
end;

procedure TStartupHarness.CheckShutdown;
begin
  CheckSynchronize;
  Require(not TUserInterface.Started, 'Application still started after shutdown');
  Require(not Assigned(TUserInterface.Node), 'Node not released after shutdown');
  Require(not TNode.NodeExists, 'Node singleton remained after shutdown');
  Require(not Assigned(TUserInterface.RPCServer), 'RPC server not released after shutdown');
  Require(not Assigned(TUserInterface.PoolMiningServer), 'Mining server not released after shutdown');
  Require(Screen.FormCount = 0, 'Forms remained after shutdown: ' + IntToStr(Screen.FormCount));
  GAllowedWindow := 0;
  GUnexpectedWindow := 0;
  EnumWindows(@FindUnexpectedWindow, 0);
  Require(GUnexpectedWindow = 0, 'Visible application window remained after shutdown');
  Log('SHUTDOWN: loader joined, queued callbacks drained, wallet/node/servers/forms released');
end;

procedure TStartupHarness.Tick(Sender : TObject);
var
  LCancel : TButton;
  LStatusText : TLabel;
begin
  if FInsideTick then Exit;
  FInsideTick := true;
  try
    try
      Require(GetTickCount64 < FDeadline, 'Startup probe timed out');
      Require(TUserInterface.State <> uisError, TUserInterface.StateText);
      if FMode = 'slow-close' then begin
        if FPhase = 0 then begin
          Require(FSlowTask.FEntered.WaitFor(0) = wrSignaled, 'Controlled loader did not enter');
          // The task's signal belongs to the worker; wait for its real queued
          // progress event before checking the receiving native label.
          if not FProgressSeen then Exit;
          Require(FLoadedCount = 0, 'Controlled loader completed unexpectedly');
          Require(TUserInterface.State = uisLoading, 'Controlled loader left loading state');
          Require(FMainForm.Visible and FMainForm.Enabled and Windows.IsWindowEnabled(FMainForm.Handle),
            'Loading disables the native main window and its Close input');
          Require(FMainForm.SyncControl.ShowSplash, 'Controlled loading splash is not visible');
          Require(not FMainForm.tbtnSync.Enabled, 'Wallet navigation enabled before local load');
          Require(not FMainForm.miOptions.Enabled, 'Options enabled before local load');
          Require(not FMainForm.miPrivateKeys.Enabled, 'Key commands enabled before local load');
          Require(Windows.IsWindowVisible(FMainForm.SyncControl.Handle), 'Loading screen has no visible child window');
          FMainForm.SetBounds(80, 90, 1000, 680);
          PumpWindowChanges;
          CheckResizeCycle('loading', wmSync, true);
          LStatusText := FMainForm.SyncControl.FindComponent('lblLoadingStatus') as TLabel;
          Require((FLastProgress.Stage = psReadingBlockchain) and
            (FLastProgress.CurrentPosition = 1) and (FLastProgress.TotalCount = 100),
            'Typed startup progress lost its stage or counters');
          Require(Assigned(LStatusText) and
            (LStatusText.Caption = FormatProgressInfo(FLastProgress)),
            'Native splash did not format the queued progress record at its UI boundary');
          Require(Pos('(' + FormatFloat('0.00', 1) + '%)', LStatusText.Caption) > 0,
            'Native 1/100 progress did not display one percent');
          Log('TYPED_PROGRESS queued stage and raw counts displayed once on main thread');
          Require(Assigned(LStatusText) and (LStatusText.Font.Color = clWhite) and
            (LStatusText.Font.Size >= 16) and LStatusText.Transparent,
            'Loading status lost its large transparent white text');
          LCancel := FMainForm.SyncControl.FindComponent('btnCancelLoading') as TButton;
          Require(Assigned(LCancel) and LCancel.Enabled and Windows.IsWindowVisible(LCancel.Handle),
            'Loading has no usable Cancel startup button');
          // These must not query or write locked/partially restored core state.
          TUserInterface.BlocksChanged.Invoke(nil);
          TUserInterface.StateChanged.Invoke(nil);
          TSettings.MinerName := 'Pending disposable QA payload while loading';
          TSettings.Save;
          Log('SLOW LOAD: safebox/mempool locks held; UI refresh/settings stayed responsive; native host enabled; local data commands disabled; posting its own native SC_CLOSE');
          PostMessage(FMainForm.Handle, WM_SYSCOMMAND, SC_CLOSE, 0);
          Inc(FPhase);
        end;
        Exit;
      end;
      if FLoadedCount = 0 then Exit;
      Require(FLoadedCount = 1, 'Loaded event occurred more than once');
      case FPhase of
        0: begin
          Require(TUserInterface.Node.Bank.BlocksCount = 0, 'Fresh test chain is not empty');
          Require(TUserInterface.Node.Bank.AccountsCount = 0, 'Fresh test safebox has accounts');
          Require(SameFileName(TNode.GetPascalCoinDataFolder, FDataDirectory), 'Wrong data directory');
          CheckOffline('loaded');
          FMainForm.SetBounds(80, 90, 1000, 680);
          PumpWindowChanges;
          CheckNative('wallet-initial', wmWallet);
          CheckResizeCycle('wallet', wmWallet);
          CheckFooterDpiCycle;
          CheckFooterActions;
          CheckClassicIntegration;
          CheckModelessFocus;
          // These are calls into our own QA application, never desktop input.
          FMainForm.tbtnConnectivityClick(nil);
          TSettings.JsonRpcPortEnabled := true;
          TSettings.JsonRpcMinerServerActive := true;
          TSettings.InternetServerPort := TSettings.InternetServerPort + 1;
          TSettings.JsonRpcMinerServerPort := TSettings.JsonRpcMinerServerPort + 1;
          TSettings.Save;
          CheckOffline('after connectivity click and settings changes');
          Log(CheckWalletKeysIcons(FMainForm, ExtractFilePath(FLogPath)));
          TWallet.GenerateNewKey('Disposable native QA key', CT_Default_EC_OpenSSL_NID);
          Require(TWallet.Keys.Count = 1, 'Generated QA key was not added');
          Log('KEY LIFECYCLE: generated one disposable key while the wallet control exists');
          Log(CheckSyncFirstDisplay(FMainForm));
        end;
        1: begin
          CheckNative('sync-first', wmSync);
          CheckResizeCycle('sync', wmSync);
          CheckEventDrivenStatuses;
          CheckBusyStatisticsRefresh;
          Log(CheckSyncWriterIndependence(FMainForm));
          TUserInterface.ShowWallet;
        end;
        2: begin
          CheckNative('wallet-return', wmWallet);
          TWallet.DeleteKey(TWallet.Keys.Key[0]);
          Require(TWallet.Keys.Count = 0, 'Generated QA key was not removed');
          Log('KEY LIFECYCLE: removed disposable key while the wallet control exists');
          Log(CheckSyncFirstDisplay(FMainForm));
        end;
        3: begin
          CheckNative('sync-reopen', wmSync);
          TUserInterface.ShowWallet;
        end;
        4: begin
          CheckNative('wallet-final', wmWallet);
          CheckOffline('after mode transitions');
          CheckBusyQuit;
        end;
      end;
      Inc(FPhase);
    except
      on E : Exception do OnException(Self, E);
    end;
  finally
    FInsideTick := false;
  end;
end;

procedure TStartupHarness.Run(const AOptions : TWalletStartupOptions);
var
  LInputDesktop : HDESK;
  LCurrentDesktop, LInputDesktopName : String;
begin
  LCurrentDesktop := DesktopName(GetThreadDesktop(GetCurrentThreadId));
  Require(AnsiStartsText('PascalCoinWalletQA_', LCurrentDesktop), 'Refusing to run outside private QA desktop');
  LInputDesktop := OpenInputDesktop(0, false, DESKTOP_READOBJECTS);
  if LInputDesktop = 0 then RaiseLastOSError;
  try
    LInputDesktopName := DesktopName(LInputDesktop);
    Require(not SameText(LCurrentDesktop, LInputDesktopName), 'Refusing to run on the input desktop');
  finally
    CloseDesktop(LInputDesktop);
  end;
  Log('SAFETY: own desktop=' + LCurrentDesktop + '; input desktop=' + LInputDesktopName);
  Require(AOptions.Offline and AOptions.NoStartupDialogs, 'Offline and no-startup-dialogs are required');
  Require(AOptions.DataDirectory <> '', 'Explicit data directory is required');
  Require(AnsiStartsText(IncludeTrailingPathDelimiter(ExpandFileName(GetTempDir)), FDataDirectory),
    'Refusing data directory outside OS temp');
  Require(AnsiStartsText('PascalCoinWalletQA_', ExtractFileName(FDataDirectory)), 'Wrong disposable directory prefix');
  Require(not DirectoryExists(FDataDirectory), 'Refusing to reuse a data directory');
  Require(ForceDirectories(FDataDirectory), 'Cannot create disposable directory');
  SeedSettings;
  TUserInterface.ConfigureStartup(AOptions);
  Require(SameFileName(TNode.GetPascalCoinDataFolder, FDataDirectory), 'Data directory not configured before forms');
  Application.Initialize;
  Application.MainFormOnTaskBar := true;
  Application.OnException := OnException;
  Application.Title := 'PascalCoin private startup QA';
  Application.CreateForm(TFRMMainForm, FMainForm);
  Require((FMainForm.Constraints.MinHeight = FMainForm.Height) and
    (FMainForm.Constraints.MaxHeight = FMainForm.Height) and
    (not (biMaximize in FMainForm.BorderIcons)), 'FormCreate did not lock startup height');
  // Capture the real resource defaults before relocking the pending startup.
  TMainFormAccess(FMainForm).SetStartupHeightLocked(false);
  FOriginalMinHeight := FMainForm.Constraints.MinHeight;
  FOriginalMaxHeight := FMainForm.Constraints.MaxHeight;
  FOriginalMinWidth := FMainForm.Constraints.MinWidth;
  FOriginalMaxWidth := FMainForm.Constraints.MaxWidth;
  FOriginalBorderIcons := FMainForm.BorderIcons;
  TMainFormAccess(FMainForm).SetStartupHeightLocked(true);
  TUserInterface.Loaded.Add(OnLoaded);
  TUserInterface.Loading.Add(OnLoading);
  TUserInterface.AppStarted.Add(OnAppStarted);
  FDeadline := GetTickCount64 + 15000;
  Log('START ' + FMode + ': data=' + FDataDirectory);
  if FMode = 'slow-close' then begin
    FSlowTask := TSlowStartupTask.Create;
    TUserInterface.StartApplication(FMainForm, FSlowTask);
  end else
    TUserInterface.StartApplication(FMainForm);
  if FMode = 'close-before-load' then begin
    Require(FExited, 'Early close did not occur');
    Require(FLoadedCount = 0, 'Loaded notification escaped early close');
  end else if FMode = 'close-loading' then begin
    Require(TUserInterface.Started, 'Startup failed before close-during-loading');
    Require(TUserInterface.State = uisLoading, 'Expected pending local-load notification');
    Require(FLoadedCount = 0, 'Loaded fired before event loop');
    StopWallet;
    Require(FLoadedCount = 0, 'Loaded notification escaped close-during-loading');
  end else begin
    Require(TUserInterface.Started, 'Startup failed: ' + TUserInterface.StateText);
    FTimer.Enabled := true;
    Application.Run;
    FTimer.Enabled := false;
    if FMode = 'slow-close' then begin
      FMainForm := nil;
      FSlowTask := nil; // Owned and joined by the actual controller.
      FExited := true;
      Require(FPhase = 1, 'Native close was not exercised');
      Require(FLoadedCount = 0, 'Loaded callback escaped slow-load cancellation');
      Require(GSlowLoadCancelled, 'Native close did not cancel and join controlled local load');
    end else begin
      FMainForm := nil; // The actual Close/Yes path has released the form.
      Require(FLoadedCount = 1, 'Expected one Loaded event');
      Require(FPhase = 5, 'Not all native mode transitions completed');
    end;
  end;
  if FFailure <> '' then
    raise Exception.Create(FFailure);
  CheckShutdown;
  Log('PASS ' + FMode);
end;

procedure TStartupHarness.SeedSettings;
var
  LScoped : TDisposables;
  LSeedSettings : TAppParams;
begin
  // The scope releases the file before normal wallet settings open it.
  LSeedSettings := LScoped.AddObject(TAppParams.Create(nil)) as TAppParams;
  LSeedSettings.FileName := IncludeTrailingPathDelimiter(FDataDirectory) + 'AppParams.prm';
  LSeedSettings.ParamByName[CT_PARAM_JSONRPCEnabled].SetAsBoolean(true);
  LSeedSettings.ParamByName[CT_PARAM_JSONRPCMinerServerActive].SetAsBoolean(true);
  LSeedSettings.ParamByName[CT_PARAM_PeerCache].SetAsString('127.0.0.1:1');
  LSeedSettings.ParamByName[CT_PARAM_TryToConnectOnlyWithThisFixedServers].SetAsString('127.0.0.1:1');
end;

var
  LArguments : array of String;
  LOptions : TWalletStartupOptions;
  LError, LMode, LLogPath, LArgument : String;
  LHarness : TStartupHarness;
  i, LArgumentCount : Integer;
begin
  LMode := '';
  LLogPath := '';
  SetLength(LArguments, ParamCount);
  LArgumentCount := 0;
  for i := 1 to ParamCount do begin
    LArgument := ParamStr(i);
    if AnsiStartsStr('--probe-mode=', LArgument) then
      LMode := Copy(LArgument, Length('--probe-mode=') + 1, MaxInt)
    else if AnsiStartsStr('--probe-log=', LArgument) then
      LLogPath := Copy(LArgument, Length('--probe-log=') + 1, MaxInt)
    else begin
      LArguments[LArgumentCount] := LArgument;
      Inc(LArgumentCount);
    end;
  end;
  SetLength(LArguments, LArgumentCount);
  if (not TryParseWalletStartupOptions(LArguments, LOptions, LError)) or
    (not ((LMode = 'loaded') or (LMode = 'close-loading') or (LMode = 'close-before-load') or (LMode = 'slow-close'))) or
    (LLogPath = '') then Halt(64);
  LHarness := TStartupHarness.Create(LMode, LLogPath, LOptions.DataDirectory);
  try
    try
      LHarness.Run(LOptions);
    except
      on E : Exception do begin
        LHarness.Log('FAIL ' + E.ClassName + ': ' + E.Message);
        if TUserInterface.Started then
          LHarness.StopWallet;
        ExitCode := 1;
      end;
    end;
  finally
    LHarness.Free;
  end;
end.
