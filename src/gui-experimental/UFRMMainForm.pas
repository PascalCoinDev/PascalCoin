unit UFRMMainForm;

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  Acknowledgements:
  - Albert Molina: portions of code copied from https://github.com/PascalCoin/PascalCoin/blob/Releases/2.1.6/Units/Forms/UFRMWallet.pas

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

{$I ..\config.inc}

uses
  {$IFNDEF FPC}Winapi.Windows,{$ENDIF}
  {$IFNDEF FPC}Vcl.AppEvnts,{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, {$IFDEF FPC}LMessages,{$ENDIF} UWallet,
  ULog,
  UBlockChain, UNode, UGridUtils, UAccounts, Menus,
  UNetProtocol, UCrypto, Buttons, ActnList, UPoolMining,
  UCTRLBanner, UCTRLWallet, UCTRLSyncronization, UCommon, UCommon.UI;

const
  CM_PC_FinishedLoadingDatabase = WM_USER + 1;
  CM_PC_WalletKeysChanged = WM_USER + 2;
  CM_PC_ConnectivityChanged = WM_USER + 3;
  CM_PC_Terminate = WM_USER + 4;
  CM_PC_ModeChanged  = WM_USER + 5;
  CM_PC_LayoutFooter = WM_USER + 6;

type

  { TFRMMainFormMode }

  TFRMMainFormMode = (wmWallet, wmSync);

  { TFRMMainForm }

  TFRMMainForm = class(TApplicationForm)
    ilSmallIcons:TImageList;
    miSyncDialog:TMenuItem;
    miBlockExplorer: TMenuItem;
    miLogs: TMenuItem;
    miOperationsExplorer: TMenuItem;
    miMessages: TMenuItem;
    miNodes: TMenuItem;
    miPendingOperations: TMenuItem;
    miViews: TMenuItem;
    miAccountExplorer: TMenuItem;
    paLogoPanel: TPanel;
    paWalletPanel: TPanel;
    paSyncPanel: TPanel;
    sbStatusBar: TStatusBar;
    meMainMenu: TMainMenu;
    miWallet: TMenuItem;
    miOptions: TMenuItem;
    miPrivateKeys: TMenuItem;
    miAdvancedOperation: TMenuItem;
    miOperationsWorkbench: TMenuItem;
    miRPCCalls: TMenuItem;
    miN1: TMenuItem;
    miAboutMenu: TMenuItem;
    miAbout: TMenuItem;
    N1: TMenuItem;
    miClose: TMenuItem;
    ilIcons: TImageList;
    {$IFDEF FPC}ApplicationEvents: TApplicationProperties;
    {$ELSE}ApplicationEvents: TApplicationEvents;{$ENDIF}
    miSeedNodes: TMenuItem;
    pnlSelectedAccountsBottom: TPanel;
    tbStatusToolBar: TToolBar;
    tbtnConnectivity: TToolButton;
    tbtnSync: TToolButton;
    tbtnWalletLock: TToolButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure miAccountExplorerClick(Sender: TObject);
    procedure miBlockExplorerClick(Sender: TObject);
    procedure miLogsClick(Sender: TObject);
    procedure miMessagesClick(Sender: TObject);
    procedure miSyncDialogClick(Sender:TObject);
    procedure miNodesClick(Sender: TObject);
    procedure miPendingOperationsClick(Sender: TObject);
    procedure miOperationsExplorerClick(Sender: TObject);
    procedure miOptionsClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure miPrivateKeysClick(Sender: TObject);
    procedure miAdvancedOperationClick(Sender: TObject);
    procedure miOperationsWorkbenchClick(Sender: TObject);
    procedure miRPCCallsClick(Sender: TObject);
    procedure miCloseClick(Sender: TObject);
    procedure tbtnSyncClick(Sender: TObject);
    procedure tbtnWalletLockClick(Sender:TObject);
    procedure tbtnConnectivityClick(Sender:TObject);
    procedure miSeedNodesClick(Sender: TObject);
  private
    FSubscribed : Boolean;
    FFooterLayoutActive : Boolean;
    FFooterLayoutPending : Boolean;
    FStartupHeightLocked : Boolean;
    FStartupMinHeight, FStartupMaxHeight : Integer;
    FStartupBorderIcons : TBorderIcons;
    FMode : TFRMMainFormMode;
    FSyncControl : TCTRLSyncronization;
    FWalletControl : TCTRLWallet;
    procedure CM_FinishedLoadingDatabase(var Msg: TMessage); message CM_PC_FinishedLoadingDatabase;
    procedure CM_WalletChanged(var Msg: TMessage); message CM_PC_WalletKeysChanged;
    procedure CM_ConnectivityChanged(var Msg: TMessage); message CM_PC_ConnectivityChanged;
    procedure CM_Terminate(var Msg: TMessage); message CM_PC_Terminate;
    procedure CM_ModeChanged(var Msg: TMessage); message CM_PC_ModeChanged;
    procedure CM_LayoutFooter(var Msg: TMessage); message CM_PC_LayoutFooter;
    procedure LayoutFooter;

    procedure OnAppStarted(Sender: TObject);
    procedure OnLoading(Sender: TObject; const AProgress : TProgressInfo);
    procedure OnLoaded(Sender: TObject);
    procedure OnStateChanged(Sender: TObject);
    procedure OnConnectivityChanged(Sender: TObject);
    procedure OnNetStatisticsChanged(Sender: TObject);
    procedure OnWalletChanged(Sender: TObject);
    procedure OnSyncSnapshotChanged(Sender: TObject);
    {$IFDEF TESTNET}
    procedure InitMenuForTesting;
    procedure Test_RandomOperations(Sender: TObject);
    procedure Test_ShowPublicKeys(Sender: TObject);
    {$IFDEF TESTING_NO_POW_CHECK}
    procedure Test_CreateABlock(Sender: TObject);
    {$ENDIF}
    {$ENDIF}
    Procedure InitMacOSMenu;
  protected
    function CreateWalletControl: TCTRLWallet; virtual;
    property WalletControl: TCTRLWallet read FWalletControl;
    procedure WndProc(var Msg: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF}); override;
    procedure RefreshConnectionStatusDisplay;
    procedure RefreshWalletLockIcon;
    procedure RefreshConnectivityIcon;
    procedure RefreshStartupControls;
    procedure SetStartupHeightLocked(ALocked: Boolean);
    procedure ActivateFirstTime; override;
    Function ForceMining : Boolean; virtual;
    procedure SetMode(AMode: TFRMMainFormMode);
  public
    // Bind after the controller has opened settings and wallet keys. Window
    // activation is not guaranteed on a background or private desktop.
    procedure SubscribeApplicationEvents;
    property SyncControl : TCTRLSyncronization read FSyncControl;
    property Mode : TFRMMainFormMode read FMode write SetMode;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses {$IFDEF FPC}LCLIntf, LCLType, {$IFDEF MSWINDOWS}Windows, CommCtrl,{$ENDIF}
  {$ELSE}Winapi.CommCtrl,{$ENDIF} Themes, Math, UUserInterface, UThread, UOpTransaction, UWizard, UTime,
  UMemory, UPCOrderedLists, USettings;

const
  CT_FOOTER_TOOLBAR_LEFT_PADDING = 8;
  CT_FOOTER_TOOLBAR_RIGHT_PADDING = 20;
  CT_FOOTER_TOOLBAR_VERTICAL_PADDING = 4;

{ TFRMWallet }

{%region Form life-cycle }

procedure TFRMMainForm.FormCreate(Sender: TObject);
begin
  CloseAction := caNone; // Will handle terminate in separate method
  SetStartupHeightLocked(true);
  Mode := wmSync;
  FSyncControl := TCTRLSyncronization.Create(self);
  paLogoPanel.AddControlDockCenter(TCTRLBanner.Create(Self));
  paSyncPanel.AddControlDockCenter(FSyncControl);
  // Remove design-time colors (avoid issue on macOS)
  paSyncPanel.Color:=clDefault;
  paSyncPanel.Caption:='';
  paWalletPanel.Color := clDefault;
  paWalletPanel.Caption:='';

  // note: wallet control is lazily constructed
  {$IFDEF TESTNET}
  // Things for testing purposes only
  InitMenuForTesting;
  {$ENDIF}
  InitMacOSMenu;
  FormResize(Self);
end;

procedure TFRMMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  // A local restore can take time. Keep native Close usable and cancel the
  // owned loader before the controller destroys its receivers and node.
  if TUserInterface.State = uisLoading then begin
    CanClose := false;
    PostMessage(Self.Handle, CM_PC_Terminate, 0, 0);
    Exit;
  end;
  case TUserInterface.AskQuestion(Self, mtConfirmation, 'Exit PascalCoin',
  'Are you sure you want to quit?', [mbNo, mbYes]) of
    mbYes: begin
      CanClose := false;
      PostMessage(Self.Handle, CM_PC_Terminate, 0, 0);
    end;
    mbNo: begin
      CanClose := false;
      //TUserInterface.RunInBackground;
    end;
    mbCancel: CanClose := false;
  end;
end;

procedure TFRMMainForm.CM_Terminate(var Msg: TMessage);
begin
  TUserInterface.ExitApplication;
end;

procedure TFRMMainForm.ActivateFirstTime;
begin
  inherited;
  SubscribeApplicationEvents;
end;

procedure TFRMMainForm.SubscribeApplicationEvents;
begin
  if FSubscribed or (not TUserInterface.Started) then Exit;
  // Settings and wallet keys must already be open before these subscriptions.
  TUserInterface.AppStarted.Add(OnAppStarted);
  TUserInterface.Loading.Add(OnLoading);
  TUserInterface.Loaded.Add(OnLoaded);
  TUserInterface.StateChanged.Add(OnStateChanged);
  TUserInterface.SynchronizationChanged.Add(OnStateChanged);
  TWallet.Keys.OnChanged.Add(OnWalletChanged);
  TNetData.NetData.OnConnectivityChanged.Add(OnConnectivityChanged);
  TUserInterface.NetStatisticsChanged.Add(OnNetStatisticsChanged);
  // Settings are applied by the controller before this cheap display receiver.
  TSettings.OnChanged.Add(OnNetStatisticsChanged);
  FSyncControl.SnapshotChanged.Add(OnSyncSnapshotChanged);
  FSubscribed := true;
  RefreshWalletLockIcon;
  RefreshConnectivityIcon;
  RefreshStartupControls;
  // Startup can finish before the window receives its first activation.
  OnAppStarted(Self);
  if TUserInterface.State <> uisLoading then
    OnLoaded(Self);
end;

procedure TFRMMainForm.FormDestroy(Sender: TObject);
begin
  if not FSubscribed then Exit;
  FSubscribed := false;
  TUserInterface.AppStarted.Remove(OnAppStarted);
  TUserInterface.Loading.Remove(OnLoading);
  TUserInterface.Loaded.Remove(OnLoaded);
  TUserInterface.StateChanged.Remove(OnStateChanged);
  TUserInterface.SynchronizationChanged.Remove(OnStateChanged);
  TWallet.Keys.OnChanged.Remove(OnWalletChanged);
  TNetData.NetData.OnConnectivityChanged.Remove(OnConnectivityChanged);
  TUserInterface.NetStatisticsChanged.Remove(OnNetStatisticsChanged);
  TSettings.OnChanged.Remove(OnNetStatisticsChanged);
  FSyncControl.SnapshotChanged.Remove(OnSyncSnapshotChanged);
end;

{%endregion}

{%region Form methods}

function TFRMMainForm.ForceMining: Boolean;
begin
  Result := false;
end;

function TFRMMainForm.CreateWalletControl: TCTRLWallet;
begin
  Result := TCTRLWallet.Create(Self);
end;

procedure TFRMMainForm.RefreshWalletLockIcon;
begin
  if TUserInterface.State in [uisLoading, uisError] then begin
    tbtnWalletLock.Enabled := false;
    Exit;
  end;
  if NOT TWallet.Keys.HasPassword then begin
    { No password has been set }
    tbtnWalletLock.Enabled := false;
    tbtnWalletLock.ImageIndex := 3;
    tbtnWalletLock.Hint := 'Your wallet does not have a password.';
  end else if TWallet.Keys.IsValidPassword then begin
    { Password has been input, allow user to lock }
    tbtnWalletLock.Enabled := true;
    tbtnWalletLock.ImageIndex := 3;
    tbtnWalletLock.Hint := 'Wallet is unlocked. Click to lock.';
  end else begin
    { Password has not been inputted, allow user to unlock }
    tbtnWalletLock.Enabled := true;
    tbtnWalletLock.ImageIndex := 2;
    tbtnWalletLock.Hint := 'Wallet is locked. Click to unlock.';
  end;
end;

procedure TFRMMainForm.RefreshConnectivityIcon;
const
  ImageIndexConst : array[false..true] of integer=(1,0);
  HintConst : array[false..true] of String =('Network is inactive. Click activate.','Network is active. Click to deactivate.');
begin
  tbtnConnectivity.ImageIndex := ImageIndexConst[TNetData.NetData.NetConnectionsActive];
  tbtnConnectivity.Hint :=  HintConst[TNetData.NetData.NetConnectionsActive];
  tbtnConnectivity.Enabled := (not TUserInterface.Offline) and
    (not (TUserInterface.State in [uisLoading, uisError]));
  if TUserInterface.Offline then
    tbtnConnectivity.Hint := 'Offline mode: restart without --offline to enable networking.';
end;

procedure TFRMMainForm.RefreshStartupControls;
var
  LReady, LPendingReady : Boolean;
begin
  SetStartupHeightLocked(TUserInterface.State = uisLoading);
  LReady := TUserInterface.Started and
    (not (TUserInterface.State in [uisLoading, uisError]));
  // Confirmed-block progress remains available while the pending overlay is
  // deliberately withheld. Commands needing pending balances wait for replay.
  LPendingReady := LReady and TUserInterface.SynchronizationStatus.PendingOperationsAvailable;
  // Keep the window and its Close command enabled. Only commands which need
  // loaded wallet/node state are gated; modal dialogs manage their own host.
  miViews.Enabled := TUserInterface.Started;
  miAccountExplorer.Enabled := LPendingReady;
  miPendingOperations.Enabled := LPendingReady;
  miBlockExplorer.Enabled := LReady;
  miOperationsExplorer.Enabled := LPendingReady;
  miNodes.Enabled := LReady;
  miMessages.Enabled := LReady;
  miSyncDialog.Enabled := LReady;
  miOptions.Enabled := LReady;
  miPrivateKeys.Enabled := LReady;
  miAdvancedOperation.Enabled := LPendingReady and (not TUserInterface.Offline);
  miOperationsWorkbench.Enabled := LPendingReady;
  miRPCCalls.Enabled := LReady and (not TUserInterface.Offline);
  miSeedNodes.Enabled := LReady;
  miAboutMenu.Enabled := LReady;
  tbtnSync.Enabled := LReady;
  RefreshWalletLockIcon;
  RefreshConnectivityIcon;
end;

procedure TFRMMainForm.SetStartupHeightLocked(ALocked: Boolean);
begin
  if FStartupHeightLocked = ALocked then Exit;
  FStartupHeightLocked := ALocked;
  if ALocked then begin
    FStartupMinHeight := Constraints.MinHeight;
    FStartupMaxHeight := Constraints.MaxHeight;
    FStartupBorderIcons := BorderIcons;
    // Keep horizontal sizing and native Close available while the splash is
    // shown. Maximizing would also change its height, so restore that command
    // together with the original constraints when local loading finishes.
    BorderIcons := BorderIcons - [biMaximize];
    Constraints.MinHeight := Height;
    Constraints.MaxHeight := Height;
  end else begin
    Constraints.MinHeight := FStartupMinHeight;
    Constraints.MaxHeight := FStartupMaxHeight;
    BorderIcons := FStartupBorderIcons;
  end;
end;

procedure TFRMMainForm.RefreshConnectionStatusDisplay;
begin
  if TUserInterface.State in [uisLoading, uisError] then
    sbStatusBar.Panels[2].Text := TUserInterface.StateText
  else if Assigned(FSyncControl) then
    // Footer and basic sync labels share one cached committed-node record.
    // Secondary average jobs cannot overwrite this with an older block time.
    sbStatusBar.Panels[2].Text := FSyncControl.ChainStatusText
  else
    sbStatusBar.Panels[2].Text := '';
end;

procedure TFRMMainForm.SetMode(AMode: TFRMMainFormMode);
var nestedForm : TForm;
begin
  if (AMode = wmWallet) and
    ((not TUserInterface.Started) or (TUserInterface.State in [uisLoading, uisError])) then Exit;
  if FMode = AMode then
    exit;
  FMode := AMode;
  PostMessage(self.Handle, CM_PC_ModeChanged, 0, 0);
end;

procedure TFRMMainForm.CM_ModeChanged(var Msg: TMessage);
begin
  try
    FUILock.Acquire;
    case FMode of
      wmWallet: begin
        if NOT Assigned(FWalletControl) then begin
          FWalletControl := CreateWalletControl;
          paWalletPanel.AddControlDockCenter(FWalletControl);
        end;
        paWalletPanel.Visible := true;
        paSyncPanel.Visible := false;
      end;
      wmSync: begin
        paSyncPanel.Visible := true;
        paWalletPanel.Visible := false;
        FSyncControl.RefreshNodeState;
      end;
      else raise Exception.Create('[Internal Error] - TFRMMainForm.SetMode: unsupported mode passed');
    end;
  finally
    FUILock.Release;
  end;
end;

{%endregion}

{%region Handlers: Form}

procedure TFRMMainForm.WndProc(var Msg: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
  // A native maximize command can bypass a disabled caption button.
  if FStartupHeightLocked and (Msg.Msg = WM_SYSCOMMAND) and
    ((Msg.WParam and $FFF0) = SC_MAXIMIZE) then begin
    {$IFDEF FPC}
    // The Win32 LCL widgetset resumes native system-command processing from
    // the message ID, independently of Result. Consume that message as well.
    Msg.Msg := LM_NULL;
    {$ENDIF}
    Msg.Result := 0;
    Exit;
  end;
  inherited;
end;

procedure TFRMMainForm.FormResize(Sender: TObject);
begin
  if FFooterLayoutActive or FFooterLayoutPending or
    (csLoading in ComponentState) or (csDestroying in ComponentState) then Exit;
  if not Assigned(sbStatusBar) or not Assigned(tbStatusToolBar) then Exit;
  // LCL can raise OnResize before the native status panel has processed its
  // new bounds. Reconcile child layout after that resize/scaling pass completes.
  FFooterLayoutPending := True;
  PostMessage(Handle, CM_PC_LayoutFooter, 0, 0);
end;

procedure TFRMMainForm.CM_LayoutFooter(var Msg: TMessage);
begin
  FFooterLayoutPending := False;
  LayoutFooter;
end;

procedure TFRMMainForm.LayoutFooter;
var
  LLeft, LTop, LHeight, LDeficit, LMargin: Integer;
  LContent, LPanelRect: TRect;

  function Scaled(AValue: Integer): Integer;
  begin
    {$IFDEF FPC}Result := Scale96ToFont(AValue);
    {$ELSE}Result := ScaleValue(AValue);{$ENDIF}
  end;

  function FooterContentRect: TRect;
  var LThemedContent: TRect;
  begin
    LPanelRect := sbStatusBar.ClientRect;
    LPanelRect.Left := Max(0, LPanelRect.Right - tbStatusToolBar.Width -
      Scaled(CT_FOOTER_TOOLBAR_LEFT_PADDING + CT_FOOTER_TOOLBAR_RIGHT_PADDING));
    {$IFDEF MSWINDOWS}
    if sbStatusBar.HandleAllocated then
      sbStatusBar.Perform(SB_GETRECT, sbStatusBar.Panels.Count - 1, LPARAM(@LPanelRect));
    {$ENDIF}
    Result := LPanelRect;
    // A status bar paints panel borders inside its client rectangle. Child
    // window containment alone does not keep the toolbar off those borders.
    InflateRect(Result, -LMargin, -LMargin);
    if sbStatusBar.HandleAllocated then begin
      {$IFDEF FPC}
      LThemedContent := ThemeServices.ContentRect(sbStatusBar.Canvas.Handle,
        ThemeServices.GetElementDetails(tsPane), LPanelRect);
      {$ELSE}
      if not StyleServices(sbStatusBar).GetElementContentRect(sbStatusBar.Canvas.Handle,
        StyleServices(sbStatusBar).GetElementDetails(tsPane), LPanelRect, LThemedContent) then
        LThemedContent := Result;
      {$ENDIF}
      Result.Left := Max(Result.Left, LThemedContent.Left);
      Result.Top := Max(Result.Top, LThemedContent.Top);
      Result.Right := Min(Result.Right, LThemedContent.Right);
      Result.Bottom := Min(Result.Bottom, LThemedContent.Bottom);
    end;
    Result.Right := Min(Result.Right,
      sbStatusBar.ClientWidth - Scaled(CT_FOOTER_TOOLBAR_RIGHT_PADDING));
  end;
begin
  if (not Assigned(sbStatusBar)) or (not Assigned(tbStatusToolBar)) then Exit;
  if FFooterLayoutActive or (csLoading in ComponentState) or
    (csDestroying in ComponentState) or (sbStatusBar.Panels.Count < 4) then Exit;
  FFooterLayoutActive := True;
  try
    LMargin := Max(1, Scaled(1));
    LContent := FooterContentRect;
    // Recalculate the complete height so scaling back down also releases space
    // temporarily reserved before the toolbar's own DPI resize has completed.
    LHeight := tbStatusToolBar.Height + Max(2 * Scaled(CT_FOOTER_TOOLBAR_VERTICAL_PADDING),
      sbStatusBar.Height - (LContent.Bottom - LContent.Top) + 2 * LMargin);
    if sbStatusBar.Height <> LHeight then sbStatusBar.Height := LHeight;
    // The native status bar stretches its last panel; reserve its space by
    // sizing the preceding text panel, including padding and the sizing grip.
    sbStatusBar.Panels[2].Width := ClipValue(sbStatusBar.ClientWidth -
      sbStatusBar.Panels[0].Width - sbStatusBar.Panels[1].Width - tbStatusToolBar.Width -
      Scaled(CT_FOOTER_TOOLBAR_LEFT_PADDING + CT_FOOTER_TOOLBAR_RIGHT_PADDING),
      0, sbStatusBar.ClientWidth);
    LContent := FooterContentRect;
    LDeficit := tbStatusToolBar.Height + 2 * LMargin - (LContent.Bottom - LContent.Top);
    if LDeficit > 0 then begin
      sbStatusBar.Height := sbStatusBar.Height + LDeficit;
      LContent := FooterContentRect;
    end;
    LLeft := Max(LContent.Left, LContent.Right - tbStatusToolBar.Width);
    LTop := LContent.Top + Max(0, (LContent.Bottom - LContent.Top - tbStatusToolBar.Height) div 2);
    tbStatusToolBar.SetBounds(LLeft, LTop, tbStatusToolBar.Width, tbStatusToolBar.Height);
  finally
    FFooterLayoutActive := False;
  end;
end;

{%endregion}

{%region Handlers: Wallet, Network}

procedure TFRMMainForm.CM_FinishedLoadingDatabase(var Msg: TMessage);
begin
  RefreshStartupControls;
end;

procedure TFRMMainForm.OnLoaded(Sender: TObject);
begin
  // Ensure handled in UI thread
  PostMessage(Self.Handle,CM_PC_FinishedLoadingDatabase,0,0);
end;

procedure TFRMMainForm.OnStateChanged(Sender: TObject);
begin
  RefreshStartupControls;
  RefreshConnectionStatusDisplay;
  OnNetStatisticsChanged(Sender);
end;

procedure TFRMMainForm.CM_WalletChanged(var Msg: TMessage);
begin
  RefreshWalletLockIcon;
end;

procedure TFRMMainForm.OnWalletChanged(Sender: TObject);
begin
  // Ensure handled in UI thread
  PostMessage(Self.Handle,CM_PC_WalletKeysChanged,0,0);
end;

procedure TFRMMainForm.CM_ConnectivityChanged(var Msg: TMessage);
begin
  RefreshConnectivityIcon;
  OnNetStatisticsChanged(Self);
  FSyncControl.RefreshNodeState;
end;

procedure TFRMMainForm.OnAppStarted(Sender: TObject);
begin
  RefreshStartupControls;
  RefreshConnectionStatusDisplay;
  OnNetStatisticsChanged(Sender);
end;

procedure TFRMMainForm.OnLoading(Sender: TObject; const AProgress : TProgressInfo);
begin
  // TODO: show progress bar
end;

procedure TFRMMainForm.OnConnectivityChanged(Sender: TObject);
begin
  // Ensure handled in UI thread
  PostMessage(Self.Handle,CM_PC_ConnectivityChanged,0,0);
end;

procedure TFRMMainForm.OnNetStatisticsChanged(Sender: TObject);
Var NS : TNetStatistics;
begin
  if Assigned(TUserInterface.Node) then begin
    If TUserInterface.Node.NetServer.Active then begin
      sbStatusBar.Panels[0].Text := 'Active (port: '+Inttostr(TUserInterface.Node.NetServer.Port)+')';
    end else sbStatusBar.Panels[0].Text := 'Server Inactive';
    NS := TNetData.NetData.NetStatistics;
    sbStatusBar.Panels[1].Text  := Format('Connections: %d Clients: %d Servers: %d - Rcvd: %d kb Sent: %d kb',
      [NS.ActiveConnections,NS.ClientsConnections, NS.ServersConnections, NS.BytesReceived DIV 1024, NS.BytesSend DIV 1024]);
  end else begin
    sbStatusBar.Panels[0].Text := '';
    sbStatusBar.Panels[1].Text := '';
  end;
end;

procedure TFRMMainForm.OnSyncSnapshotChanged(Sender: TObject);
begin
  RefreshConnectionStatusDisplay;
end;

{$IFDEF TESTNET}
procedure TFRMMainForm.InitMenuForTesting;
var mi : TMenuItem;
begin
  mi := TMenuItem.Create(meMainMenu);
  mi.Caption:='-';
  miAboutMenu.Add(mi);
  {$IFDEF TESTING_NO_POW_CHECK}
  mi := TMenuItem.Create(meMainMenu);
  mi.Caption:='Create a block';
  mi.OnClick:=Test_CreateABlock;
  miAboutMenu.Add(mi);
  {$ENDIF}
  mi := TMenuItem.Create(meMainMenu);
  mi.Caption:='Show public keys state';
  mi.OnClick:=Test_ShowPublicKeys;
  miAboutMenu.Add(mi);
  mi := TMenuItem.Create(meMainMenu);
  mi.Caption:='Create Random operations';
  mi.OnClick:=Test_RandomOperations;
  miAboutMenu.Add(mi);
end;

procedure TFRMMainForm.Test_RandomOperations(Sender: TObject);
begin
  TUserInterface.ShowRandomOperationsDialog(Self);
end;

procedure TFRMMainForm.Test_ShowPublicKeys(Sender: TObject);
begin
  TUserInterface.ShowPublicKeysDialog(Self);
end;
{$IFDEF TESTING_NO_POW_CHECK}
procedure TFRMMainForm.Test_CreateABlock(Sender: TObject);
begin
  TUserInterface.CreateABlock;
end;
{$ENDIF}
{$ENDIF}

{%endregion}

{%region Handlers: Menu Items }

procedure TFRMMainForm.miMessagesClick(Sender: TObject);
begin
  TUserInterface.ShowMessagesForm;
end;

procedure TFRMMainForm.miSyncDialogClick(Sender:TObject);
begin
  TUserInterface.ShowSyncDialog;
end;

procedure TFRMMainForm.miNodesClick(Sender: TObject);
begin
  TUserInterface.ShowNodesForm;
end;

procedure TFRMMainForm.miPendingOperationsClick(Sender: TObject);
begin
  TUserInterface.ShowPendingOperations
end;

procedure TFRMMainForm.miLogsClick(Sender: TObject);
begin
  TUserInterface.ShowLogsForm;
end;

procedure TFRMMainForm.miOperationsExplorerClick(Sender: TObject);
begin
 TUserInterface.ShowOperationsExplorer;
end;

procedure TFRMMainForm.miBlockExplorerClick(Sender: TObject);
begin
  TUserInterface.ShowBlockExplorer;
end;

procedure TFRMMainForm.miAccountExplorerClick(Sender: TObject);
begin
  TUserInterface.ShowAccountExplorer;
end;

procedure TFRMMainForm.miSeedNodesClick(Sender: TObject);
begin
  TUserInterface.ShowSeedNodesDialog(Self);
end;

procedure TFRMMainForm.miAboutClick(Sender: TObject);
begin
  TUserInterface.ShowAboutBox(Self);
end;

procedure TFRMMainForm.miCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFRMMainForm.miOptionsClick(Sender: TObject);
begin
  TUserInterface.ShowOptionsDialog(Self);
end;

procedure TFRMMainForm.miPrivateKeysClick(Sender: TObject);
begin
  TUserInterface.ShowPrivateKeysDialog(Self);
end;

procedure TFRMMainForm.miAdvancedOperationClick(Sender: TObject);
var
  LScoped : TDisposables;
  LAccounts : TOrderedCardinalList;
  LAccount : Cardinal;
begin
  if not Assigned(FWalletControl) then Exit;
  LAccounts := LScoped.AddObject(TOrderedCardinalList.Create) as TOrderedCardinalList;
  for LAccount in FWalletControl.SelectedAccounts do
    LAccounts.Add(LAccount);
  TUserInterface.ShowNewOperationDialog(Self, LAccounts, TSettings.DefaultFee);
end;

procedure TFRMMainForm.miOperationsWorkbenchClick(Sender: TObject);
begin
  TUserInterface.ShowOperationsWorkbench(Self);
end;

procedure TFRMMainForm.miRPCCallsClick(Sender: TObject);
begin
  TUserInterface.ShowRPCCallsDialog(Self);
end;

{%endregion}

{%region Handlers: Footer Toolbar}

procedure TFRMMainForm.tbtnWalletLockClick(Sender:TObject);
begin
  if NOT TWallet.Keys.HasPassword then begin
     { no password has been set }
     ShowMessage('Your wallet has no password and is unsafe. Please set a password using ''Key Manager'' in Wallet menu.');
   end else if TWallet.Keys.IsValidPassword then begin
     { wallet is unlocked, lock it }
     TWallet.Keys.LockWallet;
   end else begin
     { wallet is locked, try to unlock it}
     TUserInterface.UnlockWallet(Self);
   end;
end;

procedure TFRMMainForm.tbtnConnectivityClick(Sender:TObject);
begin
  if TUserInterface.Offline or (TUserInterface.State in [uisLoading, uisError]) then Exit;
  // Toggle connectivity (icons update with event handler)
  TUserInterface.ToggleConnectivity;
end;

procedure TFRMMainForm.tbtnSyncClick(Sender: TObject);
begin
  case Mode of
    wmSync: Mode := wmWallet;
    wmWallet: Mode := wmSync;
  end;
end;

{%endregion}

Procedure TFRMMainForm.InitMacOSMenu;
{$ifdef fpc}
var
  mi : TMenuItem;
  app : TMenuItem;
{$endif}
begin
  {$ifdef fpc}
  {$ifndef darwin}
  Exit;
  {$endif}
  app := TMenuItem.Create(meMainMenu);
  app := TMenuItem.Create(meMainMenu);
  app.Caption:=#$EF#$A3#$BF;

  miAbout.Parent.Remove(miAbout);
  app.Add(miAbout);
  {$IFDEF TESTNET}
  // Things for testing purposes only
  miAboutMenu.caption := 'Testing';
  {$ELSE}
  miAboutMenu.Visible:=false;
  {$ENDIF}

  // Move "Options" to "Prefernces"
  mi := TMenuItem.Create(meMainMenu);
  mi.Caption:='-';
  app.Add(mi);
  miOptions.Parent.Remove(miOptions);
  miOptions.Caption:='Preferences...';
  miOptions.ShortCut:=Menus.ShortCut(VK_OEM_COMMA, [ssMeta]);
  app.Add(miOptions);

  MiOperationsExplorer.ShortCut:=Menus.ShortCut(VK_E, [ssMeta]);
  miOptions.ShortCut:=Menus.ShortCut(VK_OEM_COMMA, [ssMeta]); // match preferences
  miPrivatekeys.ShortCut:=Menus.Shortcut(VK_P, [ssMeta]);

  N1.Visible:=false;
  MiClose.Visible:=false;

  meMainMenu.Items.Insert(0, app);

  paWalletPanel.Caption:='';
  {$endif}
end;

end.

