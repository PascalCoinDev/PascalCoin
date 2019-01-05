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

{$mode delphi}

interface

{$I ..\config.inc}

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, LMessages, UWallet,
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
    miN1: TMenuItem;
    miAboutMenu: TMenuItem;
    miAbout: TMenuItem;
    N1: TMenuItem;
    miClose: TMenuItem;
    ilIcons: TImageList;
    ApplicationEvents: TApplicationProperties;
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
    procedure miCloseClick(Sender: TObject);
    procedure tbtnSyncClick(Sender: TObject);
    procedure tbtnWalletLockClick(Sender:TObject);
    procedure tbtnConnectivityClick(Sender:TObject);
    procedure miSeedNodesClick(Sender: TObject);
  private
    FMode : TFRMMainFormMode;
    FSyncControl : TCTRLSyncronization;
    FWalletControl : TCTRLWallet;
    procedure CM_FinishedLoadingDatabase(var Msg: TMessage); message CM_PC_FinishedLoadingDatabase;
    procedure CM_WalletChanged(var Msg: TMessage); message CM_PC_WalletKeysChanged;
    procedure CM_ConnectivityChanged(var Msg: TMessage); message CM_PC_ConnectivityChanged;
    procedure CM_Terminate(var Msg: TMessage); message CM_PC_Terminate;
    procedure CM_ModeChanged(var Msg: TMessage); message CM_PC_ModeChanged;

    procedure OnAppStarted(Sender: TObject);
    procedure OnLoaded(Sender: TObject);
    procedure OnConnectivityChanged(Sender: TObject);
    procedure OnNetStatisticsChanged(Sender: TObject);
    procedure OnWalletChanged(Sender: TObject);
    procedure OnUIRefreshTimer(Sender: TObject);
    {$IFDEF TESTNET}
    procedure InitMenuForTesting;
    procedure Test_RandomOperations(Sender: TObject);
    procedure Test_ShowPublicKeys(Sender: TObject);
    {$IFDEF TESTING_NO_POW_CHECK}
    procedure Test_CreateABlock(Sender: TObject);
    {$ENDIF}
    {$ENDIF}
  protected
    procedure RefreshConnectionStatusDisplay;
    procedure RefreshWalletLockIcon;
    procedure RefreshConnectivityIcon;
    procedure ActivateFirstTime; override;
    Function ForceMining : Boolean; virtual;
    procedure SetMode(AMode: TFRMMainFormMode);
  public
    property SyncControl : TCTRLSyncronization read FSyncControl;
    property Mode : TFRMMainFormMode read FMode write SetMode;
  end;

implementation

{$R *.lfm}

uses LCLIntf, UUserInterface, UThread, UOpTransaction, UWizard, UTime;

const
  CT_FOOTER_TOOLBAR_LEFT_PADDING = 8;
  CT_FOOTER_TOOLBAR_RIGHT_PADDING = 20;

{ TFRMWallet }

{%region Form life-cycle }

procedure TFRMMainForm.FormCreate(Sender: TObject);

  procedure AdjustStatusToolBar;
  var
    LHeightDiff: Integer;
  begin
    LHeightDiff := sbStatusBar.Height - tbStatusToolBar.Height;
    if LHeightDiff >= 0 then
      Exit;

    tbStatusToolBar.AnchorSideTop.Side := asrTop;
    tbStatusToolBar.Anchors := [akRight, akBottom];
  end;

begin
  AdjustStatusToolBar;

  CloseAction := caNone; // Will handle terminate in separate method
  Mode := wmSync;
  FSyncControl := TCTRLSyncronization.Create(self);
  paLogoPanel.AddControlDockCenter(TCTRLBanner.Create(Self));
  paSyncPanel.AddControlDockCenter(FSyncControl);
  // note: wallet control is lazily constructed
  {$IFDEF TESTNET}
  // Things for testing purposes only
  InitMenuForTesting;
  {$ENDIF}
end;

procedure TFRMMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
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
  // Event subscriptions on activate, not create, due to load-time sequence
  TUserInterface.AppStarted.Add(OnAppStarted);
  TWallet.Keys.OnChanged.Add(OnWalletChanged);
  TNetData.NetData.OnConnectivityChanged.Add(OnConnectivityChanged);
  TUserInterface.NetStatisticsChanged.Add(OnNetStatisticsChanged);
  TUserInterface.UIRefreshTimer.Add(OnUIRefreshTimer);
  RefreshWalletLockIcon;
  RefreshConnectivityIcon;
end;

procedure TFRMMainForm.FormDestroy(Sender: TObject);
begin
  TUserInterface.AppStarted.Remove(OnAppStarted);
  TWallet.Keys.OnChanged.Remove(OnWalletChanged);
  TNetData.NetData.OnConnectivityChanged.Remove(OnConnectivityChanged);
  TUserInterface.NetStatisticsChanged.Remove(OnNetStatisticsChanged);
  TUserInterface.UIRefreshTimer.Remove(OnUIRefreshTimer);
end;

{%endregion}

{%region Form methods}

function TFRMMainForm.ForceMining: Boolean;
begin
  Result := false;
end;

procedure TFRMMainForm.RefreshWalletLockIcon;
begin
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
end;

procedure TFRMMainForm.RefreshConnectionStatusDisplay;
var errors : AnsiString;
begin
  //HS XXXXXXXXXXXX was triggering below handlers, ignore?
  //FMainForm.SyncControl.UpdateNodeStatus;
  //HandleNetStatisticsChangedBackend(FMainForm);
  if Assigned(TUserInterface.Node) then begin
    if TUserInterface.Node.IsBlockChainValid(errors) then begin
      sbStatusBar.Panels[2].Text := Format('Last account time:%s', [FormatDateTime('dd/mm/yyyy hh:nn:ss',UnivDateTime2LocalDateTime(UnixToUnivDateTime( TUserInterface.Node.Bank.LastOperationBlock.timestamp )))]);
    end else begin
      sbStatusBar.Panels[2].Text := 'NO BLOCKCHAIN: '+errors;
    end;
  end else begin
    sbStatusBar.Panels[2].Text := '';
  end;
end;

procedure TFRMMainForm.SetMode(AMode: TFRMMainFormMode);
var nestedForm : TForm;
begin
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
          FWalletControl := TCTRLWallet.Create(self);
          paWalletPanel.AddControlDockCenter(FWalletControl);
        end;
        paWalletPanel.Visible := true;
        paSyncPanel.Visible := false;
      end;
      wmSync: begin
        paSyncPanel.Visible := true;
        paWalletPanel.Visible := false;;
      end;
      else raise Exception.Create('[Internal Error] - TFRMMainForm.SetMode: unsupported mode passed');
    end;
  finally
    FUILock.Release;
  end;
end;

{%endregion}

{%region Handlers: Form}

procedure TFRMMainForm.FormResize(Sender: TObject);
begin
  // Adjust status bar so that footer toolbar panel remains fixed size (by expanding out previous panel)
   sbStatusBar.Panels[2].Width := ClipValue(sbStatusBar.Width - (sbStatusBar.Panels[0].Width + sbStatusBar.Panels[1].Width + tbStatusToolBar.Width  + CT_FOOTER_TOOLBAR_RIGHT_PADDING) - CT_FOOTER_TOOLBAR_LEFT_PADDING, 0, sbStatusBar.Width);
end;

{%endregion}

{%region Handlers: Wallet, Network}

procedure TFRMMainForm.CM_FinishedLoadingDatabase(var Msg: TMessage);
begin
  Self.Enabled:=true;
end;

procedure TFRMMainForm.OnLoaded(Sender: TObject);
begin
  // Ensure handled in UI thread
  PostMessage(Self.Handle,CM_PC_FinishedLoadingDatabase,0,0);
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
end;

procedure TFRMMainForm.OnAppStarted(Sender: TObject);
begin
  RefreshConnectionStatusDisplay;
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

procedure TFRMMainForm.OnUIRefreshTimer(Sender: TObject);
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
  // Toggle connectivity (icons update with event handler)
  TNetData.NetData.NetConnectionsActive := NOT TNetData.NetData.NetConnectionsActive;
end;

procedure TFRMMainForm.tbtnSyncClick(Sender: TObject);
begin
  case Mode of
    wmSync: Mode := wmWallet;
    wmWallet: Mode := wmSync;
  end;
end;

{%endregion}

end.

