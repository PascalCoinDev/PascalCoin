unit UFRMWallet;

{$mode delphi}

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical miPendingOperations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

interface

{$I ./../PascalCoin/config.inc}

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, UWalletKeys,
  ULog,
  UBlockChain, UNode, UGridUtils, UAccounts, Menus,
  UNetProtocol, UCrypto, Buttons, ActnList, UPoolMining,
  UCommon, UCommonUI;

Const
  CM_PC_WalletKeysChanged = WM_USER + 1;
  CM_PC_ConnectivityChanged = WM_USER + 2;

type

  { TFRMWallet }

  TFRMWallet = class(TApplicationForm)
    ilSmallIcons:TImageList;
    miSyncDialog:TMenuItem;
    miBlockExplorer: TMenuItem;
    miLogs: TMenuItem;
    miOperationsExplorer: TMenuItem;
    miMessages: TMenuItem;
    miNodes: TMenuItem;
    miPendingOperations: TMenuItem;
    PanelWindow:TPanel;
    miViews: TMenuItem;
    miAccountExplorer: TMenuItem;
    sbFooterBar: TStatusBar;
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
    tbFooter: TToolBar;
    tbtnConnectivity: TToolButton;
    tbtnSync: TToolButton;
    tbtnWalletLock: TToolButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
    procedure sbFooterBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
    procedure ApplicationEventsMinimize(Sender: TObject);
    procedure miSeedNodesClick(Sender: TObject);
  private
    FLastFooterToolBarDrawRect : TRect;
    procedure CM_WalletChanged(var Msg: TMessage); message CM_PC_WalletKeysChanged;
    procedure CM_ConnectivityChanged(var Msg: TMessage); message CM_PC_ConnectivityChanged;
    procedure OnConnectivityChanged(Sender: TObject);
    procedure OnWalletChanged(Sender: TObject);
  protected
    procedure RefreshWalletLockIcon;
    procedure RefreshConnectivityIcon;
    procedure ActivateFirstTime; override;
    Function ForceMining : Boolean; virtual;
  end;

implementation

{$R *.lfm}

uses LCLIntf, UUserInterface, UThread, UOpTransaction;

const
  CT_FOOTER_TOOLBAR_LEFT_PADDING = 8;
  CT_FOOTER_TOOLBAR_RIGHT_PADDING = 20;

{ TFRMWallet }

{%region Form life-cycle }

procedure TFRMWallet.FormCreate(Sender: TObject);
begin
  tbFooter.Parent := sbFooterBar;
  FLastFooterToolBarDrawRect := TRect.Empty;
end;

procedure TFRMWallet.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin

end;

procedure TFRMWallet.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
  // TODO - ask to go background or quit
  // if quit then
  TUserInterface.Quit;

end;

procedure TFRMWallet.ActivateFirstTime;
begin
  TUserInterface.WalletKeys.OnChanged.Add(OnWalletChanged);
  TNetData.NetData.OnConnectivityChanged.Add(OnConnectivityChanged);
  RefreshWalletLockIcon;
  RefreshConnectivityIcon;
end;

procedure TFRMWallet.FormDestroy(Sender: TObject);
begin
  TUserInterface.WalletKeys.OnChanged.Remove(OnWalletChanged);
  TNetData.NetData.OnConnectivityChanged.Remove(OnConnectivityChanged);
end;

procedure TFRMWallet.ApplicationEventsMinimize(Sender: TObject);
begin
  // TODO refactor
  {$IFnDEF FPC}
  Hide();
  WindowState := wsMinimized;
  TimerUpdateStatus.Enabled := false;
  { Show the animated tray icon and also a hint balloon. }
  TrayIcon.Visible := True;
  TrayIcon.ShowBalloonHint;
  {$ENDIF}
end;

{%endregion}

{%region Form methods}

function TFRMWallet.ForceMining: Boolean;
begin
  Result := false;
end;

procedure TFRMWallet.RefreshWalletLockIcon;
begin
  if NOT TUserInterface.WalletKeys.HasPassword then begin
    { No password has been set }
    tbtnWalletLock.Enabled := false;
    tbtnWalletLock.ImageIndex := 3;
    tbtnWalletLock.Hint := 'Your wallet does not have a password.';
  end else if TUserInterface.WalletKeys.IsValidPassword then begin
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

procedure TFRMWallet.RefreshConnectivityIcon;
const
  ImageIndexConst : array[false..true] of integer=(1,0);
  HintConst : array[false..true] of String =('Network is inactive. Click activate.','Network is active. Click to deactivate.');
begin
  tbtnConnectivity.ImageIndex := ImageIndexConst[TNetData.NetData.NetConnectionsActive];
  tbtnConnectivity.Hint :=  HintConst[TNetData.NetData.NetConnectionsActive];
end;

{%endregion}

{%region Handlers: Form}

procedure TFRMWallet.FormResize(Sender: TObject);
begin
  // Adjust status bar so that footer toolbar panel remains fixed size (by expanding out previous panel)
   sbFooterBar.Panels[2].Width := ClipValue(sbFooterBar.Width - (sbFooterBar.Panels[0].Width + sbFooterBar.Panels[1].Width + tbFooter.Width  + CT_FOOTER_TOOLBAR_RIGHT_PADDING) - CT_FOOTER_TOOLBAR_LEFT_PADDING, 0, sbFooterBar.Width);
end;

{%endregion}

{%region Handlers: Wallet, Network}

procedure TFRMWallet.CM_WalletChanged(var Msg: TMessage);
begin
  RefreshWalletLockIcon;
end;

procedure TFRMWallet.OnWalletChanged(Sender: TObject);
begin
  // Ensure handled in UI thread
  PostMessage(Self.Handle,CM_PC_WalletKeysChanged,0,0);
end;

procedure TFRMWallet.CM_ConnectivityChanged(var Msg: TMessage);
begin
  RefreshConnectivityIcon;
end;

procedure TFRMWallet.OnConnectivityChanged(Sender: TObject);
begin
  // Ensure handled in UI thread
  PostMessage(Self.Handle,CM_PC_ConnectivityChanged,0,0);
end;

{%endregion}

{%region Handlers: Menu Items }

procedure TFRMWallet.miMessagesClick(Sender: TObject);
begin
  //HS
  //// in memory for not exit program - Application.Exit - auto free mem not need control free manual for this send Self!
  //if FRMMessages = nil then FRMMessages:=TFRMMessages.Create(Self);
  //SetSubFormCoordinate(FRMMessages);
  //FRMMessages.show;
  TUserInterface.ShowMessagesForm;
end;

procedure TFRMWallet.miSyncDialogClick(Sender:TObject);
begin
  //HS
  // in memory for not exit program - Application.Exit - auto free mem not need control free manual for this send Self!
  //if FRMSyncronizationDialog = nil then FRMSyncronizationDialog:=TFRMSyncronizationDialog.Create(Self);
  //SetSubFormCoordinate(FRMSyncronizationDialog);
  //FRMSyncronizationDialog.Visible:=not FRMSyncronizationDialog.Visible;

  TUserInterface.ShowSyncronizationDialog(Self);
end;

procedure TFRMWallet.miNodesClick(Sender: TObject);
begin
  TUserInterface.ShowNodesForm;
end;

procedure TFRMWallet.miPendingOperationsClick(Sender: TObject);
begin
  TUserInterface.ShowPendingOperations
end;

procedure TFRMWallet.miLogsClick(Sender: TObject);
begin
  TUserInterface.ShowLogsForm;
end;

procedure TFRMWallet.miOperationsExplorerClick(Sender: TObject);
begin
 TUserInterface.ShowOperationsExplorer;
end;

procedure TFRMWallet.miBlockExplorerClick(Sender: TObject);
begin
  TUserInterface.ShowBlockExplorer;
end;

procedure TFRMWallet.miAccountExplorerClick(Sender: TObject);
begin
  // in memory for not exit program - Application.Exit - auto free mem not need control free manual for this send Self!
  //HS
  //if FRMAccountExplorer = nil then FRMAccountExplorer:=TFRMAccountExplorer.Create(Self);
  //SetSubFormCoordinate(FRMAccountExplorer);
  //FRMAccountExplorer.show;

  TUserInterface.ShowAccountExplorer
end;

procedure TFRMWallet.miSeedNodesClick(Sender: TObject);
begin
  TUserInterface.ShowSeedNodesDialog(Self);
end;

procedure TFRMWallet.miAboutClick(Sender: TObject);
begin
  TUserInterface.ShowAboutBox(Self);
end;

procedure TFRMWallet.MiCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFRMWallet.miOptionsClick(Sender: TObject);
begin
  TUserInterface.ShowOptionsDialog(Self);
end;

procedure TFRMWallet.miPrivateKeysClick(Sender: TObject);
begin
  TUserInterface.ShowPrivateKeysDialog(Self);
end;

{%endregion}

{%region Handlers: Footer Toolbar}

procedure TFRMWallet.tbtnWalletLockClick(Sender:TObject);
begin
  if NOT TUserInterface.WalletKeys.HasPassword then begin
     { no password has been set }
     ShowMessage('Your wallet has no password and is unsafe. Please set a password using ''Key Manager'' in Wallet menu.');
   end else if TUserInterface.WalletKeys.IsValidPassword then begin
     { wallet is unlocked, lock it }
     TUserInterface.WalletKeys.LockWallet;
   end else begin
     { wallet is locked, try to unlock it}
     TUserInterface.UnlockWallet(Self, TUserInterface.WalletKeys);
   end;
end;

procedure TFRMWallet.tbtnConnectivityClick(Sender:TObject);
begin
  // Toggle connectivity (icons update with event handler)
  TNetData.NetData.NetConnectionsActive := NOT TNetData.NetData.NetConnectionsActive;
end;

procedure TFRMWallet.tbtnSyncClick(Sender: TObject);
begin
  TUserInterface.ShowSyncronizationDialog(Self);
end;

procedure TFRMWallet.sbFooterBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
begin
  if FLastFooterToolBarDrawRect = Rect then exit; // avoid FPC bug: triggers infinite draw refresh on windows
  if Panel = sbFooterBar.Panels[3] then
    with tbFooter do begin
      Top := Rect.Top + (Rect.Height - tbFooter.Height) div 2;
      Left := Rect.Right - tbFooter.Width - CT_FOOTER_TOOLBAR_RIGHT_PADDING;
      Visible := true;
    end;
  FLastFooterToolBarDrawRect := Rect;
end;

{%endregion}

end.

