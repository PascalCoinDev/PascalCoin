unit UFRMWallet;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

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
{$IFnDEF FPC}
  pngimage, Windows, AppEvnts, ShlObj,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, UWalletKeys, StdCtrls,
  ULog, Grids, UAppParams,
  UBlockChain, UNode, UGridUtils, UAccounts, Menus,// Types,
  UNetProtocol, UCrypto, Buttons, ButtonPanel, ActnList, UPoolMining, URPC,
  UFRMAccountSelect, UCommon, UCommonUI, UFRMSyncronizationDialog;

Const
  CM_PC_WalletKeysChanged = WM_USER + 1;
  CM_PC_NetConnectionUpdated = WM_USER + 2;

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
    miPrivatekeys: TMenuItem;
    miN1: TMenuItem;
    miAbout: TMenuItem;
    miAboutPascalCoin: TMenuItem;
    N1: TMenuItem;
    miClose: TMenuItem;
    ilIcons: TImageList;
    ApplicationEvents: {$IFDEF FPC}TApplicationProperties{$ELSE}TApplicationEvents{$ENDIF};
    IPnodes1: TMenuItem;
    pnlSelectedAccountsBottom: TPanel;
    tbFooter: TToolBar;
    tbtnConnectivity: TToolButton;
    tbtnSync: TToolButton;
    tbtnWalletLock: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure miAccountExplorerClick(Sender: TObject);
    procedure miBlockExplorerClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miLogsClick(Sender: TObject);
    procedure miMessagesClick(Sender: TObject);
    procedure miSyncDialogClick(Sender:TObject);
    procedure sbFooterBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure tbtnConnectivityClick(Sender:TObject);
    procedure miNodesClick(Sender: TObject);
    procedure miPendingOperationsClick(Sender: TObject);
    procedure miOperationsExplorerClick(Sender: TObject);
    procedure tbtnSyncClick(Sender: TObject);
    procedure tbtnWalletLockClick(Sender:TObject);
    procedure miOptionsClick(Sender: TObject);
    procedure miAboutPascalCoinClick(Sender: TObject);
    procedure miPrivatekeysClick(Sender: TObject);
    procedure miCloseClick(Sender: TObject);
    procedure MiDecodePayloadFOperationsExplorerGridClick(Sender: TObject);
    procedure ApplicationEventsMinimize(Sender: TObject);
    procedure IPnodes1Click(Sender: TObject);
  private
    FLastFooterToolBarDrawRect : TRect;
    procedure OnConnectivityChanged(Sender: TObject);
    procedure OnWalletChanged(Sender: TObject);
  protected
    procedure RefreshWalletLockIcon;
    procedure RefreshConnectivityIcon;
    procedure ActivateFirstTime; override;
    Function ForceMining : Boolean; virtual;
    procedure CM_WalletChanged(var Msg: TMessage); message CM_PC_WalletKeysChanged;
  end;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

uses UThread, UOpTransaction, UUserInterface;

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

procedure TFRMWallet.ActivateFirstTime;
var count : Integer;
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

procedure TFRMWallet.CM_WalletChanged(var Msg: TMessage);
begin
  OnWalletChanged(Self);
end;

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
var   active : boolean;
const

  ImageIndexConst : array[false..true] of integer=(1,0);
  HintConst : array[false..true] of String =('Network is inactive. Click activate.','Network is active. Click to deactivate.');
begin
  active := TNetData.NetData.NetConnectionsActive;
  tbtnConnectivity.ImageIndex := ImageIndexConst[TNetData.NetData.NetConnectionsActive];
  tbtnConnectivity.Hint :=  HintConst[TNetData.NetData.NetConnectionsActive];
end;

{%endregion}

{%region Handlers: Form size}

procedure TFRMWallet.FormResize(Sender: TObject);
begin
  // Adjust status bar so that footer toolbar panel remains fixed size (by expanding out previous panel)
   sbFooterBar.Panels[2].Width := ClipValue(sbFooterBar.Width - (sbFooterBar.Panels[0].Width + sbFooterBar.Panels[1].Width + tbFooter.Width  + CT_FOOTER_TOOLBAR_RIGHT_PADDING) - CT_FOOTER_TOOLBAR_LEFT_PADDING, 0, sbFooterBar.Width);
end;

{%endregion}

{%region Handlers: Wallet, Network}

procedure TFRMWallet.OnWalletChanged(Sender: TObject);
begin
  RefreshWalletLockIcon;
end;

procedure TFRMWallet.OnConnectivityChanged(Sender: TObject);
begin
  RefreshConnectivityIcon;
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

procedure TFRMWallet.IPnodes1Click(Sender: TObject);
begin
  TUserInterface.ShowNodeIPDialog(Self);
end;

procedure TFRMWallet.miAboutPascalCoinClick(Sender: TObject);
begin
  TUserInterface.ShowAboutBox(Self);
end;

procedure TFRMWallet.MiCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFRMWallet.MiDecodePayloadFOperationsExplorerGridClick(Sender: TObject);
begin
  raise Exception.Create('Not Implemented');
//  { TODO 5 -oAntonB -csub-forms :
//  Menu Item Decode PayLoad use info about active page. That Shows different modal forms on different active pages. I delete pages now. I change Decode PayLoad to:
//  I add 3 menu Items:Decode Payload miPendingOperations Explorer, Decode Payload Pending, miPendingOperations,Decode Payload My miAccountExplorer }
////  if PageControl.ActivePage=tsOperations then begin
//  if Sender=MiDecodePayloadFOperationsExplorerGrid then begin
//    FOperationsExplorerGrid.ShowModalDecoder(TUserInterface.WalletKeys,TUserInterface.AppParams);
////  end else if PageControl.ActivePage=tsPendingOperations then begin
//  end else if Sender=MiDecodePayLoadPendingOperations then begin
//    FPendingOperationsGrid.ShowModalDecoder(TUserInterface.WalletKeys,TUserInterface.AppParams);
////  end else if PageControl.ActivePage=tsMyAccounts then begin
//  end else if Sender=MiDecodePayloadOperationsAccount then begin
//    FOperationsAccountGrid.ShowModalDecoder(TUserInterface.WalletKeys,TUserInterface.AppParams);
//  end;
end;

procedure TFRMWallet.miOptionsClick(Sender: TObject);
begin
  TUserInterface.ShowOptionsDialog(Self);
end;

procedure TFRMWallet.miPrivatekeysClick(Sender: TObject);
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

