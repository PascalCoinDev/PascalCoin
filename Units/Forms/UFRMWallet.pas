unit UFRMWallet;

{$mode delphi}

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
  - Albert Molina: portions of code copied from https://github.com/PascalCoin/PascalCoin/blob/master/Units/Forms/UFRMWallet.pas
}

interface

{$I ./../PascalCoin/config.inc}

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, UWalletKeys,
  ULog,
  UBlockChain, UNode, UGridUtils, UAccounts, Menus,
  UNetProtocol, UCrypto, Buttons, ActnList, UPoolMining,
  UCTRLBanner, UCommon, UCommonUI;

Const
  CM_PC_FinishedLoadingDatabase = WM_USER + 1;
  CM_PC_WalletKeysChanged = WM_USER + 2;
  CM_PC_ConnectivityChanged = WM_USER + 3;
  CM_PC_Terminate = WM_USER + 4;

type

  { TFRMWalletMode }

  TFRMWalletMode = (wmWallet, wmSync);

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
    procedure paLogoPanelClick(Sender: TObject);
    procedure tbtnSyncClick(Sender: TObject);
    procedure tbtnWalletLockClick(Sender:TObject);
    procedure tbtnConnectivityClick(Sender:TObject);
    procedure sbStatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
    procedure miSeedNodesClick(Sender: TObject);
  private
    __FLastFooterToolBarDrawRect : TRect;  // Required for FPC bug work-around
    FMode : TFRMWalletMode;
    FSyncPaneControl : TForm;
    procedure CM_FinishedLoadingDatabase(var Msg: TMessage); message CM_PC_FinishedLoadingDatabase;
    procedure CM_WalletChanged(var Msg: TMessage); message CM_PC_WalletKeysChanged;
    procedure CM_ConnectivityChanged(var Msg: TMessage); message CM_PC_ConnectivityChanged;
    procedure CM_Terminate(var Msg: TMessage); message CM_PC_Terminate;
    procedure OnConnectivityChanged(Sender: TObject);
    procedure OnWalletChanged(Sender: TObject);
  protected
    property SyncPaneControl : TForm read FSyncPaneControl write FSyncPaneControl;
    procedure RefreshWalletLockIcon;
    procedure RefreshConnectivityIcon;
    procedure ActivateFirstTime; override;
    Function ForceMining : Boolean; virtual;
    procedure SetMode(AMode: TFRMWalletMode);
  public
    property Mode : TFRMWalletMode read FMode write SetMode;
    procedure OnFinishedLoadingDatabase;
  end;

implementation

{$R *.lfm}

uses LCLIntf, UUserInterface, UThread, UOpTransaction, UWizard, UFRMSyncronizationDialog;

const
  CT_FOOTER_TOOLBAR_LEFT_PADDING = 8;
  CT_FOOTER_TOOLBAR_RIGHT_PADDING = 20;

{ TFRMWallet }

{%region Form life-cycle }

procedure TFRMWallet.FormCreate(Sender: TObject);
begin
  tbStatusToolBar.Parent := sbStatusBar;
  __FLastFooterToolBarDrawRect := TRect.Empty;
  CloseAction := caNone; // Will handle terminate in separate method
  FMode := wmSync;
  paLogoPanel.AddControlDockCenter(TCTRLBanner.Create(Self));
  //paSyncPanel.AddControlDockCenter(TFRMSyncronizationDialog.Create(Self));
end;

procedure TFRMWallet.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  case TUserInterface.AskQuestion(Self, 'Quit PascalCoin', 'Are you sure you want to quit? Select ''No'' to run in background.', [mbCancel, mbNo, mbYes]) of
    mbYes: begin
      CanClose := false;
      PostMessage(Self.Handle, CM_PC_Terminate, 0, 0);
    end;
    mbNo: begin
      CanClose := false;
      TUserInterface.RunInBackground;
    end;
    mbCancel: CanClose := false;
  end;
end;

procedure TFRMWallet.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
end;

procedure TFRMWallet.CM_Terminate(var Msg: TMessage);
begin
  TUserInterface.ExitApplication;
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

procedure TFRMWallet.SetMode(AMode: TFRMWalletMode);
var nestedForm : TForm;
begin
  case AMode of
    wmWallet: begin
      paWalletPanel.Visible := true;
      paSyncPanel.Visible := false;
      FMode := AMode;
    end;
    wmSync: begin
      if not paSyncPanel.ContainsControl(TUserInterface.SyncDialog) then begin
        paSyncPanel.RemoveAllControls(false);
        TUserInterface.SyncDialog.Top := 0;
        TUserInterface.SyncDialog.Left := 0;
        TUserInterface.SyncDialog.Width := paSyncPanel.Width;
        TUserInterface.SyncDialog.Height := paSyncPanel.Height;
        Application.ProcessMessages;
        paSyncPanel.AddControlDockCenter(TUserInterface.SyncDialog);
      end;
      paSyncPanel.Visible := true;
      paWalletPanel.Visible := false;;
      FMode := AMode;
    end;
    else raise Exception.Create('[Internal Error] - TFRMWallet.SetMode: unsupported mode passed');
  end;
end;

{%endregion}

{%region Handlers: Form}

procedure TFRMWallet.FormResize(Sender: TObject);
begin
  // Adjust status bar so that footer toolbar panel remains fixed size (by expanding out previous panel)
   sbStatusBar.Panels[2].Width := ClipValue(sbStatusBar.Width - (sbStatusBar.Panels[0].Width + sbStatusBar.Panels[1].Width + tbStatusToolBar.Width  + CT_FOOTER_TOOLBAR_RIGHT_PADDING) - CT_FOOTER_TOOLBAR_LEFT_PADDING, 0, sbStatusBar.Width);
end;

{%endregion}

{%region Handlers: Wallet, Network}

procedure TFRMWallet.CM_FinishedLoadingDatabase(var Msg: TMessage);
begin
  Self.Enabled:=true;
end;

procedure TFRMWallet.OnFinishedLoadingDatabase;
begin
  // Ensure handled in UI thread
  PostMessage(Self.Handle,CM_PC_FinishedLoadingDatabase,0,0);
end;

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
  TUserInterface.ShowMessagesForm;
end;

procedure TFRMWallet.miSyncDialogClick(Sender:TObject);
begin
  TUserInterface.ShowSyncDialog;
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

procedure TFRMWallet.paLogoPanelClick(Sender: TObject);
begin

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
  TUserInterface.ShowSyncDialog;
end;

procedure TFRMWallet.sbStatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
begin
  if __FLastFooterToolBarDrawRect = Rect then exit; // avoid FPC bug: triggers infinite draw refresh on windows
  if Panel = sbStatusBar.Panels[3] then
    with tbStatusToolBar do begin
      Top := Rect.Top + (Rect.Height - tbStatusToolBar.Height) div 2;
      Left := Rect.Right - tbStatusToolBar.Width - CT_FOOTER_TOOLBAR_RIGHT_PADDING;
      Visible := true;
    end;
  __FLastFooterToolBarDrawRect := Rect;
end;

{%endregion}

end.

