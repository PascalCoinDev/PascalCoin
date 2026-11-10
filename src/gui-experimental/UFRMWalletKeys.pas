unit UFRMWalletKeys;

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

{ Manages wallet keys, their names and export/import formats. Provides wallet
  backup, restore, password and lock controls. }

{ Public types in this unit

  Type           | Purpose
  ---------------+-------------------------------------------------------------------------
  TFRMWalletKeys | Manages keys, wallet backups, passwords and wallet locking.
}

interface

uses
  {$IFNDEF FPC}Winapi.Windows,{$ENDIF}
  Classes, SysUtils, {$IFDEF FPC}FileUtil,{$ENDIF} Forms, Controls, Graphics, Dialogs, StdCtrls, Messages,
  Buttons, Menus, {$IFDEF FPC}LMessages,{$ENDIF} UCommon.UI, UWallet;

const
  CM_PC_WalletChanged = WM_USER + 1;

type

  { TFRMWalletKeys }

  TFRMWalletKeys = class(TApplicationForm)
    btnChangePassword: TBitBtn;
    btnBackupWallet: TBitBtn;
    btnAddKey: TSpeedButton;
    btnLock: TSpeedButton;
    btnRestoreBackup: TBitBtn;
    ilIcons: TImageList;
    lblEncryptionType: TLabel;
    lblEncryptionTypeCaption: TLabel;
    lblKeyName: TLabel;
    lblKeyNameCaption: TLabel;
    lblKeysEncrypted: TLabel;
    lblPrivateKeyCaption: TLabel;
    lbWalletKeys: TListBox;
    memoPublicKey: TMemo;
    btnCopyClipboard: TSpeedButton;
    miDeleteKey: TMenuItem;
    miExportPublicKey: TMenuItem;
    miExportPrivateKey: TMenuItem;
    miChangeName: TMenuItem;
    miSep1: TMenuItem;
    OpenDialog: TOpenDialog;
    pmKeyMenu: TPopupMenu;
    SaveDialog: TSaveDialog;
    procedure btnAddKeyClick(Sender: TObject);
    procedure btnBackupWalletClick(Sender: TObject);
    procedure btnChangePasswordClick(Sender: TObject);
    procedure btnCopyClipboardClick(Sender: TObject);
    procedure btnDeleteKeyClick(Sender: TObject);
    procedure btnLockClick(Sender: TObject);
    procedure btnRestoreBackupClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbWalletKeysClick(Sender: TObject);
    procedure miChangeNameClick(Sender: TObject);
    procedure miDeleteKeyClick(Sender: TObject);
    procedure miExportPrivateKeyClick(Sender: TObject);
    procedure miExportPublicKeyClick(Sender: TObject);
  private
    { private declarations }
    procedure CM_WalletChanged(var Msg: TMessage); message CM_PC_WalletChanged;
    procedure OnWalletChanged(Sender: TObject);
    procedure RefreshUI;
    function GetSelectedWalletKey(var WalletKey: TWalletKey): Boolean;
    function GetSelectedWalletKeyAndIndex(var WalletKey: TWalletKey; var index: Integer): Boolean;
  protected
    function CheckIsWalletKeyValidPassword : boolean;
    procedure ToggleWalletLock;
    procedure AddNewKey;
    procedure DeleteSelectedKey;
    procedure UpdateSelectedWalletKey;
    procedure ChangeSelectedKeyName;
    procedure ExportSelectedPrivateKey;
    procedure ExportSelectedPublicKey(notify:boolean);
    procedure ChangeWalletPassword;
    procedure BackupWallet;
    procedure RestoreWallet;
  public
    { public declarations }
  end;


implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses {$IFDEF FPC}LCLIntf,{$ENDIF} Clipbrd, UUserInterface, USettings, UCommon, UAccounts, UWIZAddKey, UMemory, UBaseTypes;

{%region Form life-cycle}

procedure TFRMWalletKeys.FormCreate(Sender: TObject);
begin
  // Let the native button draw the original image-list alpha/mask, including
  // its disabled state, instead of flattening the padlock into Glyph.
  btnLock.Images := ilIcons;
  TWallet.Keys.OnChanged.Add(OnWalletChanged);
  RefreshUI;
end;

procedure TFRMWalletKeys.FormDestroy(Sender: TObject);
begin
  TWallet.Keys.OnChanged.Remove(OnWalletChanged);
end;

{%endregion}

{%region Methods}

procedure TFRMWalletKeys.ToggleWalletLock;
begin
  if NOT TWallet.Keys.HasPassword then exit;
  if TWallet.Keys.IsValidPassword then begin
    TWallet.Keys.LockWallet;
  end else begin
    TUserInterface.UnlockWallet(Self);
  end;
end;

procedure TFRMWalletKeys.AddNewKey;
var
  Scoped : TDisposables;
  wiz : TWIZAddKeyWizard;
  model : TWizAddKeyModel;
begin
  wiz := Scoped.AddObject(TWIZAddKeyWizard.Create(nil)) as TWIZAddKeyWizard;
  model := Scoped.AddObject(TWIZAddKeyModel.Create(nil)) as TWizAddKeyModel;
  wiz.Start(model);
end;

procedure TFRMWalletKeys.DeleteSelectedKey;
Var
  wk : TWalletKey;
  index : Integer;
  prompt : String;
begin
  if Not GetSelectedWalletKeyAndIndex(wk,index) then exit;

  if (TSettings.MinerPrivateKeyType = mpk_Selected) AND TBaseType.Equals(TSettings.MinerSelectedPublicKey, TAccountComp.AccountKey2RawString(wk.AccountKey)) then begin
    TUserInterface.ShowError(Self, 'Delete Key Faied',
    'This key is currently being used for mining and cannot be deleted.' + #10 + #10 +
    'Remove this key from the Options dialog and try again.');
    exit;
  end;

  prompt := 'Are you sure you want to delete the selected private key?'+#10+wk.Name+#10+#10+
    'Please note that this will forever remove access to accounts using this key...';
  if TUserInterface.AskQuestion(Self, mtWarning, 'Confirm Delete', prompt, mbYesNo) = mbYes then
    if TUserInterface.AskQuestion(Self, mtWarning, 'Confirm Delete', 'ARE YOU ABSOLUTELY SURE?', mbYesNo) = mbYes then
      TWallet.DeleteKey(wk);
end;

procedure TFRMWalletKeys.UpdateSelectedWalletKey;
Var
  wk : TWalletKey;
  ok : Boolean;
begin
  ok := false;
  wk := CT_TWalletKey_NUL;
  try
    if Not GetSelectedWalletKey(wk) then exit;
    ok := true;
    lblEncryptionType.Caption := TAccountComp.GetECInfoTxt( wk.AccountKey.EC_OpenSSL_NID );
    if wk.Name='' then lblKeyName.Caption := '(No Name)'
    else lblKeyName.Caption := wk.Name;
    memoPublicKey.Lines.Text := TAccountComp.AccountPublicKeyExport(wk.AccountKey);
    memoPublicKey.Font.Color := clBlack;
  finally
    lblEncryptionTypeCaption.Enabled := ok;
    lblEncryptionType.Enabled := ok;
    lblKeyNameCaption.Enabled := ok;
    lblKeyName.Enabled := (ok) And (wk.Name<>'');
    lblPrivateKeyCaption.Enabled := ok;
    memoPublicKey.Enabled := ok;
    btnCopyClipboard.Enabled := ok;
    if not ok then begin
      lblEncryptionType.Caption := '';
      lblKeyName.Caption := '';
      memoPublicKey.Lines.Clear;
    end;
  end;
end;

procedure TFRMWalletKeys.ChangeSelectedKeyName;
Var wk : TWalletKey;
  s : String;
  index : integer;
begin
  if not GetSelectedWalletKeyAndIndex(wk,index) then exit;
  s := wk.Name;
  if TUserInterface.AskEnterString(Self, 'Change name','Input new key name:',s) then begin
    TWallet.Keys.SetName(index,s);
  end;
end;

procedure TFRMWalletKeys.ExportSelectedPrivateKey;
Var wk : TWalletKey;
  pwd1,pwd2, enc : String;
begin
  if not CheckIsWalletKeyValidPassword then exit;
  if Not GetSelectedWalletKey(wk) then exit;
  if Assigned(wk.PrivateKey) then begin
    if TUserInterface.AskEnterProtectedString(Self, 'Export Private Key','Enter an export password (leave empty for an unencrypted key)',pwd1) then begin
      if TUserInterface.AskEnterProtectedString(Self, 'Export Private Key','Re-enter the password',pwd2) then begin
        if pwd1<>pwd2 then raise Exception.Create('Passwords does not match!');
        enc := TWallet.ExportPrivateKey(wk, pwd1);
        Clipboard.AsText := enc;
        if pwd1 = '' then
          TUserInterface.ShowInfo(Self, 'Completed', 'The unencrypted private key has been copied to the clipboard.' +
            #10+#10+'Key value:'+#10+enc)
        else
          TUserInterface.ShowInfo(Self, 'Completed',
         'The private key has been encrypted with your password and copied to the clipboard.'+
          #10+#10+
          'Encrypted key value (Copied to the clipboard):'+#10+
          enc+
          #10+#10+'Length='+Inttostr(length(enc))+' bytes');
      end else raise Exception.Create('Cancelled operation');
    end;
  end;
end;

procedure TFRMWalletKeys.ExportSelectedPublicKey(notify:boolean);
Var wk : TWalletKey;
  s : String;
begin
  if Not GetSelectedWalletKey(wk) then exit;
  s := TWallet.ExportPublicKey(wk);
  Clipboard.AsText := s;
  if notify then
    TUserInterface.ShowInfo(Self, 'Completed',
      'The public key has been copied to the clipboard'+#10+#10+
      'Public key value:'+#10+
      s+#10+#10+
      'Length='+Inttostr(length(s))+' bytes');
end;

procedure TFRMWalletKeys.ChangeWalletPassword;
begin
  if not CheckIsWalletKeyValidPassword
    then exit;
  TUserInterface.ChangeWalletPassword(Self);
end;

procedure TFRMWalletKeys.BackupWallet;
var
  filename : String;
begin
  if TWallet.Keys.Count<=0 then begin
    TUserInterface.ShowError(self, 'Error', 'Your wallet is empty. No keys!');
    exit;
  end;
  if NOT TWallet.Keys.HasPassword then
    if TUserInterface.AskQuestion(self, mtWarning,
      'No Password', 'Your wallet has NO PASSWORD'+#10+#10+
      'It is recommend to protect your wallet with a password prior to exporting it!'+#10+#10+
      'Continue without password protection?', mbYesNo) <> mbYes then exit;
  if Not SaveDialog.Execute
    then exit;
  filename := SaveDialog.FileName;
  if FileExists(filename) then
    if TUserInterface.AskQuestion(self, mtConfirmation, 'Overwrite', 'This file exists:'+#10+filename+#10#10+'Overwrite?', mbYesNo)<> mbYes then
         exit;
  TWallet.BackupWallet(filename);
  TUserInterface.ShowInfo(self, 'Completed', 'Wallet key exported to a file:'+#10+filename);
end;

procedure TFRMWalletKeys.RestoreWallet;
Var
  filename,password : String;
  restoreResult : TRestoreWalletResult;
begin
  if not CheckIsWalletKeyValidPassword then exit;
  if not OpenDialog.Execute then exit;
  filename := OpenDialog.FileName;
  if Not FileExists(filename) then
    raise Exception.Create('Cannot find file '+filename);
  repeat
    if NOT TUserInterface.AskEnterProtectedString(Self, 'Restore Wallet', 'Please enter password used to secure the imported wallet', password) then
      exit;
    restoreResult := TWallet.RestoreWallet(filename, password);
    if (not restoreResult.Success) AND (TUserInterface.AskQuestion(self, mtError, 'Wrong Password', 'Password entered is not valid, retry?', mbYesNo) <> mbYes) then
      exit;
  until restoreResult.Success;
  if restoreResult.Success then begin
    if (restoreResult.ImportedPrivateKeys>0) Or (restoreResult.ImportedPublicKeys>0) then begin
      TUserInterface.ShowInfo(self,
      'Completed',
      'Wallet file imported successfully'+#10+#10+
        'File: '+filename+#10+#10+
        'Total keys in wallet: '+inttostr(restoreResult.TotalKeysFound)+#10+
        'Imported private keys: '+IntToStr(restoreResult.ImportedPrivateKeys)+#10+
        'Imported watch-only keys: '+IntToStr(restoreResult.ImportedPublicKeys)+#10+
        'Duplicates (discarded): '+InttoStr(restoreResult.Duplicates));
    end else begin
      TUserInterface.ShowWarning(Self,
      'No New Keys',
      'Wallet file keys were already in your wallet. Nothing imported'+#10+#10+
        'File: '+filename+#10+#10+
        'Total keys in wallet: '+inttostr(restoreResult.TotalKeysFound));
    end;
  end;
end;

{%endregion}

{%region Aux}

procedure TFRMWalletKeys.RefreshUI;
var
  lasti,i : Integer;
  selected_wk,wk : TWalletKey;
  keyName : String;
begin
  GetSelectedWalletKeyAndIndex(wk,lasti);
  lbWalletKeys.Items.BeginUpdate;
  Try
    lbWalletKeys.Items.Clear;
    lblKeysEncrypted.Caption := '';

    If NOT TWallet.Keys.HasPassword then begin
      // NO PASSWORD
      lblKeysEncrypted.Caption := 'Wallet has no password';
      btnLock.ImageIndex := 1;
      btnLock.Enabled := false;
      btnChangePassword.Enabled := true;
      btnAddKey.Visible := true;
      btnChangePassword.Caption := 'Set Password';
      btnChangePassword.Enabled := true;
      btnRestoreBackup.Enabled := true;
      miChangeName.Enabled := true;
      miExportPrivateKey.Enabled := true;
      miDeleteKey.Enabled := true;
    end else if TWallet.Keys.IsValidPassword then begin
      // UNLOCKED
      lblKeysEncrypted.Caption := 'Wallet is unlocked';
      btnLock.ImageIndex := 1;
      btnLock.Enabled := true;
      btnChangePassword.Enabled := true;
      btnAddKey.Enabled := true;
      btnAddKey.Visible := true;
      btnChangePassword.Caption := 'Change Password';
      btnChangePassword.Enabled := true;
      btnRestoreBackup.Enabled := true;
      miChangeName.Enabled := true;
      miExportPrivateKey.Enabled := true;
      miDeleteKey.Enabled := true;
    end else begin
      // LOCKED
      lblKeysEncrypted.Caption := 'Wallet is locked';
      btnLock.ImageIndex := 0;
      btnLock.Enabled := true;
      btnChangePassword.Enabled := false;
      btnAddKey.Visible := false;
      btnAddKey.Enabled := false;
      btnChangePassword.Caption := 'Change Password';
      btnChangePassword.Enabled := false;
      btnRestoreBackup.Enabled := false;
      miChangeName.Enabled := false;
      miExportPrivateKey.Enabled := false;
      miDeleteKey.Enabled := false;
    end;
    for i := 0 to TWallet.Keys.Count - 1 do begin
      wk := TWallet.Keys.Key[i];
      if (wk.Name='') then begin
        keyName := '(No Name)';
      end else begin
        keyName := wk.Name;
      end;
      if Not wk.HasPrivateKey then
        keyName:=keyName+' (watch-only)';
      lbWalletKeys.Items.AddObject(keyName,TObject(i));
    end;
    i := lbWalletKeys.Items.IndexOfObject(TObject(lasti));
    lbWalletKeys.ItemIndex := i;
  Finally
    lbWalletKeys.Items.EndUpdate;
  End;
  UpdateSelectedWalletKey;
end;

function TFRMWalletKeys.CheckIsWalletKeyValidPassword : boolean;
begin
  if NOT TWallet.Keys.IsValidPassword then begin
    TUserInterface.ShowError(Self, 'Unable to continue', 'Wallet is locked. Please unlock the wallet to perform this task.');
    Result := false;
  end else
    Result := true;
end;

function TFRMWalletKeys.GetSelectedWalletKey(var WalletKey: TWalletKey): Boolean;
Var i : Integer;
begin
  Result := GetSelectedWalletKeyAndIndex(WalletKey,i);
end;

function TFRMWalletKeys.GetSelectedWalletKeyAndIndex(var WalletKey: TWalletKey; var index: Integer): Boolean;
begin
  index := -1;
  Result := false;
  if lbWalletKeys.ItemIndex<0 then exit;
  index := Integer(lbWalletKeys.Items.Objects[ lbWalletKeys.ItemIndex ]);
  if (index>=0) And (index<TWallet.Keys.Count) then begin
    WalletKey := TWallet.Keys.Key[index];
    Result := true;
  end;
end;

{%endregion}

{%region Event Handlers}

procedure TFRMWalletKeys.btnLockClick(Sender: TObject);
begin
  ToggleWalletLock;
end;

procedure TFRMWalletKeys.CM_WalletChanged(var Msg: TMessage);
begin
  RefreshUI;
end;

procedure TFRMWalletKeys.OnWalletChanged(Sender: TObject);
begin
  // Ensure handled in UI thread
  PostMessage(Self.Handle,CM_PC_WalletChanged,0,0);
end;

procedure TFRMWalletKeys.btnAddKeyClick(Sender: TObject);
begin
  AddNewKey;
end;

procedure TFRMWalletKeys.btnBackupWalletClick(Sender: TObject);
begin
 BackupWallet;
end;

procedure TFRMWalletKeys.btnChangePasswordClick(Sender: TObject);
begin
  ChangeWalletPassword;
end;

procedure TFRMWalletKeys.btnCopyClipboardClick(Sender: TObject);
begin
  ExportSelectedPublicKey(false);
end;

procedure TFRMWalletKeys.btnDeleteKeyClick(Sender: TObject);
begin
  DeleteSelectedKey;
end;

procedure TFRMWalletKeys.btnRestoreBackupClick(Sender: TObject);
begin
 RestoreWallet;
end;

procedure TFRMWalletKeys.lbWalletKeysClick(Sender: TObject);
begin
  UpdateSelectedWalletKey;
end;

procedure TFRMWalletKeys.miChangeNameClick(Sender: TObject);
begin
  ChangeSelectedKeyName;
end;

procedure TFRMWalletKeys.miDeleteKeyClick(Sender: TObject);
begin
  DeleteSelectedKey;
end;

procedure TFRMWalletKeys.miExportPrivateKeyClick(Sender: TObject);
begin
  ExportSelectedPrivateKey;
end;

procedure TFRMWalletKeys.miExportPublicKeyClick(Sender: TObject);
begin
  ExportSelectedPublicKey(true);
end;

{%endregion}

end.

