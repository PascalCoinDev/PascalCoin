unit UFRMWalletKeys;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, UWalletKeys, Buttons,
  {$IFDEF FPC}LMessages,{$ENDIF}
  clipbrd, UConst, UCommon;

Const
  CM_PC_WalletKeysChanged = {$IFDEF FPC}LM_USER{$ELSE}WM_USER{$ENDIF} + 1;

type
  TFRMWalletKeys = class(TForm)
    lbWalletKeys: TListBox;
    bbExportPrivateKey: TBitBtn;
    lblEncryptionTypeCaption: TLabel;
    lblEncryptionType: TLabel;
    lblKeyNameCaption: TLabel;
    lblKeyName: TLabel;
    lblPrivateKeyCaption: TLabel;
    memoPrivateKey: TMemo;
    bbChangeName: TBitBtn;
    bbImportPrivateKey: TBitBtn;
    bbExportPublicKey: TBitBtn;
    bbImportPublicKey: TBitBtn;
    bbGenerateNewKey: TBitBtn;
    lblPrivateKeyCaption2: TLabel;
    bbDelete: TBitBtn;
    lblKeysEncrypted: TLabel;
    bbUpdatePassword: TBitBtn;
    bbExportAllWalletKeys: TBitBtn;
    SaveDialog: TSaveDialog;
    bbImportKeysFile: TBitBtn;
    OpenDialog: TOpenDialog;
    bbLock: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure bbChangeNameClick(Sender: TObject);
    procedure bbExportPrivateKeyClick(Sender: TObject);
    procedure bbImportPrivateKeyClick(Sender: TObject);
    procedure lbWalletKeysClick(Sender: TObject);
    procedure bbGenerateNewKeyClick(Sender: TObject);
    procedure bbExportPublicKeyClick(Sender: TObject);
    procedure bbDeleteClick(Sender: TObject);
    procedure bbImportPublicKeyClick(Sender: TObject);
    procedure bbUpdatePasswordClick(Sender: TObject);
    procedure bbExportAllWalletKeysClick(Sender: TObject);
    procedure bbImportKeysFileClick(Sender: TObject);
    procedure bbLockClick(Sender: TObject);
  private
    //FOldOnChanged : TNotifyEvent;
    FWalletKeys: TWalletKeys;
    procedure SetWalletKeys(const Value: TWalletKeys);
    procedure OnWalletKeysChanged(Sender : TObject);
    { Private declarations }
    Procedure UpdateWalletKeys;
    Procedure UpdateSelectedWalletKey;
    Function GetSelectedWalletKey(var WalletKey : TWalletKey) : Boolean;
    Function GetSelectedWalletKeyAndIndex(var WalletKey : TWalletKey; var index : Integer) : Boolean;
    Procedure CheckIsWalletKeyValidPassword;
    procedure CM_WalletChanged(var Msg: TMessage); message CM_PC_WalletKeysChanged;
  public
    { Public declarations }
    Property WalletKeys : TWalletKeys read FWalletKeys write SetWalletKeys;
    Destructor Destroy; override;
  end;

implementation

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType,
{$ENDIF}
  UCrypto, UAccounts, UFRMNewPrivateKeyType, UAES;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

{ TFRMWalletKeys }

procedure TFRMWalletKeys.bbChangeNameClick(Sender: TObject);
Var wk : TWalletKey;
  s : String;
  index : integer;
begin
  if not GetSelectedWalletKeyAndIndex(wk,index) then exit;
  s := wk.Name;
  if InputQuery('Change name','Input new key name:',s) then begin
    WalletKeys.SetName(index,s);
    UpdateWalletKeys;
  end;
end;

procedure TFRMWalletKeys.bbDeleteClick(Sender: TObject);
Var wk : TWalletKey;
  index : Integer;
  s : String;
begin
  if Not GetSelectedWalletKeyAndIndex(wk,index) then exit;
  s := 'Are you sure you want to delete the selected private key?'+#10+wk.Name+#10+#10+
    'Please note that this will forever remove access to accounts using this key...';
  if Application.MessageBox(Pchar(s),PChar('Delete key'),
    MB_YESNO+MB_DEFBUTTON2+MB_ICONWARNING)<>Idyes then exit;
  if Application.MessageBox(PChar('Are you sure you want to delete?'),PChar('Delete key'),
    MB_YESNO+MB_DEFBUTTON2+MB_ICONWARNING)<>Idyes then exit;
  WalletKeys.Delete(index);
  UpdateWalletKeys;
end;

procedure TFRMWalletKeys.bbExportAllWalletKeysClick(Sender: TObject);
Var efn : String;
  fs : TFileStream;
begin
  if WalletKeys.Count<=0 then raise Exception.Create('Your wallet is empty. No keys!');
  if ((WalletKeys.IsValidPassword) And (WalletKeys.WalletPassword='')) then begin
    if Application.MessageBox(PChar('Your wallet has NO PASSWORD'+#10+#10+
      'It is recommend to protect your wallet with a password prior to exporting it!'+#10+#10+
      'Continue without password protection?'),PChar(Application.Title),MB_YESNO+MB_ICONWARNING+MB_DEFBUTTON2)<>IdYes then exit;
  end;

  if Not SaveDialog.Execute then exit;
  efn := SaveDialog.FileName;
  if FileExists(efn) then begin
    if Application.MessageBox(PChar('This file exists:'+#10+efn+#10#10+'Overwrite?'),PChar(Application.Title),MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2)<>IdYes then exit;
  end;
  fs := TFileStream.Create(efn,fmCreate);
  try
    fs.Size := 0;
    WalletKeys.SaveToStream(fs);
  finally
    fs.Free;
  end;
  Application.MessageBox(PChar('Wallet key exported to a file:'+#10+efn),PChar(Application.Title),MB_OK+MB_ICONINFORMATION);
end;

procedure TFRMWalletKeys.bbExportPrivateKeyClick(Sender: TObject);
Var wk : TWalletKey;
  pwd1,pwd2, enc : String;
begin
  CheckIsWalletKeyValidPassword;
  if Not GetSelectedWalletKey(wk) then exit;
  if Assigned(wk.PrivateKey) then begin
    if InputQuery('Export private key','Insert a password to export',pwd1) then begin
      if InputQuery('Export private key','Repeat the password to export',pwd2) then begin
        if pwd1<>pwd2 then raise Exception.Create('Passwords does not match!');
        enc := TCrypto.ToHexaString( TAESComp.EVP_Encrypt_AES256( wk.PrivateKey.ExportToRaw,pwd1) );
        Clipboard.AsText := enc;
        Application.MessageBox(PChar('The password has been encrypted with your password and copied to the clipboard.'+
          #10+#10+
          'Password: "'+pwd1+'"'+#10+
          #10+
          'Encrypted key value (Copied to the clipboard):'+#10+
          enc+
          #10+#10+'Length='+Inttostr(length(enc))+' bytes'),
          PChar(Application.Title),MB_OK+MB_ICONINFORMATION);
      end else raise Exception.Create('Cancelled operation');
    end;
  end;
end;

procedure TFRMWalletKeys.bbExportPublicKeyClick(Sender: TObject);
Var wk : TWalletKey;
  s : String;
begin
  CheckIsWalletKeyValidPassword;
  if Not GetSelectedWalletKey(wk) then exit;
  s := TAccountComp.AccountPublicKeyExport(wk.AccountKey);
  Clipboard.AsText := s;
  Application.MessageBox(PChar('The public key has been copied to the clipboard'+#10+#10+
    'Public key value:'+#10+
    s+#10+#10+
    'Length='+Inttostr(length(s))+' bytes'),
          PChar(Application.Title),MB_OK+MB_ICONINFORMATION);
end;

procedure TFRMWalletKeys.bbGenerateNewKeyClick(Sender: TObject);
Var FRM : TFRMNewPrivateKeyType;
begin
  CheckIsWalletKeyValidPassword;
  FRM := TFRMNewPrivateKeyType.Create(Self);
  try
    FRM.WalletKeys := WalletKeys;
    FRM.ShowModal;
    UpdateWalletKeys;
  finally
    FRM.Free;
  end;
end;

procedure TFRMWalletKeys.bbImportKeysFileClick(Sender: TObject);
Var wki : TWalletKeys;
  ifn,pwd : String;
  i : Integer;
  cPrivatekeys, cPublicKeys : Integer;
begin
  if Not OpenDialog.Execute then exit;
  ifn := OpenDialog.FileName;
  if Not FileExists(ifn) then raise Exception.Create('Cannot find file '+ifn);

  wki := TWalletKeys.Create(Self);
  try
    wki.WalletFileName := ifn;
    if wki.Count<=0 then raise Exception.Create('Wallet file has no valid data');
    pwd := '';
    While (Not wki.IsValidPassword) do begin
      if Not InputQuery('Import','Enter the wallet file password:',pwd) then exit;
      wki.WalletPassword := pwd;
      if not wki.IsValidPassword then begin
        If Application.MessageBox(PChar('Password entered is not valid, retry?'),PChar(Application.Title),MB_ICONERROR+MB_YESNO)<>Idyes then exit;
      end;
    end;
    cPrivatekeys := 0;
    cPublicKeys := 0;
    if wki.IsValidPassword then begin
      for i := 0 to wki.Count - 1 do begin
        if WalletKeys.IndexOfAccountKey(wki.Key[i].AccountKey)<0 then begin
          If Assigned(wki.Key[i].PrivateKey) then begin
            WalletKeys.AddPrivateKey(wki.Key[i].Name,wki.Key[i].PrivateKey);
            inc(cPrivatekeys);
          end else begin
            WalletKeys.AddPublicKey(wki.Key[i].Name,wki.Key[i].AccountKey);
            inc(cPublicKeys);
          end;
        end;
      end;
    end;
    if (cPrivatekeys>0) Or (cPublicKeys>0) then begin
      Application.MessageBox(PChar('Wallet file imported successfully'+#10+#10+
        'File:'+ifn+#10+#10+
        'Total keys in wallet: '+inttostr(wki.Count)+#10+
        'Imported private keys: '+IntToStr(cPrivatekeys)+#10+
        'Imported public keys only: '+IntToStr(cPublicKeys)+#10+
        'Duplicated (not imported): '+InttoStr(wki.Count - cPrivatekeys - cPublicKeys)),
        PChar(Application.Title),MB_OK+MB_ICONINFORMATION);
    end else begin
      Application.MessageBox(PChar('Wallet file keys were already in your wallet. Nothing imported'+#10+#10+
        'File:'+ifn+#10+#10+
        'Total keys in wallet: '+inttostr(wki.Count)),
        PChar(Application.Title),MB_OK+MB_ICONWARNING);
    end;
  finally
    wki.Free;
  end;
end;

procedure TFRMWalletKeys.bbImportPrivateKeyClick(Sender: TObject);
var s : String;
 desenc, enc : AnsiString;
 EC : TECPrivateKey;
 i : Integer;
 wk : TWalletKey;
 parseResult : Boolean;

  function ParseRawKey(EC_OpenSSL_NID : Word) : boolean;
  begin
    FreeAndNil(EC); ParseRawKey := False;
    EC := TECPrivateKey.Create;
    Try
      EC.SetPrivateKeyFromHexa(EC_OpenSSL_NID, TCrypto.ToHexaString(enc));
      ParseRawKey := True;
    Except
      On E:Exception do begin
        FreeAndNil(EC);
        Raise;
      end;
    end;
  end;

  function ParseEncryptedKey : boolean;
  begin
      Repeat
        s := '';
        desenc := '';
        if InputQuery('Import private key','Enter the password:',s) then begin
          If (TAESComp.EVP_Decrypt_AES256(enc,s,desenc)) then begin
            if (desenc<>'') then begin
              EC := TECPrivateKey.ImportFromRaw(desenc);
              ParseEncryptedKey := True;
              Exit;
            end else begin
              if Application.MessageBox(PChar('Invalid password!'),'',MB_RETRYCANCEL+MB_ICONERROR)<>IDRETRY then begin
                ParseEncryptedKey := False;
                Exit;
              end;
              desenc := '';
            end;
          end else begin
            if Application.MessageBox(PChar('Invalid password or corrupted data!'),'',MB_RETRYCANCEL+MB_ICONERROR)<>IDRETRY then begin
              ParseEncryptedKey := False;
              Exit;
            end;
          end;
        end else begin
          ParseEncryptedKey := false;
          Exit;
        end;
      Until (desenc<>'');
  end;

begin
  EC := Nil;
  CheckIsWalletKeyValidPassword;
  if Not Assigned(WalletKeys) then exit;
  if InputQuery('Import private key','Insert the password protected private key or raw private key',s) then begin
    s := trim(s);
    if (s='') then raise Exception.Create('No valid key');
    enc := TCrypto.HexaToRaw(s);
    if (enc='') then raise Exception.Create('Invalid text... You must enter an hexadecimal value ("0".."9" or "A".."F")');
    case Length(enc) of
         32: parseResult := ParseRawKey(CT_NID_secp256k1);
         35,36: parseResult := ParseRawKey(CT_NID_sect283k1);
         48: parseResult := ParseRawKey(CT_NID_secp384r1);
         65,66: parseResult := ParseRawKey(CT_NID_secp521r1);
         64, 80, 96: parseResult := ParseEncryptedKey;
         else Exception.Create('Invalidly formatted private key string. Ensure it is an encrypted private key export or raw private key hexstring.');
    end;
    if (parseResult = False) then
       exit;
    Try
      // EC is assigned by ParseRawKey/ImportEncryptedKey
      if Not Assigned(EC) then begin
        Application.MessageBox(PChar('Invalid password and/or corrupted data!'),'', MB_OK);
        exit;
      end;
      i := WalletKeys.IndexOfAccountKey(EC.PublicKey);
      if (i>=0) then begin
        wk := WalletKeys.Key[i];
        if Assigned(wk.PrivateKey) And (Assigned(wk.PrivateKey.PrivateKey)) then raise Exception.Create('This key is already in your wallet!');
      end;
      s := 'Imported '+DateTimeToStr(now);
      s := InputBox('Set a name','Name for this private key:',s);
      i := WalletKeys.AddPrivateKey(s,EC);
      Application.MessageBox(PChar('Imported private key'),PChar(Application.Title),MB_OK+MB_ICONINFORMATION);
    Finally
      EC.Free;
    End;
    UpdateWalletKeys;
  end;
end;

procedure TFRMWalletKeys.bbImportPublicKeyClick(Sender: TObject);
var s : String;
 raw : AnsiString;
 EC : TECPrivateKey;
 account : TAccountKey;
 errors : AnsiString;
begin
  CheckIsWalletKeyValidPassword;
  if Not Assigned(WalletKeys) then exit;
  if Not InputQuery('Import publick key','Insert the public key in hexa format or imported format',s) then exit;
  If not TAccountComp.AccountPublicKeyImport(s,account,errors) then begin
    raw := TCrypto.HexaToRaw(s);
    if trim(raw)='' then raise Exception.Create('Invalid public key value (Not hexa or not an imported format)'+#10+errors);
    account := TAccountComp.RawString2Accountkey(raw);
  end;
  If not TAccountComp.IsValidAccountKey(account,errors) then raise Exception.Create('This data is not a valid public key'+#10+errors);
  if WalletKeys.IndexOfAccountKey(account)>=0 then raise exception.Create('This key exists on your wallet');
  s := 'Imported public key '+DateTimeToStr(now);
  if InputQuery('Set a name','Name for this private key:',s) then begin
    WalletKeys.AddPublicKey(s,account);
    UpdateWalletKeys;
    Application.MessageBox(PChar('Imported public key'),PChar(Application.Title),MB_OK+MB_ICONINFORMATION);
  end;
end;

procedure TFRMWalletKeys.bbLockClick(Sender: TObject);
begin
  FWalletKeys.LockWallet;
end;

procedure TFRMWalletKeys.bbUpdatePasswordClick(Sender: TObject);
Var s,s2 : String;
begin
  if FWalletKeys.IsValidPassword then begin
    s := ''; s2 := '';
    if Not InputQuery('Change password','Enter new password',s) then exit;
    if trim(s)<>s then raise Exception.Create('Password cannot start or end with a space character');
    if Not InputQuery('Change password','Enter new password again',s2) then exit;
    if s<>s2 then raise Exception.Create('Two passwords are different!');

    FWalletKeys.WalletPassword := s;
    Application.MessageBox(PChar('Password changed!'+#10+#10+
      'Please note that your new password is "'+s+'"'+#10+#10+
      '(If you lose this password, you will lose your wallet forever!)'),
      PChar(Application.Title),MB_ICONWARNING+MB_OK);
    UpdateWalletKeys;
  end else begin
    s := '';
    Repeat
      if Not InputQuery('Wallet password','Enter wallet password',s) then exit;
      FWalletKeys.WalletPassword := s;
      if Not FWalletKeys.IsValidPassword then Application.MessageBox(PChar('Invalid password'),PChar(Application.Title),MB_ICONERROR+MB_OK);
    Until FWalletKeys.IsValidPassword;
    UpdateWalletKeys;
  end;
end;

procedure TFRMWalletKeys.CheckIsWalletKeyValidPassword;
begin
  if Not Assigned(FWalletKeys) then exit;

  if Not FWalletKeys.IsValidPassword then begin
    Application.MessageBox(PChar('Wallet key is encrypted!'+#10+#10+'You must enter password to continue...'),
      PCHar(Application.Title),MB_OK+MB_ICONWARNING);
    bbUpdatePasswordClick(Nil);
    if Not FWalletKeys.IsValidPassword then raise Exception.Create('Cannot continue without valid password');
  end;
end;

procedure TFRMWalletKeys.CM_WalletChanged(var Msg: TMessage);
begin
  UpdateWalletKeys;
end;

destructor TFRMWalletKeys.Destroy;
begin
  if Assigned(FWalletKeys) then FWalletKeys.OnChanged.Remove(OnWalletKeysChanged);
  inherited;
end;

procedure TFRMWalletKeys.FormCreate(Sender: TObject);
begin
  lbWalletKeys.Sorted := true;
  FWalletKeys := Nil;
  UpdateWalletKeys;
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
  if Not Assigned(WalletKeys) then exit;
  if lbWalletKeys.ItemIndex<0 then exit;
  index := Integer(lbWalletKeys.Items.Objects[ lbWalletKeys.ItemIndex ]);
  if (index>=0) And (index<WalletKeys.Count) then begin
    WalletKey := WalletKeys.Key[index];
    Result := true;
  end;
end;

procedure TFRMWalletKeys.lbWalletKeysClick(Sender: TObject);
begin
  UpdateSelectedWalletKey;
end;

procedure TFRMWalletKeys.OnWalletKeysChanged(Sender : TObject);
begin
  PostMessage(Self.Handle,CM_PC_WalletKeysChanged,0,0);
  //if Assigned(FOldOnChanged) then FOldOnChanged(Sender);   XXXXX HS 2017-09-07: is this procedure needed anymore?
end;

procedure TFRMWalletKeys.SetWalletKeys(const Value: TWalletKeys);
begin
  if FWalletKeys=Value then exit;
  if Assigned(FWalletKeys) then FWalletKeys.OnChanged.Remove(OnWalletKeysChanged);
  FWalletKeys := Value;
  if Assigned(FWalletKeys) then begin
    FWalletKeys.OnChanged.Add(OnWalletKeysChanged);
  end;
  UpdateWalletKeys;
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
    if wk.Name='' then lblKeyName.Caption := '(No name)'
    else lblKeyName.Caption := wk.Name;
    if Assigned(wk.PrivateKey) then begin
      memoPrivateKey.Lines.Text :=  TCrypto.PrivateKey2Hexa(wk.PrivateKey.PrivateKey);
      memoPrivateKey.Font.Color := clBlack;
    end else begin
      memoPrivateKey.Lines.Text := '(No private key)';
      memoPrivateKey.Font.Color := clRed;
    end;
  finally
    lblEncryptionTypeCaption.Enabled := ok;
    lblEncryptionType.Enabled := ok;
    lblKeyNameCaption.Enabled := ok;
    lblKeyName.Enabled := (ok) And (wk.Name<>'');
    lblPrivateKeyCaption.Enabled := ok;
    memoPrivateKey.Enabled := ok;
    bbExportPrivateKey.Enabled := ok;
    bbExportPublicKey.Enabled := ok;
    bbChangeName.Enabled := ok;
    lblPrivateKeyCaption2.Enabled := ok;
    bbDelete.Enabled := ok;
    if not ok then begin
      lblEncryptionType.Caption := '';
      lblKeyName.Caption := '';
      memoPrivateKey.Lines.Clear;
    end;
  end;

end;

procedure TFRMWalletKeys.UpdateWalletKeys;
Var lasti,i : Integer;
  selected_wk,wk : TWalletKey;
  s : AnsiString;
begin
  GetSelectedWalletKeyAndIndex(wk,lasti);
  lbWalletKeys.Items.BeginUpdate;
  Try
    lbWalletKeys.Items.Clear;
    lblKeysEncrypted.Caption := '';
    if not assigned(FWalletKeys) then exit;
    bbLock.Enabled := (FWalletKeys.IsValidPassword) And (FWalletKeys.WalletPassword<>'');
    If FWalletKeys.IsValidPassword then begin
      if FWalletKeys.WalletPassword='' then lblKeysEncrypted.Caption := 'Wallet without password'
      else lblKeysEncrypted.Caption := 'Wallet is password protected';
      lblKeysEncrypted.font.Color := clGreen;
      bbUpdatePassword.Caption := 'Change password';
    end else begin
      lblKeysEncrypted.Caption := 'Wallet is encrypted... need password!';
      lblKeysEncrypted.font.Color := clRed;
      bbUpdatePassword.Caption := 'Input password';
    end;
    for i := 0 to WalletKeys.Count - 1 do begin
      wk := WalletKeys.Key[i];
      if (wk.Name='') then begin
        s := 'Sha256='+TCrypto.ToHexaString( TCrypto.DoSha256( TAccountComp.AccountKey2RawString(wk.AccountKey) ) );
      end else begin
        s := wk.Name;
      end;
      if Not Assigned(wk.PrivateKey) then begin
        if wk.CryptedKey<>'' then s:=s+' (Encrypted, need password)';
        s:=s+' (* without key)';
      end;
      lbWalletKeys.Items.AddObject(s,TObject(i));
    end;
    i := lbWalletKeys.Items.IndexOfObject(TObject(lasti));
    lbWalletKeys.ItemIndex := i;
  Finally
    lbWalletKeys.Items.EndUpdate;
  End;
  UpdateSelectedWalletKey;
end;

end.
