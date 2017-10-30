unit UFRMPascalCoinWalletConfig;

{$mode delphi}

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
  LCLIntf, LCLType, LMessages,
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, UAppParams, UWalletKeys;

type

  TMinerPrivateKey = (mpk_NewEachTime, mpk_Random, mpk_Selected);

  { TFRMPascalCoinWalletConfig }

  TFRMPascalCoinWalletConfig = class(TForm)
    cbJSONRPCMinerServerActive: TCheckBox;
    ebDefaultFee: TEdit;
    Label1: TLabel;
    cbSaveLogFiles: TCheckBox;
    cbShowLogs: TCheckBox;
    bbOk: TBitBtn;
    bbCancel: TBitBtn;
    udInternetServerPort: TUpDown;
    ebInternetServerPort: TEdit;
    Label2: TLabel;
    lblDefaultInternetServerPort: TLabel;
    bbUpdatePassword: TBitBtn;
    Label3: TLabel;
    ebMinerName: TEdit;
    Label4: TLabel;
    cbShowModalMessages: TCheckBox;
    Label5: TLabel;
    udJSONRPCMinerServerPort: TUpDown;
    ebJSONRPCMinerServerPort: TEdit;
    lblDefaultJSONRPCMinerServerPort: TLabel;
    gbMinerPrivateKey: TGroupBox;
    rbGenerateANewPrivateKeyEachBlock: TRadioButton;
    rbUseARandomKey: TRadioButton;
    rbMineAllwaysWithThisKey: TRadioButton;
    cbPrivateKeyToMine: TComboBox;
    cbSaveDebugLogs: TCheckBox;
    bbOpenDataFolder: TBitBtn;
    cbJSONRPCPortEnabled: TCheckBox;
    ebJSONRPCAllowedIPs: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure bbOkClick(Sender: TObject);
    procedure bbUpdatePasswordClick(Sender: TObject);
    procedure cbSaveLogFilesClick(Sender: TObject);
    procedure bbOpenDataFolderClick(Sender: TObject);
    procedure cbJSONRPCPortEnabledClick(Sender: TObject);
  private
    FAppParams: TAppParams;
    FWalletKeys: TWalletKeys;
    procedure SetAppParams(const Value: TAppParams);
    procedure SetWalletKeys(const Value: TWalletKeys);
    Procedure UpdateWalletConfig;
    { Private declarations }
  public
    { Public declarations }
    Property AppParams : TAppParams read FAppParams write SetAppParams;
    Property WalletKeys : TWalletKeys read FWalletKeys write SetWalletKeys;
  end;

implementation

uses UConst, UAccounts, ULog, UCrypto, UFolderHelper;

{$R *.lfm}

procedure TFRMPascalCoinWalletConfig.bbOkClick(Sender: TObject);
Var df : Int64;
  mpk : TMinerPrivateKey;
  i : Integer;
begin
  if udInternetServerPort.Position = udJSONRPCMinerServerPort.Position then raise Exception.Create('Server port and JSON-RPC Server miner port are equal!');

  if TAccountComp.TxtToMoney(ebDefaultFee.Text,df) then begin
    AppParams.ParamByName[CT_PARAM_DefaultFee].SetAsInt64(df);
  end else begin
    ebDefaultFee.Text := TAccountComp.FormatMoney(AppParams.ParamByName[CT_PARAM_DefaultFee].GetAsInteger(0));
    raise Exception.Create('Invalid Fee value');
  end;
  AppParams.ParamByName[CT_PARAM_InternetServerPort].SetAsInteger(udInternetServerPort.Position );
  if rbGenerateANewPrivateKeyEachBlock.Checked then mpk := mpk_NewEachTime
  else if rbUseARandomKey.Checked then mpk := mpk_Random
  else if rbMineAllwaysWithThisKey.Checked then begin
    mpk := mpk_Selected;
    if cbPrivateKeyToMine.ItemIndex<0 then raise Exception.Create('Must select a private key');
    i := PtrInt(cbPrivateKeyToMine.Items.Objects[cbPrivateKeyToMine.ItemIndex]);
    if (i<0) Or (i>=FWalletKeys.Count) then raise Exception.Create('Invalid private key');
    AppParams.ParamByName[CT_PARAM_MinerPrivateKeySelectedPublicKey].SetAsString( TAccountComp.AccountKey2RawString( FWalletKeys.Key[i].AccountKey ) );
  end else mpk := mpk_Random;

  AppParams.ParamByName[CT_PARAM_MinerPrivateKeyType].SetAsInteger(integer(mpk));
  AppParams.ParamByName[CT_PARAM_JSONRPCMinerServerActive].SetAsBoolean(cbJSONRPCMinerServerActive.Checked );
  AppParams.ParamByName[CT_PARAM_SaveLogFiles].SetAsBoolean(cbSaveLogFiles.Checked );
  AppParams.ParamByName[CT_PARAM_ShowLogs].SetAsBoolean(cbShowLogs.Checked );
  AppParams.ParamByName[CT_PARAM_SaveDebugLogs].SetAsBoolean(cbSaveDebugLogs.Checked);
  AppParams.ParamByName[CT_PARAM_MinerName].SetAsString(ebMinerName.Text);
  AppParams.ParamByName[CT_PARAM_ShowModalMessages].SetAsBoolean(cbShowModalMessages.Checked);
  AppParams.ParamByName[CT_PARAM_JSONRPCMinerServerPort].SetAsInteger(udJSONRPCMinerServerPort.Position);
  AppParams.ParamByName[CT_PARAM_JSONRPCEnabled].SetAsBoolean(cbJSONRPCPortEnabled.Checked);
  AppParams.ParamByName[CT_PARAM_JSONRPCAllowedIPs].SetAsString(ebJSONRPCAllowedIPs.Text);

  ModalResult := MrOk;
end;

procedure TFRMPascalCoinWalletConfig.bbOpenDataFolderClick(Sender: TObject);
begin
  OpenDocument(pchar(TFolderHelper.GetPascalCoinDataFolder))
end;

procedure TFRMPascalCoinWalletConfig.bbUpdatePasswordClick(Sender: TObject);
Var s,s2 : String;
begin
  if Not Assigned(FWalletKeys) then exit;
  if Not FWalletKeys.IsValidPassword then begin
    s := '';
    Repeat
      if Not InputQuery('Wallet Password','Insert Wallet Password',s) then exit;
      FWalletKeys.WalletPassword := s;
      if Not FWalletKeys.IsValidPassword then Application.MessageBox(PChar('Invalid password'),PChar(Application.Title),MB_ICONERROR+MB_OK);
    Until FWalletKeys.IsValidPassword;
  end;
  if FWalletKeys.IsValidPassword then begin
    s := ''; s2 := '';
    if Not InputQuery('Change password','Type new password',s) then exit;
    if trim(s)<>s then raise Exception.Create('Password cannot start or end with a space character');
    if Not InputQuery('Change password','Type new password again',s2) then exit;
    if s<>s2 then raise Exception.Create('Two passwords are different!');

    FWalletKeys.WalletPassword := s;
    Application.MessageBox(PChar('Password changed!'+#10+#10+
      'Please note that your new password is "'+s+'"'+#10+#10+
      '(If you lose this password, you will lose your Wallet forever !)'),
      PChar(Application.Title),MB_ICONWARNING+MB_OK);
  end;
  UpdateWalletConfig;
end;

procedure TFRMPascalCoinWalletConfig.cbJSONRPCPortEnabledClick(Sender: TObject);
begin
  ebJSONRPCAllowedIPs.Enabled := cbJSONRPCPortEnabled.Checked;
end;

procedure TFRMPascalCoinWalletConfig.cbSaveLogFilesClick(Sender: TObject);
begin
  cbSaveDebugLogs.Enabled := cbSaveLogFiles.Checked;
end;

procedure TFRMPascalCoinWalletConfig.FormCreate(Sender: TObject);
begin
  lblDefaultInternetServerPort.Caption := Format('(Default %d)',[CT_NetServer_Port]);
  udInternetServerPort.Position := CT_NetServer_Port;
  ebDefaultFee.Text := TAccountComp.FormatMoney(0);
  ebMinerName.Text := '';
  bbUpdatePassword.Enabled := false;
  UpdateWalletConfig;
  lblDefaultJSONRPCMinerServerPort.Caption := Format('(Default %d)',[CT_JSONRPCMinerServer_Port]);
end;

procedure TFRMPascalCoinWalletConfig.SetAppParams(const Value: TAppParams);
Var i : Integer;
begin
  FAppParams := Value;
  if Not Assigned(Value) then exit;
  Try
    udInternetServerPort.Position := AppParams.ParamByName[CT_PARAM_InternetServerPort].GetAsInteger(CT_NetServer_Port);
    ebDefaultFee.Text := TAccountComp.FormatMoney(AppParams.ParamByName[CT_PARAM_DefaultFee].GetAsInt64(0));
    cbJSONRPCMinerServerActive.Checked := AppParams.ParamByName[CT_PARAM_JSONRPCMinerServerActive].GetAsBoolean(true);
    case TMinerPrivateKey(AppParams.ParamByName[CT_PARAM_MinerPrivateKeyType].GetAsInteger(Integer(mpk_Random))) of
      mpk_NewEachTime : rbGenerateANewPrivateKeyEachBlock.Checked := true;
      mpk_Random : rbUseARandomKey.Checked := true;
      mpk_Selected : rbMineAllwaysWithThisKey.Checked := true;
    else rbUseARandomKey.Checked := true;
    end;
    UpdateWalletConfig;
    cbSaveLogFiles.Checked := AppParams.ParamByName[CT_PARAM_SaveLogFiles].GetAsBoolean(false);
    cbShowLogs.Checked := AppParams.ParamByName[CT_PARAM_ShowLogs].GetAsBoolean(false);
    cbSaveDebugLogs.Checked := AppParams.ParamByName[CT_PARAM_SaveDebugLogs].GetAsBoolean(false);
    ebMinerName.Text := AppParams.ParamByName[CT_PARAM_MinerName].GetAsString('');
    cbShowModalMessages.Checked := AppParams.ParamByName[CT_PARAM_ShowModalMessages].GetAsBoolean(false);
    udJSONRPCMinerServerPort.Position := AppParams.ParamByName[CT_PARAM_JSONRPCMinerServerPort].GetAsInteger(CT_JSONRPCMinerServer_Port);
    cbJSONRPCPortEnabled.Checked := AppParams.ParamByName[CT_PARAM_JSONRPCEnabled].GetAsBoolean(false);
    ebJSONRPCAllowedIPs.Text := AppParams.ParamByName[CT_PARAM_JSONRPCAllowedIPs].GetAsString('127.0.0.1;');
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,ClassName,'Exception at SetAppParams: '+E.Message);
    end;
  End;
  cbSaveLogFilesClick(nil);
  cbJSONRPCPortEnabledClick(nil);
end;

procedure TFRMPascalCoinWalletConfig.SetWalletKeys(const Value: TWalletKeys);
begin
  FWalletKeys := Value;
  UpdateWalletConfig;
end;


procedure TFRMPascalCoinWalletConfig.UpdateWalletConfig;
Var i, iselected : Integer;
  s : String;
  wk : TWalletKey;
begin
  if Assigned(FWalletKeys) then begin
    if FWalletKeys.IsValidPassword then begin
      if FWalletKeys.WalletPassword='' then begin
        bbUpdatePassword.Caption := 'Wallet without password, protect it!';
      end else begin
        bbUpdatePassword.Caption := 'Change Wallet password';
      end;
    end else begin
        bbUpdatePassword.Caption := 'Wallet with password, change it!';
    end;
    cbPrivateKeyToMine.Items.Clear;
    for i := 0 to FWalletKeys.Count - 1 do begin
      wk := FWalletKeys.Key[i];
      if (wk.Name='') then begin
        s := TCrypto.ToHexaString( TAccountComp.AccountKey2RawString(wk.AccountKey));
      end else begin
        s := wk.Name;
      end;
      if wk.CryptedKey<>'' then begin
        cbPrivateKeyToMine.Items.AddObject(s,TObject(i));
      end;
    end;
    cbPrivateKeyToMine.Sorted := true;
    if Assigned(FAppParams) then begin
      s := FAppParams.ParamByName[CT_PARAM_MinerPrivateKeySelectedPublicKey].GetAsString('');
      iselected := FWalletKeys.IndexOfAccountKey(TAccountComp.RawString2Accountkey(s));
      if iselected>=0 then begin
        iselected :=  cbPrivateKeyToMine.Items.IndexOfObject(TObject(iselected));
        cbPrivateKeyToMine.ItemIndex := iselected;
      end;

    end;

  end else bbUpdatePassword.Caption := '(Wallet password)';
  bbUpdatePassword.Enabled := Assigned(FWAlletKeys);
end;

end.
