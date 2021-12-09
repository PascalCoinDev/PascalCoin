unit UFRMWalletConfig;

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  If you like it, consider a donation using Bitcoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
  ShellApi,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, UAppParams, USettings, UWallet,
  Vcl.ExtCtrls;

type


  { TFRMPascalCoinWalletConfig }

  TFRMPascalCoinWalletConfig = class(TForm)
    Panel1: TPanel;
    PageControlWalletConfig: TPageControl;
    TabSheetMiner: TTabSheet;
    TabSheetPassword: TTabSheet;
    TabSheetJSON: TTabSheet;
    Label7: TLabel;
    Label6: TLabel;
    lblDefaultJSONRPCMinerServerPort: TLabel;
    Label5: TLabel;
    ebJSONRPCAllowedIPs: TEdit;
    cbJSONRPCPortEnabled: TCheckBox;
    ebJSONRPCMinerServerPort: TEdit;
    udJSONRPCMinerServerPort: TUpDown;
    cbJSONRPCMinerServerActive: TCheckBox;
    TabSheetBlockchain: TTabSheet;
    ebMinFutureBlocksToDownloadNewSafebox: TEdit;
    cbDownloadNewCheckpoint: TCheckBox;
    TabSheetOperations: TTabSheet;
    Label1: TLabel;
    ebDefaultFee: TEdit;
    TabSheetLogs: TTabSheet;
    cbSaveLogFiles: TCheckBox;
    cbShowLogs: TCheckBox;
    cbShowModalMessages: TCheckBox;
    cbSaveDebugLogs: TCheckBox;
    TabSheetLanguage: TTabSheet;
    bbChangeLanguage: TBitBtn;
    TabSheetStorage: TTabSheet;
    bbOpenDataFolder: TBitBtn;
    Label3: TLabel;
    Label4: TLabel;
    lblDefaultInternetServerPort: TLabel;
    Label2: TLabel;
    ebMinerName: TEdit;
    ebInternetServerPort: TEdit;
    udInternetServerPort: TUpDown;
    gbMinerPrivateKey: TGroupBox;
    rbGenerateANewPrivateKeyEachBlock: TRadioButton;
    rbUseARandomKey: TRadioButton;
    rbMineAllwaysWithThisKey: TRadioButton;
    cbPrivateKeyToMine: TComboBox;
    bbUpdatePassword: TBitBtn;
    bbOk: TBitBtn;
    bbCancel: TBitBtn;
    procedure bbChangeLanguageClick(Sender: TObject);
    procedure cbDownloadNewCheckpointClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bbOkClick(Sender: TObject);
    procedure bbUpdatePasswordClick(Sender: TObject);
    procedure cbSaveLogFilesClick(Sender: TObject);
    procedure bbOpenDataFolderClick(Sender: TObject);
    procedure cbJSONRPCPortEnabledClick(Sender: TObject);
  private
    FAppParams: TAppParams;
    FWalletKeys: TWalletKeys;
    FNewUILanguage:String;
    procedure SetAppParams(const Value: TAppParams);
    procedure SetWalletKeys(const Value: TWalletKeys);
    Procedure UpdateWalletConfig;
    { Private declarations }
  public
    { Public declarations }
    Property AppParams : TAppParams read FAppParams write SetAppParams;
    Property WalletKeys : TWalletKeys read FWalletKeys write SetWalletKeys;
  end;

  {$I ../config.inc}

implementation

uses
  {$IFDEF USE_GNUGETTEXT}gnugettext, UFRMSelectLanguage, {$ENDIF}UConst, UAccounts, ULog, UCrypto, UNode, UGUIUtils, UNetProtocol;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TFRMPascalCoinWalletConfig.bbOkClick(Sender: TObject);
Var df : Int64;
  mpk : TMinerPrivateKeyType;
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
    AppParams.ParamByName[CT_PARAM_MinerPrivateKeySelectedPublicKey].SetAsTBytes( TAccountComp.AccountKey2RawString( FWalletKeys.Key[i].AccountKey ) );
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
  if cbDownloadNewCheckpoint.Checked then begin
    i := StrToIntDef(ebMinFutureBlocksToDownloadNewSafebox.Text,0);
    AppParams.ParamByName[CT_PARAM_MinFutureBlocksToDownloadNewSafebox].SetAsInteger(i);
    AppParams.ParamByName[CT_PARAM_AllowDownloadNewCheckpointIfOlderThan].SetAsBoolean(i>200);
  end else AppParams.ParamByName[CT_PARAM_AllowDownloadNewCheckpointIfOlderThan].SetAsBoolean(False);
  AppParams.ParamByName[CT_PARAM_UILanguage].SetAsString(FNewUILanguage);

  ModalResult := MrOk;
end;

procedure TFRMPascalCoinWalletConfig.bbOpenDataFolderClick(Sender: TObject);
begin
  {$IFDEF FPC}
  OpenDocument(pchar(TNode.GetPascalCoinDataFolder))
  {$ELSE}
  shellexecute(0, 'open', pchar(TNode.GetPascalCoinDataFolder), nil, nil, SW_SHOW)
  {$ENDIF}
end;

procedure TFRMPascalCoinWalletConfig.bbUpdatePasswordClick(Sender: TObject);
Var s,s2 : String;
begin
  if Not Assigned(FWalletKeys) then exit;
  if Not FWalletKeys.IsValidPassword then begin
    s := '';
    Repeat
      if Not InputQueryPassword('Wallet Password','Insert Wallet Password',s) then exit;
      FWalletKeys.WalletPassword := s;
      if Not FWalletKeys.IsValidPassword then Application.MessageBox(PChar('Invalid password'),PChar(Application.Title),MB_ICONERROR+MB_OK);
    Until FWalletKeys.IsValidPassword;
  end;
  if FWalletKeys.IsValidPassword then begin
    s := ''; s2 := '';
    if Not InputQueryPassword('Change password','Type new password',s) then exit;
    if trim(s)<>s then raise Exception.Create('Password cannot start or end with a space character');
    if Not InputQueryPassword('Change password','Type new password again',s2) then exit;
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
  {$IFDEF USE_GNUGETTEXT}TranslateComponent(self);{$ENDIF}
  //
  lblDefaultInternetServerPort.Caption := Format('(Default %d)',[CT_NetServer_Port]);
  udInternetServerPort.Position := CT_NetServer_Port;
  ebDefaultFee.Text := TAccountComp.FormatMoney(0);
  ebMinerName.Text := '';
  bbUpdatePassword.Enabled := false;
  UpdateWalletConfig;
  lblDefaultJSONRPCMinerServerPort.Caption := Format('(Default %d)',[CT_JSONRPCMinerServer_Port]);
  {$ifdef fpc}{$ifdef darwin}
  Caption:='Preferences';
  {$endif}{$endif}
end;

procedure TFRMPascalCoinWalletConfig.cbDownloadNewCheckpointClick(
  Sender: TObject);
begin
  UpdateWalletConfig;
end;

procedure TFRMPascalCoinWalletConfig.bbChangeLanguageClick(Sender: TObject);
begin
  {$IFDEF USE_GNUGETTEXT}
   fNewUILanguage := AppParams.ParamByName[CT_PARAM_UILanguage].GetAsString(GetCurrentLanguage);
   fNewUILanguage := SelectUILanguage(fNewUILanguage);
   if fNewUILanguage<>AppParams.ParamByName[CT_PARAM_UILanguage].GetAsString(GetCurrentLanguage) then // new language selected
   begin
     UseLanguage(fNewUILanguage);
     RetranslateComponent(Self);
   end;
  {$ENDIF}
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
    case TMinerPrivateKeyType(AppParams.ParamByName[CT_PARAM_MinerPrivateKeyType].GetAsInteger(Integer(mpk_Random))) of
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
    ebMinFutureBlocksToDownloadNewSafebox.Text := IntToStr(AppParams.ParamByName[CT_PARAM_MinFutureBlocksToDownloadNewSafebox].GetAsInteger(TNetData.NetData.MinFutureBlocksToDownloadNewSafebox));
    cbDownloadNewCheckpoint.Checked:= AppParams.ParamByName[CT_PARAM_AllowDownloadNewCheckpointIfOlderThan].GetAsBoolean(TNetData.NetData.MinFutureBlocksToDownloadNewSafebox>200);
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,ClassName,'Exception at SetAppParams: '+E.Message);
    end;
  End;
  cbSaveLogFilesClick(nil);
  cbJSONRPCPortEnabledClick(nil);
  UpdateWalletConfig;
end;

procedure TFRMPascalCoinWalletConfig.SetWalletKeys(const Value: TWalletKeys);
begin
  FWalletKeys := Value;
  UpdateWalletConfig;
end;


procedure TFRMPascalCoinWalletConfig.UpdateWalletConfig;
Var i, iselected : Integer;
  raw : TBytes;
  wk : TWalletKey;
  auxs : String;
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
        auxs := TCrypto.ToHexaString( TAccountComp.AccountKey2RawString(wk.AccountKey));
      end else begin
        auxs := wk.Name;
      end;
      if (Length(wk.CryptedKey)>0) then begin
        cbPrivateKeyToMine.Items.AddObject(auxs,TObject(i));
      end;
    end;
    cbPrivateKeyToMine.Sorted := true;
    if Assigned(FAppParams) then begin
      raw := FAppParams.ParamByName[CT_PARAM_MinerPrivateKeySelectedPublicKey].GetAsTBytes(Nil);
      iselected := FWalletKeys.IndexOfAccountKey(TAccountComp.RawString2Accountkey(raw));
      if iselected>=0 then begin
        iselected :=  cbPrivateKeyToMine.Items.IndexOfObject(TObject(iselected));
        cbPrivateKeyToMine.ItemIndex := iselected;
      end;

    end;

  end else bbUpdatePassword.Caption := '(Wallet password)';
  bbUpdatePassword.Enabled := Assigned(FWAlletKeys);
  ebMinFutureBlocksToDownloadNewSafebox.Enabled:=cbDownloadNewCheckpoint.Checked;
end;

end.
