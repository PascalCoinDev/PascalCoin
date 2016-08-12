unit UFRMPascalCoinWalletConfig;

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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, UAppParams, UWalletKeys;

type
  TFRMPascalCoinWalletConfig = class(TForm)
    cbAutomaticMiningWhenConnectedToNodes: TCheckBox;
    cbGenerateANewPrivateKeyForEachNewGeneratedBlock: TCheckBox;
    udDefaultFee: TUpDown;
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
    procedure FormCreate(Sender: TObject);
    procedure bbOkClick(Sender: TObject);
    procedure udDefaultFeeChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: SmallInt; Direction: TUpDownDirection);
    procedure bbUpdatePasswordClick(Sender: TObject);
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

uses UConst, UAccounts, UFRMWallet;

{$R *.dfm}

procedure TFRMPascalCoinWalletConfig.bbOkClick(Sender: TObject);
begin
  AppParams.ParamByName[CT_PARAM_DefaultFee].SetAsInt64(udDefaultFee.Position );
  AppParams.ParamByName[CT_PARAM_InternetServerPort].SetAsInteger(udInternetServerPort.Position );
  AppParams.ParamByName[CT_PARAM_NewPrivateKeyForEachGeneratedBlock].SetAsBoolean(cbGenerateANewPrivateKeyForEachNewGeneratedBlock.Checked );
  AppParams.ParamByName[CT_PARAM_AutomaticMineWhenConnectedToNodes].SetAsBoolean(cbAutomaticMiningWhenConnectedToNodes.Checked );
  AppParams.ParamByName[CT_PARAM_SaveLogFiles].SetAsBoolean(cbSaveLogFiles.Checked );
  AppParams.ParamByName[CT_PARAM_ShowLogs].SetAsBoolean(cbShowLogs.Checked );
  AppParams.ParamByName[CT_PARAM_MinerName].SetAsString(ebMinerName.Text);
  AppParams.ParamByName[CT_PARAM_ShowModalMessages].SetAsBoolean(cbShowModalMessages.Checked);
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

procedure TFRMPascalCoinWalletConfig.FormCreate(Sender: TObject);
begin
  lblDefaultInternetServerPort.Caption := Format('(Default %d)',[CT_NetServer_Port]);
  udInternetServerPort.Position := CT_NetServer_Port;
  udDefaultFee.Position := 0;
  ebDefaultFee.Text := TAccountComp.FormatMoney(0);
  ebMinerName.Text := '';
  bbUpdatePassword.Enabled := false;
  UpdateWalletConfig;
end;

procedure TFRMPascalCoinWalletConfig.SetAppParams(const Value: TAppParams);
begin
  FAppParams := Value;
  if Not Assigned(Value) then exit;
  udInternetServerPort.Position := AppParams.ParamByName[CT_PARAM_InternetServerPort].GetAsInteger(CT_NetServer_Port);
  udDefaultFee.Position := AppParams.ParamByName[CT_PARAM_DefaultFee].GetAsInt64(0);
  ebDefaultFee.Text := TAccountComp.FormatMoney(udDefaultFee.Position);
  cbAutomaticMiningWhenConnectedToNodes.Checked := AppParams.ParamByName[CT_PARAM_AutomaticMineWhenConnectedToNodes].GetAsBoolean(true);
  cbGenerateANewPrivateKeyForEachNewGeneratedBlock.Checked := AppParams.ParamByName[CT_PARAM_NewPrivateKeyForEachGeneratedBlock].GetAsBoolean(false);
  cbSaveLogFiles.Checked := AppParams.ParamByName[CT_PARAM_SaveLogFiles].GetAsBoolean(false);
  cbShowLogs.Checked := AppParams.ParamByName[CT_PARAM_ShowLogs].GetAsBoolean(false);
  ebMinerName.Text := AppParams.ParamByName[CT_PARAM_MinerName].GetAsString('');
  cbShowModalMessages.Checked := AppParams.ParamByName[CT_PARAM_ShowModalMessages].GetAsBoolean(false);
end;

procedure TFRMPascalCoinWalletConfig.SetWalletKeys(const Value: TWalletKeys);
begin
  FWalletKeys := Value;
  UpdateWalletConfig;
end;

procedure TFRMPascalCoinWalletConfig.udDefaultFeeChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
begin
  AllowChange := true;
  ebDefaultFee.Text := TAccountComp.FormatMoney(NewValue);
end;

procedure TFRMPascalCoinWalletConfig.UpdateWalletConfig;
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
  end else bbUpdatePassword.Caption := '(Wallet password)';
  bbUpdatePassword.Enabled := Assigned(FWAlletKeys);
end;

end.
