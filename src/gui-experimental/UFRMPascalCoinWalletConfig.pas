unit UFRMPascalCoinWalletConfig;

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

{$mode delphi}

interface

uses
  LCLIntf, LCLType, LMessages,
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, UCommon.UI;

const
    CM_PC_WalletKeysChanged = WM_USER + 1;

type

  { TFRMPascalCoinWalletConfig }

  TFRMPascalCoinWalletConfig = class(TApplicationForm)
    cbDownloadNewCheckpoint: TCheckBox;
    cbJSONRPCMinerServerActive: TCheckBox;
    ebDefaultFee: TEdit;
    ebMinFutureBlocksToDownloadNewSafebox: TEdit;
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
    procedure cbDownloadNewCheckpointClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bbOkClick(Sender: TObject);
    procedure bbUpdatePasswordClick(Sender: TObject);
    procedure cbSaveLogFilesClick(Sender: TObject);
    procedure bbOpenDataFolderClick(Sender: TObject);
    procedure cbJSONRPCPortEnabledClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure CM_WalletChanged(var Msg: TMessage); message CM_PC_WalletKeysChanged;
    procedure OnWalletChanged(Sender: TObject);
    procedure RefreshUI;
    procedure RefreshUI_WalletAspect;
  public
  end;

implementation

uses UConst, USettings, UAccounts, ULog, UCrypto, UFolderHelper, UWallet, UUserInterface, UCommon, UNode;

{$R *.lfm}

procedure TFRMPascalCoinWalletConfig.bbOkClick(Sender: TObject);
Var df : Int64;
  mpk : TMinerPrivateKeyType;
  i : Integer;
begin
  if udInternetServerPort.Position = udJSONRPCMinerServerPort.Position then raise Exception.Create('Server port and JSON-RPC Server miner port are equal!');

  if TAccountComp.TxtToMoney(ebDefaultFee.Text,df) then begin
    TSettings.DefaultFee := df;
  end else begin
    ebDefaultFee.Text := TAccountComp.FormatMoney(TSettings.DefaultFee);
    raise Exception.Create('Invalid Fee value');
  end;
  TSettings.InternetServerPort := udInternetServerPort.Position;
  if rbGenerateANewPrivateKeyEachBlock.Checked then mpk := mpk_NewEachTime
  else if rbUseARandomKey.Checked then mpk := mpk_Random
  else if rbMineAllwaysWithThisKey.Checked then begin
    mpk := mpk_Selected;
    if cbPrivateKeyToMine.ItemIndex<0 then raise Exception.Create('Must select a private key');
    i := PtrInt(cbPrivateKeyToMine.Items.Objects[cbPrivateKeyToMine.ItemIndex]);
    if (i<0) Or (i>=TWallet.Keys.Count) then raise Exception.Create('Invalid private key');
    if NOT TWallet.Keys.Key[i].HasPrivateKey then raise Exception.Create('Cannot use key "' + TWallet.Keys.Key[i].Name + '" for mining since it is watch-only');
    TSettings.MinerSelectedPublicKey := TAccountComp.AccountKey2RawString(TWallet.Keys.Key[i].AccountKey);
  end else mpk := mpk_Random;

    if cbDownloadNewCheckpoint.Checked then begin
    i := StrToIntDef(ebMinFutureBlocksToDownloadNewSafebox.Text,0);
    TSettings.MinFutureBlocksToDownloadNewSafebox := i;
    TSettings.AllowDownloadNewCheckpointIfOlderThan := i > 200;
  end else TSettings.AllowDownloadNewCheckpointIfOlderThan := False;

  TSettings.MinerPrivateKeyType := mpk;
  TSettings.JsonRpcMinerServerActive := cbJSONRPCMinerServerActive.Checked;
  TSettings.JsonRpcMinerServerPort := udJSONRPCMinerServerPort.Position;
  TSettings.SaveLogFiles := cbSaveLogFiles.Checked;
  TSettings.ShowLogs := cbShowLogs.Checked;
  TSettings.SaveDebugLogs := cbSaveDebugLogs.Checked;
  TSettings.MinerName := ebMinerName.Text;
  TSettings.ShowModalMessages := cbShowModalMessages.Checked;
  TSettings.JsonRpcPortEnabled := cbJSONRPCPortEnabled.Checked;
  TSettings.JsonRpcAllowedIPs := ebJSONRPCAllowedIPs.Text;
  TSettings.Save;
  ModalResult := MrOk;
end;

procedure TFRMPascalCoinWalletConfig.bbOpenDataFolderClick(Sender: TObject);
begin
  // Skybuck:TFolderHelper.GetPascalCoinDataFolder don't exist no more replaced with TNode.GetPascalCoinDataFolder
  // OpenDocument(PChar(TFolderHelper.GetPascalCoinDataFolder))
  // Two solutions possible:
  // 1: TNode.GetPascalCoinDataFolder
  // 2. TFolderHelper.GetDataFolder(CT_PascalCoin_Data_Folder
  // I will try TNode first if this does not work use solution 2.
  OpenDocument(PChar(TNode.GetPascalCoinDataFolder));
end;

procedure TFRMPascalCoinWalletConfig.bbUpdatePasswordClick(Sender: TObject);
begin
  TUserInterface.ChangeWalletPassword(Self)
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
  lblDefaultJSONRPCMinerServerPort.Caption := Format('(Default %d)',[CT_JSONRPCMinerServer_Port]);
  RefreshUI;
  TWallet.Keys.OnChanged.Add(OnWalletChanged);
end;

procedure TFRMPascalCoinWalletConfig.cbDownloadNewCheckpointClick(
  Sender: TObject);
begin
  RefreshUI_WalletAspect;
end;

procedure TFRMPascalCoinWalletConfig.FormDestroy(Sender: TObject);
begin
  TWallet.Keys.OnChanged.Remove(OnWalletChanged);
end;

procedure TFRMPascalCoinWalletConfig.CM_WalletChanged(var Msg: TMessage);
begin
  RefreshUI_WalletAspect;
end;

procedure TFRMPascalCoinWalletConfig.OnWalletChanged(Sender: TObject);
begin
  // Ensure handled in UI thread
  PostMessage(Self.Handle,CM_PC_WalletKeysChanged,0,0);
end;

procedure TFRMPascalCoinWalletConfig.RefreshUI;
Var i : Integer;
begin
  Try
    udInternetServerPort.Position := TSettings.InternetServerPort;
    ebDefaultFee.Text := TAccountComp.FormatMoney(TSettings.DefaultFee);
    cbJSONRPCMinerServerActive.Checked := TSettings.JsonRpcMinerServerActive;
    case TSettings.MinerPrivateKeyType of
      mpk_NewEachTime : rbGenerateANewPrivateKeyEachBlock.Checked := true;
      mpk_Random : rbUseARandomKey.Checked := true;
      mpk_Selected : rbMineAllwaysWithThisKey.Checked := true;
    else rbUseARandomKey.Checked := true;
    end;
    RefreshUI_WalletAspect;
    cbSaveLogFiles.Checked := TSettings.SaveLogFiles;
    cbShowLogs.Checked := TSettings.ShowLogs;
    cbSaveDebugLogs.Checked := TSettings.SaveDebugLogs;
    ebMinerName.Text := TSettings.MinerName;
    cbShowModalMessages.Checked := TSettings.ShowModalMessages;
    udJSONRPCMinerServerPort.Position := TSettings.JsonRpcMinerServerPort;
    cbJSONRPCPortEnabled.Checked := TSettings.JsonRpcPortEnabled;
    ebJSONRPCAllowedIPs.Text := TSettings.JsonRpcAllowedIPs;
    cbDownloadNewCheckpoint.Checked := TSettings.AllowDownloadNewCheckpointIfOlderThan;
    ebMinFutureBlocksToDownloadNewSafebox.Text := IntToStr(TSettings.MinFutureBlocksToDownloadNewSafebox);
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,ClassName,'Exception at SetAppParams: '+E.Message);
    end;
  End;
  cbSaveLogFilesClick(nil);
  cbJSONRPCPortEnabledClick(nil);
end;

procedure TFRMPascalCoinWalletConfig.RefreshUI_WalletAspect;
Var i, iselected : Integer;
  auxs : String;
  wk : TWalletKey;
  raw: TBytes;
begin
  if TWallet.Keys.HasPassword then
    bbUpdatePassword.Caption := 'Change Wallet password'
  else
    bbUpdatePassword.Caption := 'Wallet without password, protect it!';
  cbPrivateKeyToMine.Items.Clear;
  for i := 0 to TWallet.Keys.Count - 1 do begin
    wk := TWallet.Keys.Key[i];
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
  raw := TSettings.MinerSelectedPublicKey;
  iselected := TWallet.Keys.IndexOfAccountKey(TAccountComp.RawString2Accountkey(raw));
  if iselected >= 0 then begin
    iselected :=  cbPrivateKeyToMine.Items.IndexOfObject(TObject(iselected));
    cbPrivateKeyToMine.ItemIndex := iselected;
  end;
  bbUpdatePassword.Enabled := Assigned(TWallet.Keys);
  ebMinFutureBlocksToDownloadNewSafebox.Enabled:= cbDownloadNewCheckpoint.Checked;
end;

end.
