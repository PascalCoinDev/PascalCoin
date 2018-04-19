unit UWIZChangeKey_SelectKey;

{$mode delphi}
{$modeswitch nestedprocvars}

{ Copyright (c) 2018 Sphere 10 Software (http://www.sphere10.com/)

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
  Ugochukwu Mmaduekwe - main developer
  Herman Schoenfeld - designer
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, UCommon, UCommon.Collections, UWallet,
  UFRMAccountSelect, UNode, UWizard, UWIZOperationFee_Custom, UWIZOperationSigner_Select, UWIZOperationPayload_Encryption,
  UWIZModels;

type

  { TWIZChangeKey_SelectKey }

  TWIZChangeKey_SelectKey = class(TWizardForm<TWIZOperationsModel>)
    cbNewPrivateKey: TComboBox;
    chkAttachPayload: TCheckBox;
    chkChooseFee: TCheckBox;
    gbNewPrivateKey: TGroupBox;
    lblKeyName: TLabel;
    lblPrivateKeyNote: TLabel;
    procedure cbNewPrivateKeyChange(Sender: TObject);
  private
    procedure UpdateWalletKeys();
  public
    procedure OnPresent; override;
    procedure OnNext; override;
    function Validate(out message: ansistring): boolean; override;
  end;


implementation

{$R *.lfm}

uses
  UAccounts, UCrypto, UUserInterface, USettings;

{ TWIZChangeKey_SelectKey }

procedure TWIZChangeKey_SelectKey.cbNewPrivateKeyChange(Sender: TObject);
var
  i: integer;
  wk: TWalletKey;
begin
  if cbNewPrivateKey.ItemIndex < 1 then
  begin
    lblKeyName.Font.Color := clRed;
    lblKeyName.Caption := 'Please Select Private Key';
  end
  else
  begin
    lblKeyName.Font.Color := clGreen;
    i := PtrInt(cbNewPrivateKey.Items.Objects[cbNewPrivateKey.ItemIndex]);
    wk := TWallet.Keys.Key[i];
    lblKeyName.Caption := Format('%s ',
      [IIF(wk.Name = '', TCrypto.ToHexaString(
      TAccountComp.AccountKey2RawString(wk.AccountKey)), wk.Name)]);
  end;
end;

procedure TWIZChangeKey_SelectKey.UpdateWalletKeys();
var
  i: integer;
  wk: TWalletKey;
  s: string;
begin
  cbNewPrivateKey.items.BeginUpdate;
  try
    cbNewPrivateKey.Items.Clear;
    cbNewPrivateKey.Items.Add('Select Private Key');
    if not Assigned(TWallet.Keys) then
      Exit;
    for i := 0 to TWallet.Keys.Count - 1 do
    begin
      wk := TWallet.Keys.Key[i];
      s := IIF(wk.Name = '', TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(wk.AccountKey)), wk.Name);
      if not Assigned(wk.PrivateKey) then
        s := s + '(*)';
      cbNewPrivateKey.Items.AddObject(s, TObject(i));
    end;
    cbNewPrivateKey.Sorted := True;
  finally
    cbNewPrivateKey.Items.EndUpdate;
  end;
end;

procedure TWIZChangeKey_SelectKey.OnPresent;
begin
  UpdateWalletKeys();
  if TWallet.Keys.Count = 1 then
    cbNewPrivateKey.ItemIndex := 1
  else
    cbNewPrivateKey.ItemIndex := Model.ChangeAccountPrivateKey.SelectedIndex;
  cbNewPrivateKeyChange(Self);
  if Length(Model.Account.SelectedAccounts) > 1 then
  begin
    chkChooseFee.Checked := True;
    chkChooseFee.Enabled := False;
  end;
  cbNewPrivateKey.SetFocus;
end;

procedure TWIZChangeKey_SelectKey.OnNext;
begin
  Model.ChangeAccountPrivateKey.SelectedIndex := cbNewPrivateKey.ItemIndex;
  Model.ChangeAccountPrivateKey.NewWalletKey := TWallet.Keys.Key[PtrInt(cbNewPrivateKey.Items.Objects[cbNewPrivateKey.ItemIndex])];
  Model.Payload.HasPayload := chkAttachPayload.Checked;

  if chkChooseFee.Checked then
    UpdatePath(ptInject, [TWIZOperationFee_Custom])
  else
  begin
    Model.Fee.SingleOperationFee := TSettings.DefaultFee;
    if Model.Payload.HasPayload then
      UpdatePath(ptInject, [TWIZOperationPayload_Encryption])
    else if Length(Model.Account.SelectedAccounts) > 1 then
      UpdatePath(ptInject, [TWIZOperationSigner_Select])
    else
    begin
      Model.Signer.SignerAccount := Model.Account.SelectedAccounts[0];
      Model.Signer.OperationSigningMode := akaPrimary;
    end;
  end;

end;

function TWIZChangeKey_SelectKey.Validate(out message: ansistring): boolean;
var
  i: integer;
  tempAccountKey: TAccountKey;
begin
  Result := True;
  if cbNewPrivateKey.ItemIndex < 1 then
  begin
    message := 'A key must be selected';
    Result := False;
    Exit;
  end;

  tempAccountKey := TWallet.Keys.Key[PtrInt(cbNewPrivateKey.Items.Objects[cbNewPrivateKey.ItemIndex])].AccountKey;

  for i := Low(Model.Account.SelectedAccounts) to High(Model.Account.SelectedAccounts) do
    if TAccountComp.EqualAccountKeys(Model.Account.SelectedAccounts[i].accountInfo.accountKey,
      tempAccountKey) then
    begin
      Result := False;
      message := 'New key is same as current key';
      Exit;
    end;

end;

end.
