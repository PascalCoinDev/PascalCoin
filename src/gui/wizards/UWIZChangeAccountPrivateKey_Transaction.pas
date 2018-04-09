unit UWIZChangeAccountPrivateKey_Transaction;

{$mode delphi}
{$modeswitch nestedprocvars}

{ Copyright (c) 2018 by Ugochukwu Mmaduekwe

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, UCommon, UCommon.Collections, UWallet,
  UFRMAccountSelect, UNode, UWizard, UWIZChangeAccountPrivateKey,
  UWIZChangeAccountPrivateKey_TransactionPayload,
  UWIZChangeAccountPrivateKey_Confirmation, UWIZModels;

type

  { TWIZChangeAccountPrivateKey_Transaction }

  TWIZChangeAccountPrivateKey_Transaction = class(TWizardForm<TWIZOperationsModel>)
    cbSignerAccount: TComboBox;
    cbNewPrivateKey: TComboBox;
    edtOpFee: TEdit;
    gbTransaction: TGroupBox;
    lblKeyName: TLabel;
    lblOpFee: TLabel;
    lblprivatekey: TLabel;
    lblTotalBalanceValue: TLabel;
    lblTotalBalances: TLabel;
    lblBalance: TLabel;
    procedure cbNewPrivateKeyChange(Sender: TObject);
    procedure cbSignerAccountChange(Sender: TObject);
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

{ TWIZChangeAccountPrivateKey_Transaction }

procedure TWIZChangeAccountPrivateKey_Transaction.cbSignerAccountChange(Sender: TObject);
begin
  if cbSignerAccount.ItemIndex < 1 then
  begin
    lblBalance.Font.Color := clRed;
    lblBalance.Caption := 'Please Select Signer Account';
  end
  else
  begin
    lblBalance.Font.Color := clGreen;
    lblBalance.Caption := Format('%s PASC',
      [TAccountComp.FormatMoney(Model.ChangeAccountPrivateKeyModel.SelectedAccounts[PtrInt(cbSignerAccount.Items.Objects[cbSignerAccount.ItemIndex])].Balance)]);
  end;
end;

procedure TWIZChangeAccountPrivateKey_Transaction.cbNewPrivateKeyChange(
  Sender: TObject);
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

procedure TWIZChangeAccountPrivateKey_Transaction.UpdateWalletKeys();
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
    begin
      Exit;
    end;
    for i := 0 to TWallet.Keys.Count - 1 do
    begin
      wk := TWallet.Keys.Key[i];
      s := IIF(wk.Name = '', TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(wk.AccountKey)), wk.Name);
      if not Assigned(wk.PrivateKey) then
      begin
        s := s + '(*)';
      end;
      cbNewPrivateKey.Items.AddObject(s, TObject(i));
    end;
    cbNewPrivateKey.Sorted := True;
  finally
    cbNewPrivateKey.Items.EndUpdate;
  end;
end;

procedure TWIZChangeAccountPrivateKey_Transaction.OnPresent;

  function GetAccNoWithChecksum(AAccountNumber: cardinal): string;
  begin
    Result := TAccountComp.AccountNumberToAccountTxtNumber(AAccountNumber);
  end;

var
  acc: TAccount;
  accNumberwithChecksum: string;
  totalBalance: int64;
  i: integer;
begin
  cbSignerAccount.Items.BeginUpdate;
  totalBalance := 0;
  try
    cbSignerAccount.Items.Clear;
    cbSignerAccount.Items.Add('Select Signer Account');
    for i := Low(Model.ChangeAccountPrivateKeyModel.SelectedAccounts) to High(Model.ChangeAccountPrivateKeyModel.SelectedAccounts) do
    begin
      acc := Model.ChangeAccountPrivateKeyModel.SelectedAccounts[i];
      accNumberwithChecksum := GetAccNoWithChecksum(acc.account);
      totalBalance := totalBalance + acc.balance;
      cbSignerAccount.Items.AddObject(accNumberwithChecksum, TObject(i));
    end;
  finally
    cbSignerAccount.Items.EndUpdate;
  end;
  UpdateWalletKeys();
  cbSignerAccount.ItemIndex := Model.ChangeAccountPrivateKeyModel.SelectedIndex;
  cbSignerAccountChange(Self);
  cbNewPrivateKey.ItemIndex := Model.ChangeAccountPrivateKeyModel.PrivateKeySelectedIndex;
  cbNewPrivateKeyChange(Self);
  lblTotalBalanceValue.Caption :=
    Format('%s PASC', [TAccountComp.FormatMoney(totalBalance)]);

  edtOpFee.Text := TAccountComp.FormatMoney(TSettings.DefaultFee);
end;

procedure TWIZChangeAccountPrivateKey_Transaction.OnNext;
begin
  Model.ChangeAccountPrivateKeyModel.SelectedIndex := cbSignerAccount.ItemIndex;
  Model.ChangeAccountPrivateKeyModel.PrivateKeySelectedIndex := cbNewPrivateKey.ItemIndex;
  Model.ChangeAccountPrivateKeyModel.SignerAccount := Model.ChangeAccountPrivateKeyModel.SelectedAccounts[PtrInt(cbSignerAccount.Items.Objects[cbSignerAccount.ItemIndex])];
  Model.ChangeAccountPrivateKeyModel.NewWalletKey := TWallet.Keys.Key[PtrInt(cbNewPrivateKey.Items.Objects[cbNewPrivateKey.ItemIndex])];

  UpdatePath(ptReplaceAllNext, [TWIZChangeAccountPrivateKey_TransactionPayload, TWIZChangeAccountPrivateKey_Confirmation]);
end;

function TWIZChangeAccountPrivateKey_Transaction.Validate(out message: ansistring): boolean;
var
  i: integer;
begin
  if cbSignerAccount.ItemIndex < 1 then
  begin
    message := 'A signer account must be selected';
    Result := False;
    Exit;
  end;

  if cbNewPrivateKey.ItemIndex < 1 then
  begin
    message := 'A private key must be selected';
    Result := False;
    Exit;
  end;

  if not TAccountComp.TxtToMoney(Trim(edtOpFee.Text), Model.ChangeAccountPrivateKeyModel.DefaultFee) then
  begin
    message := 'Invalid fee value "' + edtOpFee.Text + '"';
    Result := False;
    Exit;
  end;

  //Result := TAccountComp.AccountKeyFromImport(edtPublicKey.Text,
  //  Model.AccountKey, message);
  //for i := Low(Model.SelectedAccounts) to High(Model.SelectedAccounts) do
  //begin
  //  if TAccountComp.EqualAccountKeys(Model.SelectedAccounts[i].accountInfo.accountKey,
  //    Model.AccountKey) then
  //  begin
  //    Result := False;
  //    message := 'new public key is same as selected account public key';
  //  end;
  //end;
end;

end.
