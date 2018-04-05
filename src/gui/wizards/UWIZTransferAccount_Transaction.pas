unit UWIZTransferAccount_Transaction;

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
  UFRMAccountSelect, UNode, UWizard, UWIZTransferAccount, UWIZTransferAccount_TransactionPayload,
  UWIZTransferAccount_Confirmation;

type

  { TWIZTransferAccount_Transaction }

  TWIZTransferAccount_Transaction = class(TWizardForm<TWIZTransferAccountModel>)
    cbSignerAccount: TComboBox;
    edtOpFee: TEdit;
    edtPublicKey: TEdit;
    gbTransaction: TGroupBox;
    lblOpFee: TLabel;
    lblpublickey: TLabel;
    lblTotalBalanceValue: TLabel;
    lblTotalBalances: TLabel;
    lblBalance: TLabel;
    procedure cbSignerAccountChange(Sender: TObject);

  public
    procedure OnPresent; override;
    procedure OnNext; override;
    function Validate(out message: ansistring): boolean; override;
  end;


implementation

{$R *.lfm}

uses
  UAccounts, UUserInterface, USettings;

{ TWIZTransferAccount_Transaction }

procedure TWIZTransferAccount_Transaction.cbSignerAccountChange(Sender: TObject);
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
      [TAccountComp.FormatMoney(Model.SelectedAccounts[PtrInt(
      cbSignerAccount.Items.Objects[cbSignerAccount.ItemIndex])].Balance)]);
  end;
end;

procedure TWIZTransferAccount_Transaction.OnPresent;

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
    for i := Low(Model.SelectedAccounts) to High(Model.SelectedAccounts) do
    begin
      acc := Model.SelectedAccounts[i];
      accNumberwithChecksum := GetAccNoWithChecksum(acc.account);
      totalBalance := totalBalance + acc.balance;
      cbSignerAccount.Items.AddObject(accNumberwithChecksum, TObject(i));
    end;
  finally
    cbSignerAccount.Items.EndUpdate;
  end;
  cbSignerAccount.ItemIndex := Model.SelectedIndex;
  cbSignerAccountChange(Self);
  lblTotalBalanceValue.Caption :=
    Format('%s PASC', [TAccountComp.FormatMoney(totalBalance)]);

  edtOpFee.Text := TAccountComp.FormatMoney(TSettings.DefaultFee);
end;

procedure TWIZTransferAccount_Transaction.OnNext;
begin
  Model.SelectedIndex := cbSignerAccount.ItemIndex;
  Model.SignerAccount := Model.SelectedAccounts[PtrInt(
    cbSignerAccount.Items.Objects[cbSignerAccount.ItemIndex])];
  Model.NewPublicKey := Trim(edtPublicKey.Text);
  // make a deep copy
  Model.SortedAccounts := Copy(Model.SelectedAccounts);
  // must ensure that Signer account is last in the new array according version 2 protocol
  TArrayTool<TAccount>.MoveItem(Model.SortedAccounts,
    TArrayTool<TAccount>.IndexOf(Model.SortedAccounts, Model.SignerAccount),
    High(Model.SortedAccounts));
  UpdatePath(ptReplaceAllNext, [TWIZTransferAccount_TransactionPayload,
    TWIZTransferAccount_Confirmation]);
end;

function TWIZTransferAccount_Transaction.Validate(out message: ansistring): boolean;
var
  i: integer;
begin
  if cbSignerAccount.ItemIndex < 1 then
  begin
    message := 'A signer account must be selected';
    Result := False;
    Exit;
  end;

  if not TAccountComp.TxtToMoney(Trim(edtOpFee.Text), Model.DefaultFee) then
  begin
    message := 'Invalid fee value "' + edtOpFee.Text + '"';
    Result := False;
    Exit;
  end;

  Result := TAccountComp.AccountKeyFromImport(edtPublicKey.Text,
    Model.AccountKey, message);
  for i := Low(Model.SelectedAccounts) to High(Model.SelectedAccounts) do
  begin
    if TAccountComp.EqualAccountKeys(Model.SelectedAccounts[i].accountInfo.accountKey,
      Model.AccountKey) then
    begin
      Result := False;
      message := 'new public key is same as selected account public key';
    end;
  end;
end;

end.
