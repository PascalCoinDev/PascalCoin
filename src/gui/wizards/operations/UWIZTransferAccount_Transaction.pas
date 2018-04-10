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
  UFRMAccountSelect, UNode, UWizard, UWIZTransferAccount,
  UWIZTransferAccount_TransactionPayload,
  UWIZTransferAccount_Confirmation, UWIZModels;

type

  { TWIZTransferAccount_Transaction }

  TWIZTransferAccount_Transaction = class(TWizardForm<TWIZOperationsModel>)
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
    lblBalance.Caption := Format('%s PASC', [TAccountComp.FormatMoney(Model.TransferAccount.SelectedAccounts[PtrInt(cbSignerAccount.Items.Objects[cbSignerAccount.ItemIndex])].Balance)]);
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
    for i := Low(Model.TransferAccount.SelectedAccounts) to High(Model.TransferAccount.SelectedAccounts) do
    begin
      acc := Model.TransferAccount.SelectedAccounts[i];
      accNumberwithChecksum := GetAccNoWithChecksum(acc.account);
      totalBalance := totalBalance + acc.balance;
      cbSignerAccount.Items.AddObject(accNumberwithChecksum, TObject(i));
    end;
  finally
    cbSignerAccount.Items.EndUpdate;
  end;
  cbSignerAccount.ItemIndex := Model.TransferAccount.SelectedIndex;
  cbSignerAccountChange(Self);
  lblTotalBalanceValue.Caption :=
    Format('%s PASC', [TAccountComp.FormatMoney(totalBalance)]);

  edtOpFee.Text := TAccountComp.FormatMoney(TSettings.Fee.DefaultFee);
end;

procedure TWIZTransferAccount_Transaction.OnNext;
begin
  Model.TransferAccount.SelectedIndex := cbSignerAccount.ItemIndex;
  Model.Signer.SignerAccount := Model.TransferAccount.SelectedAccounts[PtrInt(
    cbSignerAccount.Items.Objects[cbSignerAccount.ItemIndex])];
  Model.TransferAccount.NewPublicKey := Trim(edtPublicKey.Text);

  UpdatePath(ptReplaceAllNext, [TWIZTransferAccount_TransactionPayload,
    TWIZTransferAccount_Confirmation]);
end;

function TWIZTransferAccount_Transaction.Validate(out message: ansistring): boolean;
var
  i: integer;
begin
  Result := True;
  if cbSignerAccount.ItemIndex < 1 then
  begin
    message := 'A signer account must be selected';
    Result := False;
    Exit;
  end;

  if not TAccountComp.TxtToMoney(Trim(edtOpFee.Text), Model.TransferAccount.Fee.DefaultFee) then
  begin
    message := 'Invalid fee value "' + edtOpFee.Text + '"';
    Result := False;
    Exit;
  end;

  Result := TAccountComp.AccountKeyFromImport(edtPublicKey.Text,
    Model.TransferAccount.AccountKey, message);
  for i := Low(Model.TransferAccount.SelectedAccounts) to High(Model.TransferAccount.SelectedAccounts) do
  begin
    if TAccountComp.EqualAccountKeys(Model.TransferAccount.SelectedAccounts[i].accountInfo.accountKey,
      Model.TransferAccount.AccountKey) then
    begin
      Result := False;
      message := 'new public key is same as selected account public key';
      Exit;
    end;
  end;
end;

end.
