unit UWIZEnlistAccountForSale_Transaction;

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
  UFRMAccountSelect, UNode, UWizard, UWIZEnlistAccountForSale,
  UWIZEnlistAccountForSale_TransactionPayload,
  UWIZEnlistAccountForSale_Confirmation;

type

  { TWIZEnlistAccountForSale_Transaction }

  TWIZEnlistAccountForSale_Transaction =
    class(TWizardForm<TWIZEnlistAccountForSaleModel>)
    btnSearch: TSpeedButton;
    cbSignerAccount: TComboBox;
    edtSalePrice: TEdit;
    edtSellerAccount: TEdit;
    edtOpFee: TEdit;
    gbTransaction: TGroupBox;
    lblBalance: TLabel;
    lblSalePrice: TLabel;
    lblSellerAccount: TLabel;
    lblOpFee: TLabel;
    lblTotalBalances: TLabel;
    lblTotalBalanceValue: TLabel;
    procedure btnSearchClick(Sender: TObject);
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

{ TWIZEnlistAccountForSale_Transaction }

procedure TWIZEnlistAccountForSale_Transaction.cbSignerAccountChange(Sender: TObject);
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

procedure TWIZEnlistAccountForSale_Transaction.btnSearchClick(Sender: TObject);
var
  F: TFRMAccountSelect;
  c: cardinal;
begin
  F := TFRMAccountSelect.Create(Self);
  F.Position := poMainFormCenter;
  try
    F.Node := TNode.Node;
    F.WalletKeys := TWallet.Keys;
    F.Filters := edtSellerAccount.Tag;
    if TAccountComp.AccountTxtNumberToAccountNumber(edtSellerAccount.Text, c) then
      F.DefaultAccount := c;
    F.AllowSelect := True;
    if F.ShowModal = mrOk then
    begin
      edtSellerAccount.Text := TAccountComp.AccountNumberToAccountTxtNumber(F.GetSelected);
    end;
  finally
    F.Free;
  end;
end;

procedure TWIZEnlistAccountForSale_Transaction.OnPresent;

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
  edtSalePrice.Text := TAccountComp.FormatMoney(0);
end;

procedure TWIZEnlistAccountForSale_Transaction.OnNext;
begin
  Model.SelectedIndex := cbSignerAccount.ItemIndex;
  Model.SignerAccount := Model.SelectedAccounts[PtrInt(
    cbSignerAccount.Items.Objects[cbSignerAccount.ItemIndex])];

  UpdatePath(ptReplaceAllNext, [TWIZEnlistAccountForSale_TransactionPayload,
    TWIZEnlistAccountForSale_Confirmation]);
end;

function TWIZEnlistAccountForSale_Transaction.Validate(out message: ansistring): boolean;
var
  c: cardinal;
  i: integer;
begin
  Result := True;
  if cbSignerAccount.ItemIndex < 1 then
  begin
    message := 'A signer account must be selected';
    Result := False;
    Exit;
  end;

  if not TAccountComp.TxtToMoney(edtSalePrice.Text, Model.SalePrice) then
  begin
    message := 'Invalid price (' + edtSalePrice.Text + ')';
    Result := False;
    Exit;
  end;

  if not (TAccountComp.AccountTxtNumberToAccountNumber(edtSellerAccount.Text, c)) then
  begin
    message := 'Invalid seller account (' + edtSellerAccount.Text + ')';
    Result := False;
    Exit;
  end;

  if (c < 0) or (c >= TNode.Node.Bank.AccountsCount) then
  begin
    message := 'Invalid seller account (' +
      TAccountComp.AccountNumberToAccountTxtNumber(c) + ')';
    Result := False;
    Exit;
  end;

  for i := Low(Model.SelectedAccounts) to High(Model.SelectedAccounts) do
  begin
    if (Model.SelectedAccounts[i].Account = c) then
    begin
      message := 'Seller account cannot be same account';
      Result := False;
      Exit;
    end;
  end;

  Model.SellerAccount := TNode.Node.Operations.SafeBoxTransaction.account(c);

  if not TAccountComp.TxtToMoney(Trim(edtOpFee.Text), Model.DefaultFee) then
  begin
    message := 'Invalid fee value "' + edtOpFee.Text + '"';
    Result := False;
    Exit;
  end;

end;

end.
