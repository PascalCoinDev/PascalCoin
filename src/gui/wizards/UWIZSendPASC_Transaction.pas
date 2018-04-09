unit UWIZSendPASC_Transaction;

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
  UFRMAccountSelect, UNode, UWizard, UWIZSendPASC, UWIZSendPASC_Confirmation, UWIZSendPASC_TransactionPayload, UWIZModels;

type

  { TWIZSendPASC_Transaction }

  TWIZSendPASC_Transaction = class(TWizardForm<TWIZOperationsModel>)
    cbSignerAccount: TComboBox;
    edtOpFee: TEdit;
    edtAmt: TEdit;
    edtDestAcc: TEdit;
    gbTransaction: TGroupBox;
    lblOpFee: TLabel;
    lblAmount: TLabel;
    lblDestAcc: TLabel;
    lblTotalBalanceValue: TLabel;
    lblTotalBalances: TLabel;
    lblBalance: TLabel;
    btnSearch: TSpeedButton;
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

{ TWIZSendPASC_Transaction }

procedure TWIZSendPASC_Transaction.cbSignerAccountChange(Sender: TObject);
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
      [TAccountComp.FormatMoney(Model.SendPASCModel.SelectedAccounts[PtrInt(
      cbSignerAccount.Items.Objects[cbSignerAccount.ItemIndex])].Balance)]);
  end;
end;

procedure TWIZSendPASC_Transaction.btnSearchClick(Sender: TObject);
var
  F: TFRMAccountSelect;
  c: cardinal;
begin
  F := TFRMAccountSelect.Create(Self);
  F.Position := poMainFormCenter;
  try
    F.Node := TNode.Node;
    F.WalletKeys := TWallet.Keys;
    F.Filters := edtDestAcc.Tag;
    if TAccountComp.AccountTxtNumberToAccountNumber(edtDestAcc.Text, c) then
      F.DefaultAccount := c;
    F.AllowSelect := True;
    if F.ShowModal = mrOk then
    begin
      edtDestAcc.Text := TAccountComp.AccountNumberToAccountTxtNumber(F.GetSelected);
    end;
  finally
    F.Free;
  end;
end;

procedure TWIZSendPASC_Transaction.OnPresent;

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
    for i := 0 to High(Model.SendPASCModel.SelectedAccounts) do
    begin
      acc := Model.SendPASCModel.SelectedAccounts[i];
      accNumberwithChecksum := GetAccNoWithChecksum(acc.account);
      totalBalance := totalBalance + acc.balance;
      cbSignerAccount.Items.AddObject(accNumberwithChecksum, TObject(i));
    end;
  finally
    cbSignerAccount.Items.EndUpdate;
  end;
  cbSignerAccount.ItemIndex := Model.SendPASCModel.SelectedIndex;
  cbSignerAccountChange(Self);
  lblTotalBalanceValue.Caption :=
    Format('%s PASC', [TAccountComp.FormatMoney(totalBalance)]);

  if Length(Model.SendPASCModel.SelectedAccounts) > 1 then
  begin
    edtAmt.Text := 'ALL BALANCE';
    edtAmt.Enabled := False;
  end
  else
  begin
    edtAmt.Text := TAccountComp.FormatMoney(0);
  end;
  edtOpFee.Text := TAccountComp.FormatMoney(TSettings.DefaultFee);
end;

procedure TWIZSendPASC_Transaction.OnNext;

  function GetAccNoWithoutChecksum(const AAccount: string): cardinal;
  begin
    if not TAccountComp.AccountTxtNumberToAccountNumber(AAccount, Result) then
    begin
      raise Exception.Create(
        'Internal Error: Unable to parse account number from input');
    end;
  end;

  function GetAccounts(AccountNumber: cardinal): TAccount;
  var
    acc: TAccount;
    safeBox: TPCSafeBox;
    keys: TOrderedAccountKeysList;
    i: integer;
  begin
    keys := TWallet.keys.AccountsKeyList;
    safeBox := TUserInterface.Node.Bank.safeBox;
    safeBox.StartThreadSafe;
    try
      acc := safeBox.Account(AccountNumber);
      if keys.IndexOfAccountKey(acc.accountInfo.accountKey) >= 0 then
      begin
        Result := acc;
      end;
    finally
      safeBox.EndThreadSave;
    end;

  end;

begin
  Model.SendPASCModel.SelectedIndex := cbSignerAccount.ItemIndex;
  Model.SendPASCModel.SignerAccount := Model.SendPASCModel.SelectedAccounts[PtrInt(
    cbSignerAccount.Items.Objects[cbSignerAccount.ItemIndex])];
  Model.SendPASCModel.DestinationAccount := GetAccounts(GetAccNoWithoutChecksum(edtDestAcc.Text));
  Model.SendPASCModel.AmountToSend := edtAmt.Text;
  UpdatePath(ptReplaceAllNext, [TWIZSendPASC_TransactionPayload, TWIZSendPASC_Confirmation]);
end;

function TWIZSendPASC_Transaction.Validate(out message: ansistring): boolean;

  function GetAccNoWithChecksum(constref AAccount: TAccount): string;
  begin
    Result := TAccountComp.AccountNumberToAccountTxtNumber(AAccount.account);
  end;

var
  AccountNumbersWithChecksum: TArray<string>;
  Accounts: TArray<TAccount>;
  c: cardinal;
  DestAccount: TAccount;
  amount, opfee: int64;
  i: integer;
begin
  Accounts := Model.SendPASCModel.SelectedAccounts;
  Result := True;
  if cbSignerAccount.ItemIndex < 1 then
  begin
    message := 'A signer account must be selected';
    Result := False;
    Exit;
  end;

  if not (TAccountComp.AccountTxtNumberToAccountNumber(edtDestAcc.Text, c)) then
  begin
    message := 'Invalid destination account (' + edtDestAcc.Text + ')';
    Result := False;
    Exit;
  end;

  if (c < 0) or (c >= TNode.Node.Bank.AccountsCount) then
  begin
    message := 'Invalid destination account (' +
      TAccountComp.AccountNumberToAccountTxtNumber(c) + ')';
    Result := False;
    Exit;
  end;

  DestAccount := TNode.Node.Operations.SafeBoxTransaction.account(c);
  if Length(Accounts) = 1 then
  begin
    if not TAccountComp.TxtToMoney(edtAmt.Text, amount) then
    begin
      message := 'Invalid amount (' + edtAmt.Text + ')';
      Result := False;
      Exit;
    end;
  end;

  AccountNumbersWithChecksum := TListTool<TAccount, string>.Transform(Accounts, GetAccNoWithCheckSum);

  if TArrayTool<string>.Contains(AccountNumbersWithChecksum, edtDestAcc.Text) then
  begin
    message := 'Sender and destination account are the same';
    Result := False;
    Exit;
  end;

  if not TAccountComp.TxtToMoney(Trim(edtOpFee.Text), Model.SendPASCModel.DefaultFee) then
  begin
    message := 'Invalid fee value "' + edtOpFee.Text + '"';
    Result := False;
    Exit;
  end;

  //for i := Low(Accounts) to High(Accounts) do
  //begin
  //  if (Accounts[i].balance < (amount + opfee)) then
  //  begin
  //    message := 'Insufficient funds';
  //    Result := False;
  //    Exit;
  //  end;
  //end;

end;

end.
