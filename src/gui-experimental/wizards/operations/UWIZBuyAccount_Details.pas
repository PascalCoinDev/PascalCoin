unit UWIZBuyAccount_Details;

{ Copyright (c) 2018 by Sphere 10 Software <http://www.sphere10.com/>

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  Acknowledgements:
  - Ugochukwu Mmaduekwe - main developer
  - Herman Schoenfeld - designer

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$mode delphi}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, Forms, Dialogs, Controls, StdCtrls, ExtCtrls, Buttons, UWizard, UWIZOperation;

type

  { TWIZBuyAccount_Details }

  TWIZBuyAccount_Details = class(TWizardForm<TWIZOperationsModel>)
    btnGetNewKey: TSpeedButton;
    chkPayload: TCheckBox;
    chkCustomFee: TCheckBox;
    cbPrivateKeys: TComboBox;
    gbNewPrivateKey: TGroupBox;
    txtAmount: TEdit;
    txtAccountToBuy: TEdit;
    gbAccountToBuy: TGroupBox;
    gpAmount: TGroupBox;
    gpOptions: TGroupBox;
    lblAccountToBuyDetails: TLabel;
    lblAccountToBuyNotice: TLabel;
    btnSearch: TSpeedButton;
    lblAmountNotice: TLabel;
    procedure btnGetNewKeyClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure txtAccountToBuyChange(Sender: TObject);
    procedure UpdateUI();

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
  UNode,
  UWallet,
  UCommon,
  UCrypto,
  UMemory,
  UAccounts,
  USettings,
  UCoreUtils,
  UBaseTypes,
  UCoreObjects,
  UFRMWalletKeys,
  UFRMAccountSelect,
  UCommon.Collections,
  Generics.Collections,
  UWIZOperationFee_Custom,
  UWIZOperationPayload_Encryption;

{ TWIZBuyAccount_Details }

procedure TWIZBuyAccount_Details.txtAccountToBuyChange(Sender: TObject);
begin
  UpdateUI();
end;

procedure TWIZBuyAccount_Details.OnPresent;
begin
  UpdateUI();

  UpdateWalletKeys();
  if TWallet.Keys.Count = 1 then
    cbPrivateKeys.ItemIndex := 1
  else
    cbPrivateKeys.ItemIndex := Model.BuyAccount.SelectedIndex;
  cbPrivateKeys.SetFocus;

  if Model.Account.Count > 1 then
  begin
    chkCustomFee.Checked := True;
    chkCustomFee.Enabled := False;
  end;

  // Account To Buy section
  txtAccountToBuy.SetFocus;
  txtAccountToBuy.Tag := CT_AS_OnlyForSale;

end;

procedure TWIZBuyAccount_Details.UpdateUI();
var
  LTempAccount: TAccount;
  LAccountNumber: cardinal;
begin
  // Account To Buy section
  if TAccountComp.AccountTxtNumberToAccountNumber(txtAccountToBuy.Text, LAccountNumber) then
    if ((LAccountNumber < 0) or (LAccountNumber >= TNode.Node.Bank.AccountsCount)) then
      lblAccountToBuyDetails.Caption := ''
    else
    begin
      LTempAccount := TNode.Node.Operations.SafeBoxTransaction.account(LAccountNumber);
      lblAccountToBuyDetails.Caption := LTempAccount.Name.ToPrintable;
    end;
end;

procedure TWIZBuyAccount_Details.UpdateWalletKeys();
var
  LIdx: integer;
  LWalletKey: TWalletKey;
  LBuilder: string;
begin
  cbPrivateKeys.items.BeginUpdate;
  try
    cbPrivateKeys.Items.Clear;
    cbPrivateKeys.Items.Add('Select Private Key');
    if not Assigned(TWallet.Keys) then
      Exit;
    for LIdx := 0 to TWallet.Keys.Count - 1 do
    begin
      LWalletKey := TWallet.Keys.Key[LIdx];
      LBuilder := IIF(LWalletKey.Name = '', TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(LWalletKey.AccountKey)), LWalletKey.Name);
      if not Assigned(LWalletKey.PrivateKey) then
        LBuilder := LBuilder + '(*)';
      cbPrivateKeys.Items.AddObject(LBuilder, TObject(LIdx));
    end;
  finally
    cbPrivateKeys.Items.EndUpdate;
  end;
end;

procedure TWIZBuyAccount_Details.OnNext;
var
  LAccountNumber: cardinal;
  LAccount: TAccount;
  LWizStepsToInject: TList<TComponentClass>;
  LDisposables: TDisposables;
begin
  LWizStepsToInject := LDisposables.AddObject(TList<TComponentClass>.Create) as TList<TComponentClass>;

  // Account To Buy section
  TAccountComp.AccountTxtNumberToAccountNumber(txtAccountToBuy.Text, LAccountNumber);
  Model.BuyAccount.AccountToBuy := TNode.Node.Operations.SafeBoxTransaction.account(LAccountNumber);

  // Fee Section
  Model.Fee.SingleOperationFee := TSettings.DefaultFee;
  if chkCustomFee.Checked then
    LWizStepsToInject.Add(TWIZOperationFee_Custom);

  // Payload Section
  Model.Payload.HasPayload := IIF(chkPayload.Checked, True, False);
  if Model.Payload.HasPayload then
    LWizStepsToInject.Add(TWIZOperationPayload_Encryption);

  // Update wizard flow if applicable
  if LWizStepsToInject.Count > 0 then
    UpdatePath(ptInject, LWizStepsToInject.ToArray);

  Model.BuyAccount.SelectedIndex := cbPrivateKeys.ItemIndex;
end;

function TWIZBuyAccount_Details.Validate(out message: ansistring): boolean;
var
  LAccountNumbersWithChecksum: TArray<string>;
  LAccountNumber: cardinal;
  LAmount: int64;
  LIdx: integer;
  LAccountToBuy: TAccount;
begin
  Result := True;
  // Buy Account section
  if not (TAccountComp.AccountTxtNumberToAccountNumber(txtAccountToBuy.Text, LAccountNumber)) then
  begin
    message := Format('Invalid account "%s"', [txtAccountToBuy.Text]);
    Exit(False);
  end;

  if (LAccountNumber < 0) or (LAccountNumber >= TNode.Node.Bank.AccountsCount) or (LAccountNumber = Model.Account.SelectedAccounts[0].account) then
  begin
    message := Format('Invalid account to buy "%s"', [TAccountComp.AccountNumberToAccountTxtNumber(LAccountNumber)]);
    Exit(False);
  end;

  LAccountToBuy := TNode.Node.Operations.SafeBoxTransaction.Account(LAccountNumber);

  if not TAccountComp.IsAccountForSale(LAccountToBuy.accountInfo) then
  begin
    message := Format('Account "%s" is not enlisted for sale', [LAccountToBuy.AccountString]);
    Exit(False);
  end;

  if not TAccountComp.TxtToMoney(txtAmount.Text, LAmount) then
  begin
    message := 'Invalid Amount Value';
    Exit(False);
  end;

  if (LAccountToBuy.accountInfo.price > LAmount) then
  begin
    message := Format('Account Price is "%s"', [TAccountComp.FormatMoney(LAccountToBuy.accountInfo.price)]);
    Exit(False);
  end;

  if ((LAmount + TSettings.DefaultFee) > Model.Account.SelectedAccounts[0].balance) then
  begin
    message := Format('Insufficient Funds in Account "%s" to buy Account "%s"', [Model.Account.SelectedAccounts[0].AccountString, LAccountToBuy.AccountString]);
    Exit(False);
  end;

  if cbPrivateKeys.ItemIndex < 1 then
  begin
    message := 'Please select a Private Key';
    Exit(False);
  end;

  LIdx := PtrInt(cbPrivateKeys.Items.Objects[cbPrivateKeys.ItemIndex]);
  if (LIdx < 0) or (LIdx >= TWallet.Keys.Count) then
  begin
    message := 'Invalid Selected Key';
    Exit(False);
  end;
  Model.BuyAccount.NewOwnerPublicKey := TWallet.Keys.Key[LIdx].AccountKey;
  Model.BuyAccount.Amount := LAmount;

end;

procedure TWIZBuyAccount_Details.btnSearchClick(Sender: TObject);
var
  LFRMAccountSelect: TFRMAccountSelect;
  LAccountNumber: cardinal;
begin
  LFRMAccountSelect := TFRMAccountSelect.Create(Self);
  LFRMAccountSelect.Position := poMainFormCenter;
  try
    LFRMAccountSelect.Node := TNode.Node;
    LFRMAccountSelect.WalletKeys := TWallet.Keys;
    LFRMAccountSelect.Filters := txtAccountToBuy.Tag;
    if TAccountComp.AccountTxtNumberToAccountNumber(txtAccountToBuy.Text, LAccountNumber) then
      LFRMAccountSelect.DefaultAccount := LAccountNumber;
    LFRMAccountSelect.AllowSelect := True;
    if LFRMAccountSelect.ShowModal = mrOk then
      txtAccountToBuy.Text := TAccountComp.AccountNumberToAccountTxtNumber(LFRMAccountSelect.GetSelected);
  finally
    LFRMAccountSelect.Free;
  end;
end;

procedure TWIZBuyAccount_Details.btnGetNewKeyClick(Sender: TObject);
var
  LFRMWalletKeys: TFRMWalletKeys;
begin
  LFRMWalletKeys := TFRMWalletKeys.Create(Self);
  LFRMWalletKeys.Position := poMainFormCenter;
  try
    LFRMWalletKeys.ShowModal;
    cbPrivateKeys.SetFocus;
    UpdateWalletKeys;
  finally
    LFRMWalletKeys.Free;
  end;
end;

end.
