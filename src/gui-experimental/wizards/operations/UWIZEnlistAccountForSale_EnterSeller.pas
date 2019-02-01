unit UWIZEnlistAccountForSale_EnterSeller;

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
  SysUtils, Forms, StdCtrls, Buttons, Controls, Graphics, UWizard, UWIZOperation, Classes;

type

  { TWIZEnlistAccountForSale_EnterSeller }

  TWIZEnlistAccountForSale_EnterSeller = class(TWizardForm<TWIZOperationsModel>)
    btnSearch: TSpeedButton;
    edtBeneficiaryAcc: TEdit;
    gbBeneficiary: TGroupBox;
    lblBeneficiaryDetails: TLabel;
    lblBeneficiaryNotice: TLabel;
    procedure btnSearchClick(Sender: TObject);
    procedure edtBeneficiaryAccChange(Sender: TObject);
    procedure UpdateUI();

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
  UAccounts,
  UCoreUtils,
  UBaseTypes,
  UFRMAccountSelect;

{ TWIZEnlistAccountForSale_EnterSeller }

procedure TWIZEnlistAccountForSale_EnterSeller.UpdateUI();
var
  LAccount: TAccount;
  LAccountNumber: cardinal;
begin
  if TAccountComp.AccountTxtNumberToAccountNumber(edtBeneficiaryAcc.Text, LAccountNumber) then
    if (LAccountNumber < 0) or (LAccountNumber >= TNode.Node.Bank.AccountsCount) then
    begin
      lblBeneficiaryDetails.Caption := '';
      lblBeneficiaryDetails.Visible := False;
    end
    else
    begin
      LAccount := TNode.Node.Operations.SafeBoxTransaction.account(LAccountNumber);
      lblBeneficiaryDetails.Caption := LAccount.Name.ToPrintable;
      lblBeneficiaryDetails.Visible := True;
    end;
end;

procedure TWIZEnlistAccountForSale_EnterSeller.OnPresent;
begin
  UpdateUI();
  edtBeneficiaryAcc.SetFocus;
  edtBeneficiaryAcc.Tag := CT_AS_MyAccounts;
end;

procedure TWIZEnlistAccountForSale_EnterSeller.btnSearchClick(Sender: TObject);
var
  LFRMAccountSelect: TFRMAccountSelect;
  LAccountNumber: cardinal;
begin
  LFRMAccountSelect := TFRMAccountSelect.Create(Self);
  LFRMAccountSelect.Position := poMainFormCenter;
  try
    LFRMAccountSelect.Node := TNode.Node;
    LFRMAccountSelect.WalletKeys := TWallet.Keys;
    LFRMAccountSelect.Filters := edtBeneficiaryAcc.Tag;
    if TAccountComp.AccountTxtNumberToAccountNumber(edtBeneficiaryAcc.Text, LAccountNumber) then
      LFRMAccountSelect.DefaultAccount := LAccountNumber;
    LFRMAccountSelect.AllowSelect := True;
    if LFRMAccountSelect.ShowModal = mrOk then
      edtBeneficiaryAcc.Text := TAccountComp.AccountNumberToAccountTxtNumber(LFRMAccountSelect.GetSelected);
  finally
    LFRMAccountSelect.Free;
  end;
end;

procedure TWIZEnlistAccountForSale_EnterSeller.edtBeneficiaryAccChange(Sender: TObject);
begin
  UpdateUI();
end;

procedure TWIZEnlistAccountForSale_EnterSeller.OnNext;
var
  LAccountNumber: cardinal;
  LAccount: TAccount;
begin
  TAccountComp.AccountTxtNumberToAccountNumber(edtBeneficiaryAcc.Text, LAccountNumber);
  Model.SendPASC.DestinationAccount := TNode.Node.Operations.SafeBoxTransaction.account(LAccountNumber);
end;

function TWIZEnlistAccountForSale_EnterSeller.Validate(out message: ansistring): boolean;
var
  LIdx: integer;
  LAccountNumber: cardinal;
begin
  Result := True;

  if not (TAccountComp.AccountTxtNumberToAccountNumber(edtBeneficiaryAcc.Text, LAccountNumber)) then
  begin
    message := Format('Invalid Seller Account "%s"', [edtBeneficiaryAcc.Text]);
    Exit(False);
  end;

  if (LAccountNumber < 0) or (LAccountNumber >= TNode.Node.Bank.AccountsCount) then
  begin
    message := Format('Invalid Seller Account "%s"', [TAccountComp.AccountNumberToAccountTxtNumber(LAccountNumber)]);
    Exit(False);
  end;

  for LIdx := Low(Model.Account.SelectedAccounts) to High(Model.Account.SelectedAccounts) do
    if (Model.Account.SelectedAccounts[LIdx].Account = LAccountNumber) then
    begin
      message := 'Seller Account Cannot Be Same As Account To Be Sold.';
      Exit(False);
    end;

  Model.EnlistAccountForSale.SellerAccount := TNode.Node.Operations.SafeBoxTransaction.account(LAccountNumber);

end;

end.
