unit UWIZEnlistAccountForSale_EnterSeller;

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
  UFRMAccountSelect, UNode, UWizard, UCoreObjects;

type

  { TWIZEnlistAccountForSale_EnterSeller }

  TWIZEnlistAccountForSale_EnterSeller = class(TWizardForm<TExecuteOperationsModel>)
    edtSellerAcc: TEdit;
    gbSeller: TGroupBox;
    lblSellerAccount: TLabel;
    lblDestNotice: TLabel;
    btnSearch: TSpeedButton;
    procedure btnSearchClick(Sender: TObject);
    procedure edtSellerAccChange(Sender: TObject);
    procedure UpdateUI();

  public
    procedure OnPresent; override;
    procedure OnNext; override;
    function Validate(out message: ansistring): boolean; override;
  end;


implementation

{$R *.lfm}

uses
  UAccounts, UUserInterface, USettings, UCoreUtils;

{ TWIZEnlistAccountForSale_EnterSeller }

procedure TWIZEnlistAccountForSale_EnterSeller.edtSellerAccChange(Sender: TObject);
begin
  UpdateUI();
end;

procedure TWIZEnlistAccountForSale_EnterSeller.UpdateUI();
var
  LAccount: TAccount;
  LAccountNumber: cardinal;
begin
  if TAccountComp.AccountTxtNumberToAccountNumber(edtSellerAcc.Text, LAccountNumber) then
    if (LAccountNumber < 0) or (LAccountNumber >= TNode.Node.Bank.AccountsCount) then
    begin
      lblSellerAccount.Caption := '';
      lblSellerAccount.Visible := False;
    end
    else
    begin
      LAccount := TNode.Node.Operations.SafeBoxTransaction.account(LAccountNumber);
      lblSellerAccount.Caption := LAccount.DisplayString;
      lblSellerAccount.Visible := True;
    end;
end;

procedure TWIZEnlistAccountForSale_EnterSeller.OnPresent;
begin
  UpdateUI();
  edtSellerAcc.SetFocus;
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
    LFRMAccountSelect.Filters := edtSellerAcc.Tag;
    if TAccountComp.AccountTxtNumberToAccountNumber(edtSellerAcc.Text, LAccountNumber) then
      LFRMAccountSelect.DefaultAccount := LAccountNumber;
    LFRMAccountSelect.AllowSelect := True;
    if LFRMAccountSelect.ShowModal = mrOk then
      edtSellerAcc.Text := TAccountComp.AccountNumberToAccountTxtNumber(LFRMAccountSelect.GetSelected);
  finally
    LFRMAccountSelect.Free;
  end;
end;


procedure TWIZEnlistAccountForSale_EnterSeller.OnNext;
var
  LAccountNumber: cardinal;
  LAccount: TAccount;
begin
  TAccountComp.AccountTxtNumberToAccountNumber(edtSellerAcc.Text, LAccountNumber);
  Model.SendPASC.DestinationAccount := TNode.Node.Operations.SafeBoxTransaction.account(LAccountNumber);
end;

function TWIZEnlistAccountForSale_EnterSeller.Validate(out message: ansistring): boolean;
var
  LIdx: integer;
  LAccountNumber: cardinal;
begin
  Result := True;

  if not (TAccountComp.AccountTxtNumberToAccountNumber(edtSellerAcc.Text, LAccountNumber)) then
  begin
    message := Format('Invalid Seller Account "%s"', [edtSellerAcc.Text]);
    Result := False;
    Exit;
  end;

  if (LAccountNumber < 0) or (LAccountNumber >= TNode.Node.Bank.AccountsCount) then
  begin
    message := Format('Invalid Seller Account "%s"', [TAccountComp.AccountNumberToAccountTxtNumber(LAccountNumber)]);
    Result := False;
    Exit;
  end;

  for LIdx := Low(Model.Account.SelectedAccounts) to High(Model.Account.SelectedAccounts) do
    if (Model.Account.SelectedAccounts[LIdx].Account = LAccountNumber) then
    begin
      message := 'Seller Account Cannot Be Same As Account To Be Sold.';
      Result := False;
      Exit;
    end;

  Model.EnlistAccountForSale.SellerAccount := TNode.Node.Operations.SafeBoxTransaction.account(LAccountNumber);

end;

end.
