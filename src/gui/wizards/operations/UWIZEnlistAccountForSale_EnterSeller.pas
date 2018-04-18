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
  UFRMAccountSelect, UNode, UWizard, UWIZModels;

type

  { TWIZEnlistAccountForSale_EnterSeller }

  TWIZEnlistAccountForSale_EnterSeller = class(TWizardForm<TWIZOperationsModel>)
    edtSellerAcc: TEdit;
    gbSeller: TGroupBox;
    lblSellerAccNumber: TLabel;
    lblSellerAccNumberValue: TLabel;
    lblSellerAccNumberName: TLabel;
    lblSellerAccNumberNameValue: TLabel;
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
  UAccounts, UUserInterface, USettings;

{ TWIZEnlistAccountForSale_EnterSeller }

procedure TWIZEnlistAccountForSale_EnterSeller.edtSellerAccChange(Sender: TObject);
begin
  UpdateUI();
end;

procedure TWIZEnlistAccountForSale_EnterSeller.UpdateUI();
var
  tempAcc: TAccount;
  c: cardinal;
begin
  if TAccountComp.AccountTxtNumberToAccountNumber(edtSellerAcc.Text, c) then
  begin
    if (c < 0) or (c >= TNode.Node.Bank.AccountsCount) then
    begin
      lblSellerAccNumberValue.Caption := 'unknown';
      lblSellerAccNumberNameValue.Caption := 'unknown';
      Exit;
    end;
    tempAcc := TNode.Node.Operations.SafeBoxTransaction.account(c);
    lblSellerAccNumberValue.Caption := edtSellerAcc.Text;
    lblSellerAccNumberNameValue.Caption := tempAcc.Name;
  end
  else
  begin
    lblSellerAccNumberValue.Caption := 'unknown';
    lblSellerAccNumberNameValue.Caption := 'unknown';
  end;

end;

procedure TWIZEnlistAccountForSale_EnterSeller.OnPresent;
begin
  UpdateUI();
  edtSellerAcc.SetFocus;
end;

procedure TWIZEnlistAccountForSale_EnterSeller.btnSearchClick(Sender: TObject);
var
  F: TFRMAccountSelect;
  c: cardinal;
begin
  F := TFRMAccountSelect.Create(Self);
  F.Position := poMainFormCenter;
  try
    F.Node := TNode.Node;
    F.WalletKeys := TWallet.Keys;
    F.Filters := edtSellerAcc.Tag;
    if TAccountComp.AccountTxtNumberToAccountNumber(edtSellerAcc.Text, c) then
      F.DefaultAccount := c;
    F.AllowSelect := True;
    if F.ShowModal = mrOk then
      edtSellerAcc.Text := TAccountComp.AccountNumberToAccountTxtNumber(F.GetSelected);
  finally
    F.Free;
  end;
end;


procedure TWIZEnlistAccountForSale_EnterSeller.OnNext;
var
  c: cardinal;
  aa: TAccount;
begin
  TAccountComp.AccountTxtNumberToAccountNumber(edtSellerAcc.Text, c);
  Model.SendPASC.DestinationAccount := TNode.Node.Operations.SafeBoxTransaction.account(c);
end;

function TWIZEnlistAccountForSale_EnterSeller.Validate(out message: ansistring): boolean;
var
  i: integer;
  c: cardinal;
begin
  Result := True;

  if not (TAccountComp.AccountTxtNumberToAccountNumber(edtSellerAcc.Text, c)) then
  begin
    message := 'Invalid seller account (' + edtSellerAcc.Text + ')';
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

  for i := Low(Model.Account.SelectedAccounts) to High(Model.Account.SelectedAccounts) do
    if (Model.Account.SelectedAccounts[i].Account = c) then
    begin
      message := 'Seller account cannot be same account';
      Result := False;
      Exit;
    end;

  Model.EnlistAccountForSale.SellerAccount := TNode.Node.Operations.SafeBoxTransaction.account(c);

end;

end.
