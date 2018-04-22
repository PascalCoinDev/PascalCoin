unit UWIZEnlistAccountForSale_EnterSaleAmount;

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
  UFRMAccountSelect, UNode, UWizard, UWIZOperationFee_Custom, UWIZOperationPayload_Encryption, UWIZOperationSigner_Select, UDataObjects;

type

  { TWIZEnlistAccountForSale_EnterSaleAmount }

  TWIZEnlistAccountForSale_EnterSaleAmount = class(TWizardForm<TExecuteOperationsModel>)
    chkChooseFee: TCheckBox;
    chkAttachPayload: TCheckBox;
    edtAmt: TEdit;
    gbSalePrice: TGroupBox;
    lblSaleAmountNotice: TLabel;
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

{ TWIZSendPASC_EnterQuantity }

procedure TWIZEnlistAccountForSale_EnterSaleAmount.UpdateUI();
begin
  edtAmt.Text := TAccountComp.FormatMoney(0);
end;

procedure TWIZEnlistAccountForSale_EnterSaleAmount.OnPresent;
begin
  UpdateUI();
  if Length(Model.Account.SelectedAccounts) > 1 then
  begin
    chkChooseFee.Checked := True;
    chkChooseFee.Enabled := False;
  end;
  if edtAmt.Enabled then
    edtAmt.SetFocus;
end;

procedure TWIZEnlistAccountForSale_EnterSaleAmount.OnNext;
var
  amount: int64;
begin
  Model.Payload.HasPayload := chkAttachPayload.Checked;
  TAccountComp.TxtToMoney(edtAmt.Text, Model.EnlistAccountForSale.SalePrice);

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

function TWIZEnlistAccountForSale_EnterSaleAmount.Validate(out message: ansistring): boolean;
var
  amount: int64;
begin
  Result := True;

  if not TAccountComp.TxtToMoney(edtAmt.Text, amount) then
  begin
    message := 'Invalid amount (' + edtAmt.Text + ')';
    Result := False;
    Exit;
  end;

  if amount < 1 then
  begin
    message := 'Invalid amount (' + edtAmt.Text + '), you must sell for an amount greater than zero';
    Result := False;
    Exit;
  end;

end;

end.
