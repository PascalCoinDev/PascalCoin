unit UWIZEnlistAccountForSale_EnterSaleAmount;

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
  SysUtils, StdCtrls, UWizard, UWIZOperation;

type

  { TWIZEnlistAccountForSale_EnterSaleAmount }

  TWIZEnlistAccountForSale_EnterSaleAmount = class(TWizardForm<TWIZOperationsModel>)
    chkAttachPayload: TCheckBox;
    chkChooseFee: TCheckBox;
    edtAmt: TEdit;
    gbSalePrice: TGroupBox;
    gpOptions: TGroupBox;
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
  UAccounts,
  USettings,
  UCoreObjects,
  UWIZOperationFee_Custom,
  UWIZOperationSigner_Select,
  UWIZOperationPayload_Encryption;

{ TWIZEnlistAccountForSale_EnterSaleAmount }

procedure TWIZEnlistAccountForSale_EnterSaleAmount.UpdateUI();
begin
  edtAmt.Text := TAccountComp.FormatMoney(0);
end;

procedure TWIZEnlistAccountForSale_EnterSaleAmount.OnPresent;
begin
  UpdateUI();
  if Model.Account.Count > 1 then
  begin
    chkChooseFee.Checked := True;
    chkChooseFee.Enabled := False;
  end;
  if edtAmt.Enabled then
    edtAmt.SetFocus;
end;

procedure TWIZEnlistAccountForSale_EnterSaleAmount.OnNext;
begin
  Model.Payload.HasPayload := chkAttachPayload.Checked;

  if chkChooseFee.Checked then
    UpdatePath(ptInject, [TWIZOperationFee_Custom])
  else
  begin
    Model.Fee.SingleOperationFee := TSettings.DefaultFee;
    if Model.Payload.HasPayload then
      UpdatePath(ptInject, [TWIZOperationPayload_Encryption])
    else
    begin
      UpdatePath(ptInject, [TWIZOperationSigner_Select]);
    end;
  end;

end;

function TWIZEnlistAccountForSale_EnterSaleAmount.Validate(out message: ansistring): boolean;
var
  LSaleAmount: int64;
begin
  Result := True;

  if not TAccountComp.TxtToMoney(edtAmt.Text, LSaleAmount) then
  begin
    message := Format('Invalid Amount "%s"', [edtAmt.Text]);
    Exit(False);
  end;

  if LSaleAmount < 1 then
  begin
    message := 'You Must Sell For An Amount Greater Than Zero.';
    Exit(False);
  end;

  Model.EnlistAccountForSale.SalePrice := LSaleAmount;

end;

end.
