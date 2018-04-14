unit UWIZSendPASC_EnterQuantity;

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
  UFRMAccountSelect, UNode, UWizard, UWIZSendPASC, UWIZFeeOverride, UWIZPayloadOverride, UWIZSelectSignerOverride,
  UWIZSendPASC_Confirmation, UWIZModels;

type

  { TWIZSendPASC_EnterQuantity }

  TWIZSendPASC_EnterQuantity = class(TWizardForm<TWIZOperationsModel>)
    chkChooseFee: TCheckBox;
    chkAttachPayload: TCheckBox;
    chkallfunds: TCheckBox;
    edtAmt: TEdit;
    gbQuantity: TGroupBox;
    lblQuantityNotice: TLabel;
    procedure chkAttachPayloadChange(Sender: TObject);
    procedure UpdateUI();
    procedure chkallfundsChange(Sender: TObject);


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

procedure TWIZSendPASC_EnterQuantity.UpdateUI();
begin
  if chkallfunds.Checked then
  begin
    edtAmt.Text := 'ALL BALANCE';
    edtAmt.Enabled := False;
    Model.SendPASC.SendPASCMode := akaAllBalance;
  end
  else
  begin
    edtAmt.Text := TAccountComp.FormatMoney(0);
    edtAmt.Enabled := True;
    Model.SendPASC.SendPASCMode := akaSpecifiedAmount;
  end;

  Model.Payload.HasPayload := IIF(chkAttachPayload.Checked, True, False);

end;

procedure TWIZSendPASC_EnterQuantity.chkallfundsChange(Sender: TObject);
begin
  UpdateUI();
end;

procedure TWIZSendPASC_EnterQuantity.OnPresent;
begin
  UpdateUI();
end;

procedure TWIZSendPASC_EnterQuantity.chkAttachPayloadChange(Sender: TObject);
begin
  UpdateUI();
end;

procedure TWIZSendPASC_EnterQuantity.OnNext;
var
  amount: Int64;
begin
  Model.Payload.HasPayload := chkAttachPayload.Checked;
  if chkallfunds.Checked then
  begin
    Model.SendPASC.SingleAmountToSend := 0; // all balance
  end
  else
  begin
    TAccountComp.TxtToMoney(edtAmt.Text, Model.SendPASC.SingleAmountToSend);
  end;

  if chkChooseFee.Checked then
  begin
    UpdatePath(ptReplaceAllNext, [TWIZFeeOverride, TWIZSendPASC_Confirmation]);
  end
  else
  begin
    Model.Fee.SingleOperationFee := TSettings.DefaultFee;
    if Model.Payload.HasPayload then
    begin
      UpdatePath(ptReplaceAllNext, [TWIZPayloadOverride, TWIZSendPASC_Confirmation]);
    end
    else
    begin
      UpdatePath(ptReplaceAllNext, [TWIZSelectSignerOverride, TWIZSendPASC_Confirmation]);
    end;
  end;

end;

function TWIZSendPASC_EnterQuantity.Validate(out message: ansistring): boolean;
var
  amount: int64;
begin
  Result := True;
  if not chkallfunds.Checked then
  begin
    if not TAccountComp.TxtToMoney(edtAmt.Text, amount) then
    begin
      message := 'Invalid amount (' + edtAmt.Text + ')';
      Result := False;
      Exit;
    end;
  end;

end;

end.
