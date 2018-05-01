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
  UFRMAccountSelect, UNode,
  UWizard, UWIZOperation, UWIZSendPASC, UWIZOperationFee_Custom, UWIZOperationPayload_Encryption,
  UWIZOperationSigner_Select, UCoreObjects;

type

  { TWIZSendPASC_EnterQuantity }

  TWIZSendPASC_EnterQuantity = class(TWizardForm<TWIZOperationsModel>)
    chkChooseFee: TCheckBox;
    chkAttachPayload: TCheckBox;
    chkallfunds: TCheckBox;
    edtAmt: TEdit;
    gbQuantity: TGroupBox;
    lblQuantityNotice: TLabel;
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
    edtAmt.Text := '';
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
  if Length(Model.Account.SelectedAccounts) > 1 then
  begin
    chkChooseFee.Checked := True;
    chkChooseFee.Enabled := False;
  end;
  if edtAmt.Enabled then
    edtAmt.SetFocus;
end;

procedure TWIZSendPASC_EnterQuantity.OnNext;
begin
  Model.Payload.HasPayload := chkAttachPayload.Checked;
  if chkallfunds.Checked then
    Model.SendPASC.SingleAmountToSend := 0 // all balance

  else
    TAccountComp.TxtToMoney(edtAmt.Text, Model.SendPASC.SingleAmountToSend);

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

function TWIZSendPASC_EnterQuantity.Validate(out message: ansistring): boolean;
var
  LAmount: int64;
  LIdx: integer;
  LAccount: TAccount;
begin
  Result := True;
  if not chkallfunds.Checked then
  begin
    if not TAccountComp.TxtToMoney(edtAmt.Text, LAmount) then
    begin
      message := Format('Invalid Amount "%s"', [edtAmt.Text]);
      Result := False;
      Exit;
    end;

    if LAmount < 1 then
    begin
      message := 'You Must Send An Amount Greater Than Zero.';
      Result := False;
      Exit;
    end;

    for LIdx := Low(Model.Account.SelectedAccounts) to High(Model.Account.SelectedAccounts) do
    begin
      LAccount := Model.Account.SelectedAccounts[LIdx];
      if LAccount.balance < LAmount then
      begin
        message := 'Insufficient Funds In One Or More Accounts.';
        Result := False;
        Exit;
      end;
    end;

  end;

end;

end.
