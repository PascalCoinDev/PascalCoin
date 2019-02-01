unit UWIZSendPASC_Details;

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

  { TWIZSendPASC_Details }

  TWIZSendPASC_Details = class(TWizardForm<TWIZOperationsModel>)
    chkSendAll: TCheckBox;
    chkPayload: TCheckBox;
    chkCustomFee: TCheckBox;
    txtAmount: TEdit;
    txtRecipient: TEdit;
    gbRecipient: TGroupBox;
    gpAmount: TGroupBox;
    gpOptions: TGroupBox;
    lblRecipientDetails: TLabel;
    lblDestNotice: TLabel;
    btnSearch: TSpeedButton;
    lblQuantityNotice: TLabel;
    procedure btnSearchClick(Sender: TObject);
    procedure chkSendAllChange(Sender: TObject);
    procedure txtRecipientChange(Sender: TObject);
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
  UCommon,
  UMemory,
  UAccounts,
  USettings,
  UCoreUtils,
  UBaseTypes,
  UCoreObjects,
  UFRMAccountSelect,
  UCommon.Collections,
  Generics.Collections,
  UWIZOperationFee_Custom,
  UWIZOperationPayload_Encryption;

{ TWIZSendPASC_Details }

procedure TWIZSendPASC_Details.txtRecipientChange(Sender: TObject);
begin
  UpdateUI();
end;

procedure TWIZSendPASC_Details.OnPresent;
begin
  UpdateUI();

  // Recipient section
  txtRecipient.SetFocus;

  // Quantity section
  if Model.Account.Count > 1 then
  begin
    chkCustomFee.Checked := True;
    chkCustomFee.Enabled := False;
  end;
end;

procedure TWIZSendPASC_Details.UpdateUI();
var
  LTempAccount: TAccount;
  LAccountNumber: cardinal;
begin
  // Recipient section
  if TAccountComp.AccountTxtNumberToAccountNumber(txtRecipient.Text, LAccountNumber) then
    if ((LAccountNumber < 0) or (LAccountNumber >= TNode.Node.Bank.AccountsCount)) then
      lblRecipientDetails.Caption := ''
    else
    begin
      LTempAccount := TNode.Node.Operations.SafeBoxTransaction.account(LAccountNumber);
      lblRecipientDetails.Caption := LTempAccount.Name.ToPrintable;
    end;

  // Quantity section
  if chkSendAll.Checked then
  begin
    txtAmount.Text := 'ALL BALANCE';
    txtAmount.Enabled := False;
    Model.SendPASC.SendPASCMode := akaAllBalance;
  end
  else
  begin
    txtAmount.Enabled := True;
    txtAmount.Text := IIF(Model.SendPASC.SingleAmountToSend = 0, '', TAccountComp.FormatMoney(Model.SendPASC.SingleAmountToSend));
    Model.SendPASC.SendPASCMode := akaSpecifiedAmount;
  end;
end;

procedure TWIZSendPASC_Details.OnNext;
var
  LAccountNumber: cardinal;
  LAccount: TAccount;
  LWizStepsToInject: TList<TComponentClass>;
  LDisposables: TDisposables;
begin
  LWizStepsToInject := LDisposables.AddObject(TList<TComponentClass>.Create) as TList<TComponentClass>;

  // Recipient section
  TAccountComp.AccountTxtNumberToAccountNumber(txtRecipient.Text, LAccountNumber);
  Model.SendPASC.DestinationAccount := TNode.Node.Operations.SafeBoxTransaction.account(LAccountNumber);

  // Quantity section
  if chkSendAll.Checked then
  begin
    Model.SendPASC.SendPASCMode := akaAllBalance;
    Model.SendPASC.SingleAmountToSend := 0; // all balance
  end
  else
  begin
    Model.SendPASC.SendPASCMode := akaSpecifiedAmount;
    TAccountComp.TxtToMoney(txtAmount.Text, Model.SendPASC.SingleAmountToSend);
  end;

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
end;

function TWIZSendPASC_Details.Validate(out message: ansistring): boolean;

  function GetAccountNumberWithChecksum(constref AAccount: TAccount): string;
  begin
    Result := AAccount.AccountString;
  end;

var
  LAccountNumbersWithChecksum: TArray<string>;
  LAccountNumber: cardinal;
  LAmount: int64;
  LIdx: integer;
  LAccount: TAccount;
begin
  Result := True;
  // Recipient section
  if not (TAccountComp.AccountTxtNumberToAccountNumber(txtRecipient.Text, LAccountNumber)) then
  begin
    message := Format('Invalid destination account "%s"', [txtRecipient.Text]);
    Exit(False);
  end;

  if (LAccountNumber < 0) or (LAccountNumber >= TNode.Node.Bank.AccountsCount) then
  begin
    message := Format('Invalid destination account "%s"', [TAccountComp.AccountNumberToAccountTxtNumber(LAccountNumber)]);
    Exit(False);
  end;

  LAccountNumbersWithChecksum := TListTool<TAccount, string>.Transform(Model.Account.SelectedAccounts, GetAccountNumberWithChecksum);

  if TArrayTool<string>.Contains(LAccountNumbersWithChecksum, txtRecipient.Text) then
  begin
    message := 'Sender and destination account are same';
    Exit(False);
  end;

  // Quantity section
  if not chkSendAll.Checked then
  begin
    if not TAccountComp.TxtToMoney(txtAmount.Text, LAmount) then
    begin
      message := Format('Invalid quantity to send "%s"', [txtAmount.Text]);
      Exit(False);
    end;

    if LAmount < 1 then
    begin
      message := 'You Must Send An Amount Greater Than Zero.';
      Exit(False);
    end;

    for LIdx := Low(Model.Account.SelectedAccounts) to High(Model.Account.SelectedAccounts) do
    begin
      LAccount := Model.Account.SelectedAccounts[LIdx];
      if LAccount.balance < LAmount then
      begin
        message := Format('Account balance in %s (%s PASC) is less than specified amount (%s PASC) to send', [LAccount.AccountString, TAccountComp.FormatMoney(LAccount.balance), TAccountComp.FormatMoney(LAmount)]);
        Exit(False);
      end;
    end;
  end;
end;

procedure TWIZSendPASC_Details.btnSearchClick(Sender: TObject);
var
  LFRMAccountSelect: TFRMAccountSelect;
  LAccountNumber: cardinal;
begin
  LFRMAccountSelect := TFRMAccountSelect.Create(Self);
  LFRMAccountSelect.Position := poMainFormCenter;
  try
    LFRMAccountSelect.Node := TNode.Node;
    LFRMAccountSelect.WalletKeys := TWallet.Keys;
    LFRMAccountSelect.Filters := txtRecipient.Tag;
    if TAccountComp.AccountTxtNumberToAccountNumber(txtRecipient.Text, LAccountNumber) then
      LFRMAccountSelect.DefaultAccount := LAccountNumber;
    LFRMAccountSelect.AllowSelect := True;
    if LFRMAccountSelect.ShowModal = mrOk then
      txtRecipient.Text := TAccountComp.AccountNumberToAccountTxtNumber(LFRMAccountSelect.GetSelected);
  finally
    LFRMAccountSelect.Free;
  end;
end;

procedure TWIZSendPASC_Details.chkSendAllChange(Sender: TObject);
begin
  UpdateUI();
end;

end.
