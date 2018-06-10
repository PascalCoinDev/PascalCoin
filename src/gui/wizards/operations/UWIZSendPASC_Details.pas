unit UWIZSendPASC_Details;

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
  ExtCtrls, Buttons, UCommon, UCommon.Collections, UWallet, UCoreObjects,
  UFRMAccountSelect, UNode, UWizard, UWIZOperation, UWIZSendPASC, UCoreUtils;

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
  Generics.Collections, UMemory,
  UAccounts, UUserInterface, USettings,
  UWIZOperationFee_Custom, UWIZOperationPayload_Encryption, UWIZOperationSigner_Select;

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
  if Length(Model.Account.SelectedAccounts) > 1 then begin
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
  else begin
    LTempAccount := TNode.Node.Operations.SafeBoxTransaction.account(LAccountNumber);
    lblRecipientDetails.Caption := LTempAccount.name;
  end;

  // Quantity section
  if chkSendAll.Checked then begin
    txtAmount.Text := 'ALL BALANCE';
    txtAmount.Enabled := False;
    Model.SendPASC.SendPASCMode := akaAllBalance;
  end else begin
    txtAmount.Enabled := True;
    txtAmount.Text := IIF(Model.SendPASC.SingleAmountToSend = 0, '', TAccountComp.FormatMoney(Model.SendPASC.SingleAmountToSend));
    Model.SendPASC.SendPASCMode := akaSpecifiedAmount;
  end;
end;

procedure TWIZSendPASC_Details.OnNext;
var
  LAccountNumber: cardinal;
  LAccount: TAccount;
  LWizStepsToInject : TList<TComponentClass>;
  LDisposables : TDisposables;
begin
  LWizStepsToInject := LDisposables.AddObject( TList<TComponentClass>.Create ) as TList<TComponentClass>;

  // Recipient section
  TAccountComp.AccountTxtNumberToAccountNumber(txtRecipient.Text, LAccountNumber);
  Model.SendPASC.DestinationAccount := TNode.Node.Operations.SafeBoxTransaction.account(LAccountNumber);

  // Quantity section
  if chkSendAll.Checked then begin
    Model.SendPASC.SendPASCMode := akaAllBalance;
    Model.SendPASC.SingleAmountToSend := 0 // all balance
  end else begin
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

  // Signer section
  if Length(Model.Account.SelectedAccounts) > 1 then
    LWizStepsToInject.Add(TWIZOperationSigner_Select)
  else begin
    Model.Signer.SignerAccount := Model.Account.SelectedAccounts[0];
    Model.Signer.OperationSigningMode := akaPrimary;
  end;

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
  message := '';
  // Recipient section
  if not (TAccountComp.AccountTxtNumberToAccountNumber(txtRecipient.Text, LAccountNumber)) then begin
    message := message + Format('Invalid Destination Account "%s"', [txtRecipient.Text]) + LineEnding;
    Result := False;
    Exit;
  end;

  if (LAccountNumber < 0) or (LAccountNumber >= TNode.Node.Bank.AccountsCount) then begin
    message := message + Format('Invalid Destination Account "%s"', [TAccountComp.AccountNumberToAccountTxtNumber(LAccountNumber)]) + LineEnding;
    Result := False;
    Exit;
  end;

  LAccountNumbersWithChecksum := TListTool<TAccount, string>.Transform(Model.Account.SelectedAccounts, GetAccountNumberWithChecksum);

  if TArrayTool<string>.Contains(LAccountNumbersWithChecksum, txtRecipient.Text) then begin
    message := message + 'Sender And Destination Account Are Same' + LineEnding;
    Result := False;
    Exit;
  end;

  // Quantity section
  if not chkSendAll.Checked then begin
    if not TAccountComp.TxtToMoney(txtAmount.Text, LAmount) then  begin
      message := message + Format('Invalid quantity to send "%s"', [txtAmount.Text]) + LineEnding;
      Result := False;
      Exit;
    end;

    if LAmount < 1 then begin
      message := message + 'You Must Send An Amount Greater Than Zero.' + LineEnding;
      Result := False;
      Exit;
    end;

    for LIdx := Low(Model.Account.SelectedAccounts) to High(Model.Account.SelectedAccounts) do begin
      LAccount := Model.Account.SelectedAccounts[LIdx];
      if LAccount.balance < LAmount then begin
        message := message + 'Insufficient Funds In One Or More Accounts.' + LineEnding;
        Result := False;
        Exit;
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
