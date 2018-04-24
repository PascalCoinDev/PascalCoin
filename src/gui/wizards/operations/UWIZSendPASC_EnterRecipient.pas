unit UWIZSendPASC_EnterRecipient;

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
  UFRMAccountSelect, UNode, UWizard, UWIZSendPASC, UWIZSendPASC_EnterQuantity, UCoreUtils;

type

  { TWIZSendPASC_EnterRecipient }

  TWIZSendPASC_EnterRecipient = class(TWizardForm<TExecuteOperationsModel>)
    edtDestAcc: TEdit;
    gbRecipient: TGroupBox;
    lblDestAccNumber: TLabel;
    lblDestAccNumberValue: TLabel;
    lblDestAccNumberName: TLabel;
    lblDestAccNumberNameValue: TLabel;
    lblDestNotice: TLabel;
    btnSearch: TSpeedButton;
    procedure btnSearchClick(Sender: TObject);
    procedure edtDestAccChange(Sender: TObject);
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

{ TWIZSendPASC_EnterRecipient }

procedure TWIZSendPASC_EnterRecipient.edtDestAccChange(Sender: TObject);
begin
  UpdateUI();
end;

procedure TWIZSendPASC_EnterRecipient.UpdateUI();
var
  LTempAccount: TAccount;
  LAccountNumber: cardinal;
begin
  if TAccountComp.AccountTxtNumberToAccountNumber(edtDestAcc.Text, LAccountNumber) then
  begin
    if (LAccountNumber < 0) or (LAccountNumber >= TNode.Node.Bank.AccountsCount) then
    begin
      lblDestAccNumberValue.Caption := 'Unknown';
      lblDestAccNumberNameValue.Caption := 'Unknown';
      Exit;
    end;
    LTempAccount := TNode.Node.Operations.SafeBoxTransaction.account(LAccountNumber);
    lblDestAccNumberValue.Caption := edtDestAcc.Text;
    lblDestAccNumberNameValue.Caption := LTempAccount.Name;
  end
  else
  begin
    lblDestAccNumberValue.Caption := 'Unknown';
    lblDestAccNumberNameValue.Caption := 'Unknown';
  end;

end;

procedure TWIZSendPASC_EnterRecipient.OnPresent;
begin
  UpdateUI();
  edtDestAcc.SetFocus;
end;

procedure TWIZSendPASC_EnterRecipient.btnSearchClick(Sender: TObject);
var
  LFRMAccountSelect: TFRMAccountSelect;
  LAccountNumber: cardinal;
begin
  LFRMAccountSelect := TFRMAccountSelect.Create(Self);
  LFRMAccountSelect.Position := poMainFormCenter;
  try
    LFRMAccountSelect.Node := TNode.Node;
    LFRMAccountSelect.WalletKeys := TWallet.Keys;
    LFRMAccountSelect.Filters := edtDestAcc.Tag;
    if TAccountComp.AccountTxtNumberToAccountNumber(edtDestAcc.Text, LAccountNumber) then
      LFRMAccountSelect.DefaultAccount := LAccountNumber;
    LFRMAccountSelect.AllowSelect := True;
    if LFRMAccountSelect.ShowModal = mrOk then
      edtDestAcc.Text := TAccountComp.AccountNumberToAccountTxtNumber(LFRMAccountSelect.GetSelected);
  finally
    LFRMAccountSelect.Free;
  end;
end;


procedure TWIZSendPASC_EnterRecipient.OnNext;
var
  LAccountNumber: cardinal;
  LAccount: TAccount;
begin
  TAccountComp.AccountTxtNumberToAccountNumber(edtDestAcc.Text, LAccountNumber);
  Model.SendPASC.DestinationAccount := TNode.Node.Operations.SafeBoxTransaction.account(LAccountNumber);
  UpdatePath(ptInject, [TWIZSendPASC_EnterQuantity]);

end;

function TWIZSendPASC_EnterRecipient.Validate(out message: ansistring): boolean;

  function GetAccountNumberWithChecksum(constref AAccount: TAccount): string;
  begin
    Result := AAccount.AccountString;
  end;

var
  LAccountNumbersWithChecksum: TArray<string>;
  LAccountNumber: cardinal;
begin
  Result := True;

  if not (TAccountComp.AccountTxtNumberToAccountNumber(edtDestAcc.Text, LAccountNumber)) then
  begin
    message := Format('Invalid Destination Account "%s"', [edtDestAcc.Text]);
    Result := False;
    Exit;
  end;

  if (LAccountNumber < 0) or (LAccountNumber >= TNode.Node.Bank.AccountsCount) then
  begin
    message := Format('Invalid Destination Account "%s"', [TAccountComp.AccountNumberToAccountTxtNumber(LAccountNumber)]);
    Result := False;
    Exit;
  end;

  LAccountNumbersWithChecksum :=
    TListTool<TAccount, string>.Transform(Model.Account.SelectedAccounts, GetAccountNumberWithChecksum);

  if TArrayTool<string>.Contains(LAccountNumbersWithChecksum, edtDestAcc.Text) then
  begin
    message := 'Sender And Destination Account Are Same';
    Result := False;
    Exit;
  end;

end;

end.
