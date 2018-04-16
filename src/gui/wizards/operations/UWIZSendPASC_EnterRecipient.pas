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
  ExtCtrls, Buttons, UCommon, UCommon.Collections, UWallet,
  UFRMAccountSelect, UNode, UWizard, UWIZSendPASC, UWIZSendPASC_EnterQuantity,
  UWIZSendPASC_Confirmation, UWIZModels;

type

  { TWIZSendPASC_EnterRecipient }

  TWIZSendPASC_EnterRecipient = class(TWizardForm<TWIZOperationsModel>)
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
  tempAcc: TAccount;
  c: Cardinal;
begin
  if TAccountComp.AccountTxtNumberToAccountNumber(edtDestAcc.Text, c) then
  begin
    if (c < 0) or (c >= TNode.Node.Bank.AccountsCount) then
    begin
      lblDestAccNumberValue.Caption := 'unknown';
      lblDestAccNumberNameValue.Caption := 'unknown';
      Exit;
    end;
    tempAcc := TNode.Node.Operations.SafeBoxTransaction.account(c);
    lblDestAccNumberValue.Caption := edtDestAcc.Text;
    lblDestAccNumberNameValue.Caption := tempAcc.Name;
  end
  else
  begin
    lblDestAccNumberValue.Caption := 'unknown';
    lblDestAccNumberNameValue.Caption := 'unknown';
  end;

end;

procedure TWIZSendPASC_EnterRecipient.OnPresent;
begin
  UpdateUI();
  edtDestAcc.SetFocus;
end;

procedure TWIZSendPASC_EnterRecipient.btnSearchClick(Sender: TObject);
var
  F: TFRMAccountSelect;
  c: cardinal;
begin
  F := TFRMAccountSelect.Create(Self);
  F.Position := poMainFormCenter;
  try
    F.Node := TNode.Node;
    F.WalletKeys := TWallet.Keys;
    F.Filters := edtDestAcc.Tag;
    if TAccountComp.AccountTxtNumberToAccountNumber(edtDestAcc.Text, c) then
      F.DefaultAccount := c;
    F.AllowSelect := True;
    if F.ShowModal = mrOk then
    begin
      edtDestAcc.Text := TAccountComp.AccountNumberToAccountTxtNumber(F.GetSelected);
    end;
  finally
    F.Free;
  end;
end;


procedure TWIZSendPASC_EnterRecipient.OnNext;
var
  c: cardinal;
  aa: TAccount;
begin
  TAccountComp.AccountTxtNumberToAccountNumber(edtDestAcc.Text, c);
  Model.SendPASC.DestinationAccount := TNode.Node.Operations.SafeBoxTransaction.account(c);
  UpdatePath(ptReplaceAllNext, [TWIZSendPASC_EnterQuantity,
    TWIZSendPASC_Confirmation]);
end;

function TWIZSendPASC_EnterRecipient.Validate(out message: ansistring): boolean;

  function GetAccNoWithChecksum(constref AAccount: TAccount): string;
  begin
    Result := TAccountComp.AccountNumberToAccountTxtNumber(AAccount.account);
  end;

var
  AccountNumbersWithChecksum: TArray<string>;
  c: cardinal;
begin
  Result := True;

  if not (TAccountComp.AccountTxtNumberToAccountNumber(edtDestAcc.Text, c)) then
  begin
    message := 'Invalid destination account (' + edtDestAcc.Text + ')';
    Result := False;
    Exit;
  end;

  if (c < 0) or (c >= TNode.Node.Bank.AccountsCount) then
  begin
    message := 'Invalid destination account (' +
      TAccountComp.AccountNumberToAccountTxtNumber(c) + ')';
    Result := False;
    Exit;
  end;

  AccountNumbersWithChecksum :=
    TListTool<TAccount, string>.Transform(Model.Account.SelectedAccounts, GetAccNoWithCheckSum);

  if TArrayTool<string>.Contains(AccountNumbersWithChecksum, edtDestAcc.Text) then
  begin
    message := 'Sender and destination account are the same';
    Result := False;
    Exit;
  end;

end;

end.
