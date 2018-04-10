unit UWIZSelectSignerOverride;

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
  UFRMAccountSelect, UNode, UWizard, UWIZSendPASC, UWIZSendPASC_Confirmation, UWIZModels;

type

  { TWIZSelectSignerOverride }

  TWIZSelectSignerOverride = class(TWizardForm<TWIZOperationsModel>)
    cbSignerAccount: TComboBox;
    gbTransaction: TGroupBox;
    lblNote: TLabel;
    lblBalance: TLabel;
    rbPrimary: TRadioButton;
    rbSecondary: TRadioButton;
    procedure cbSignerAccountChange(Sender: TObject);



  public
    procedure OnPresent; override;
    procedure OnNext; override;
    function Validate(out message: ansistring): boolean; override;
  end;


implementation

{$R *.lfm}

uses
  UAccounts, UUserInterface, USettings;

{ TWIZSelectSignerOverride }

procedure TWIZSelectSignerOverride.cbSignerAccountChange(Sender: TObject);
begin
  if cbSignerAccount.ItemIndex < 1 then
  begin
    lblBalance.Font.Color := clRed;
    lblBalance.Caption := 'Please Select Signer Account';
  end
  else
  begin
    lblBalance.Font.Color := clGreen;
    lblBalance.Caption := Format('%s PASC',
      [TAccountComp.FormatMoney(Model.SendPASC.SelectedAccounts[PtrInt(
      cbSignerAccount.Items.Objects[cbSignerAccount.ItemIndex])].Balance)]);
  end;
end;

procedure TWIZSelectSignerOverride.OnPresent;

  function GetAccNoWithChecksum(AAccountNumber: cardinal): string;
  begin
    Result := TAccountComp.AccountNumberToAccountTxtNumber(AAccountNumber);
  end;

var
  acc: TAccount;
  accNumberwithChecksum: string;
  i: integer;
begin
  cbSignerAccount.Items.BeginUpdate;
  try
    cbSignerAccount.Items.Clear;
    cbSignerAccount.Items.Add('Select Signer Account');
    for i := Low(Model.SendPASC.SelectedAccounts) to High(Model.SendPASC.SelectedAccounts) do
    begin
      acc := Model.SendPASC.SelectedAccounts[i];
      accNumberwithChecksum := GetAccNoWithChecksum(acc.account);
      cbSignerAccount.Items.AddObject(accNumberwithChecksum, TObject(i));
    end;
  finally
    cbSignerAccount.Items.EndUpdate;
  end;
  cbSignerAccount.ItemIndex := Model.SendPASC.SelectedIndex;
  cbSignerAccountChange(Self);
end;

procedure TWIZSelectSignerOverride.OnNext;
begin
  Model.SendPASC.SelectedIndex := cbSignerAccount.ItemIndex;
  Model.Signer.SignerAccount := Model.SendPASC.SelectedAccounts[PtrInt(
    cbSignerAccount.Items.Objects[cbSignerAccount.ItemIndex])];
  if rbPrimary.Checked then
  begin
    Model.Signer.OperationSigningMode := akaPrimary;
  end
  else if rbSecondary.Checked then
  begin
    Model.Signer.OperationSigningMode := akaSecondary;
  end;
end;

function TWIZSelectSignerOverride.Validate(out message: ansistring): boolean;
begin
  Result := True;
  if cbSignerAccount.ItemIndex < 1 then
  begin
    message := 'A signer account must be selected';
    Result := False;
    Exit;
  end;

end;

end.
