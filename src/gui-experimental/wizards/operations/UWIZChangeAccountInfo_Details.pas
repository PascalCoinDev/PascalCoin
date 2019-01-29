unit UWIZChangeAccountInfo_Details;

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

  { TWIZChangeAccountInfo_Details }

  TWIZChangeAccountInfo_Details = class(TWizardForm<TWIZOperationsModel>)
    chkPayload: TCheckBox;
    chkCustomFee: TCheckBox;
    lblTypeDetails: TLabel;
    txtType: TEdit;
    txtName: TEdit;
    gbName: TGroupBox;
    gpType: TGroupBox;
    gpOptions: TGroupBox;
    lblNameDetails: TLabel;
    lblNameNotice: TLabel;
    lblTypeNotice: TLabel;
    procedure txtNameChange(Sender: TObject);
    procedure txtTypeChange(Sender: TObject);
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
  UWIZOperationSigner_Select,
  UWIZOperationPayload_Encryption;

{ TWIZChangeAccountInfo_Details }

procedure TWIZChangeAccountInfo_Details.txtNameChange(Sender: TObject);
begin
  UpdateUI();
end;

procedure TWIZChangeAccountInfo_Details.txtTypeChange(Sender: TObject);
begin
  UpdateUI();
end;

procedure TWIZChangeAccountInfo_Details.OnPresent;
begin
  UpdateUI();
  txtType.Text := '0';

  if Model.Account.Count > 1 then
  begin
    gbName.Enabled := False;
  end
  else
  begin
    txtName.SetFocus;
  end;
end;

procedure TWIZChangeAccountInfo_Details.UpdateUI();
begin
  if txtName.Enabled then
  begin
    lblNameDetails.Caption := txtName.Text;
  end;
  lblTypeDetails.Caption := txtType.Text;
end;

procedure TWIZChangeAccountInfo_Details.OnNext;
var
  LWizStepsToInject: TList<TComponentClass>;
  LDisposables: TDisposables;
begin
  LWizStepsToInject := LDisposables.AddObject(TList<TComponentClass>.Create) as TList<TComponentClass>;

  // Fee Section
  Model.Fee.SingleOperationFee := TSettings.DefaultFee;
  if chkCustomFee.Checked then
    LWizStepsToInject.Add(TWIZOperationFee_Custom);

  // Payload Section
  Model.Payload.HasPayload := IIF(chkPayload.Checked, True, False);
  if Model.Payload.HasPayload then
    LWizStepsToInject.Add(TWIZOperationPayload_Encryption);

  // Signer section
  if Model.Account.Count = 1 then
  begin
    LWizStepsToInject.Add(TWIZOperationSigner_Select); // special case for changeaccount info wizard
  end;

  // Update wizard flow if applicable
  if LWizStepsToInject.Count > 0 then
    UpdatePath(ptInject, LWizStepsToInject.ToArray);
end;

function TWIZChangeAccountInfo_Details.Validate(out message: ansistring): boolean;
var
  LAccNumberIndex, LIdx, LErrCode: integer;
  LCurrentAccount: TAccount;
  LNewName: TRawBytes;
  LNewType: word;
begin
  Result := True;

  for LIdx := Low(Model.Account.SelectedAccounts) to High(Model.Account.SelectedAccounts) do
  begin
    LCurrentAccount := Model.Account.SelectedAccounts[LIdx];

    Val(Trim(txtType.Text), LNewType, LErrCode);
    if LErrCode > 0 then
    begin
      message := Format('New type "%s" is not a valid numeric value', [txtType.Text]);
      Exit(False);
    end;

    // New name (only for single operations)
    if Model.Account.Count = 1 then
    begin
      LNewName.FromString(LowerCase(Trim(txtName.Text)));

      if not TBaseType.Equals(LNewName, LCurrentAccount.Name) then
      begin
        if LNewName <> nil then
        begin
          if (not TPCSafeBox.ValidAccountName(LNewName, message)) then
          begin
            message := Format('New name "%s" is not a valid name: %s ', [LNewName.ToPrintable, message]);
            Exit(False);
          end;
          LAccNumberIndex := (TNode.Node.Bank.SafeBox.FindAccountByName(LNewName));
          if (LAccNumberIndex >= 0) then
          begin
            message := Format('Name "%s" is used by account %s ', [LNewName.ToPrintable, TAccountComp.AccountNumberToAccountTxtNumber(LAccNumberIndex)]);
            Exit(False);
          end;
        end;
      end;

      if (TBaseType.Equals(LNewName, LCurrentAccount.Name)) and (LNewType = LCurrentAccount.account_type) then
      begin
        message := 'New account name and type are same as former.';
        Exit(False);
      end;

    end;

  end;

  Model.ChangeAccountInfo.NewName := LNewName;
  Model.ChangeAccountInfo.NewType := LNewType;
end;

end.
