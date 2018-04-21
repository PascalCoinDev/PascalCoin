unit UWIZChangeKey_EnterKey;

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
  UFRMAccountSelect, UNode, UWizard, UWIZOperationFee_Custom, UWIZOperationPayload_Encryption, UWIZOperationSigner_Select,
  UWIZModels;

type

  { TWIZChangeKey_EnterKey }

  TWIZChangeKey_EnterKey = class(TWizardForm<TWIZOperationsModel>)
    chkChooseFee: TCheckBox;
    chkAttachPayload: TCheckBox;
    gbNewPublicKey: TGroupBox;
    lblPublicKeyNotice: TLabel;
    mmoNewPrivateKey: TMemo;

  public
    procedure OnPresent; override;
    procedure OnNext; override;
    function Validate(out message: ansistring): boolean; override;
  end;


implementation

{$R *.lfm}

uses
  UAccounts, UUserInterface, USettings;

{ TWIZChangeKey_EnterKey }

procedure TWIZChangeKey_EnterKey.OnPresent;
begin
  if Length(Model.Account.SelectedAccounts) > 1 then
  begin
    chkChooseFee.Checked := True;
    chkChooseFee.Enabled := False;
  end;
  mmoNewPrivateKey.Clear;
  mmoNewPrivateKey.SetFocus;
end;

procedure TWIZChangeKey_EnterKey.OnNext;
begin

  Model.Payload.HasPayload := chkAttachPayload.Checked;

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

function TWIZChangeKey_EnterKey.Validate(out message: ansistring): boolean;
var
  tempAccountKey: TAccountKey;
  i: integer;
begin
  Result := True;
  if not TAccountComp.AccountKeyFromImport(Trim(mmoNewPrivateKey.Lines.Text),
    tempAccountKey, message) then
  begin
    Result := False;
    Exit;
  end;
  for i := Low(Model.Account.SelectedAccounts) to High(Model.Account.SelectedAccounts) do
    if TAccountComp.EqualAccountKeys(Model.Account.SelectedAccounts[i].accountInfo.accountKey,
      tempAccountKey) then
    begin
      Result := False;
      message := 'New key is same as current key';
      Exit;
    end;

  Model.TransferAccount.AccountKey := tempAccountKey;
end;

end.
