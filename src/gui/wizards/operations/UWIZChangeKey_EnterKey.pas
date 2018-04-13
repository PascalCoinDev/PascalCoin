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
  UFRMAccountSelect, UNode, UWizard, UWIZFeeOverride, UWIZPayloadOverride, UWIZSelectSignerOverride,
  UWIZChangeKey_Confirmation, UWIZModels;

type

  { TWIZChangeKey_EnterKey }

  TWIZChangeKey_EnterKey = class(TWizardForm<TWIZOperationsModel>)
    chkChooseFee: TCheckBox;
    chkAttachPayload: TCheckBox;
    gbNewPublicKey: TGroupBox;
    lblPublicKeyNotice: TLabel;
    mmoNewPrivateKey: TMemo;

  public
    procedure OnNext; override;
    function Validate(out message: ansistring): boolean; override;
  end;


implementation

{$R *.lfm}

uses
  UAccounts, UUserInterface, USettings;

{ TWIZChangeKey_EnterKey }

procedure TWIZChangeKey_EnterKey.OnNext;
begin
  Model.Payload.HasPayload := chkAttachPayload.Checked;

  if chkChooseFee.Checked then
  begin
    UpdatePath(ptReplaceAllNext, [TWIZFeeOverride, TWIZChangeKey_Confirmation]);
  end
  else
  begin
    Model.Fee.SingleOperationFee := TSettings.DefaultFee;
    if Model.Payload.HasPayload then
    begin
      UpdatePath(ptReplaceAllNext, [TWIZPayloadOverride, TWIZChangeKey_Confirmation]);
    end
    else
    begin
      UpdatePath(ptReplaceAllNext, [TWIZSelectSignerOverride, TWIZChangeKey_Confirmation]);
    end;
  end;
end;

function TWIZChangeKey_EnterKey.Validate(out message: ansistring): boolean;
var
  tempAccountKey: TAccountKey;
  i: Integer;
begin
  Result := True;
  if not TAccountComp.AccountKeyFromImport(mmoNewPrivateKey.Lines.Text,
    tempAccountKey, message) then
  begin
    Result := False;
    Exit;
  end;
  for i := Low(Model.ChangeKey.SelectedAccounts) to High(Model.ChangeKey.SelectedAccounts) do
  begin
    if TAccountComp.EqualAccountKeys(Model.ChangeKey.SelectedAccounts[i].accountInfo.accountKey,
      tempAccountKey) then
    begin
      Result := False;
      message := 'New key is same as current key';
      Exit;
    end;
  end;
  Model.TransferAccount.AccountKey := tempAccountKey;
end;

end.
