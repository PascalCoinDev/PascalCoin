unit UWIZChangeKey_EnterKey;

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
  SysUtils, StdCtrls, UWizard, UWIZOperation;

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
  UAccounts,
  USettings,
  UCoreObjects,
  UWIZOperationFee_Custom,
  UWIZOperationSigner_Select,
  UWIZOperationPayload_Encryption;

{ TWIZChangeKey_EnterKey }

procedure TWIZChangeKey_EnterKey.OnPresent;
begin
  if Model.Account.Count > 1 then
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
    else
    begin
      UpdatePath(ptInject, [TWIZOperationSigner_Select]);
    end;
  end;

end;

function TWIZChangeKey_EnterKey.Validate(out message: ansistring): boolean;
var
  LTempAccountKey: TAccountKey;
  LIdx: integer;
begin
  Result := True;
  if not TAccountComp.AccountKeyFromImport(Trim(mmoNewPrivateKey.Lines.Text),
    LTempAccountKey, message) then
  begin
    Exit(False);
  end;
  for LIdx := Low(Model.Account.SelectedAccounts) to High(Model.Account.SelectedAccounts) do
    if TAccountComp.EqualAccountKeys(Model.Account.SelectedAccounts[LIdx].accountInfo.accountKey,
      LTempAccountKey) then
    begin
      message := 'New Key Is Same As Current Key';
      Exit(False);
    end;

  Model.TransferAccount.AccountKey := LTempAccountKey;
end;

end.
