unit UWIZOperationFee_Custom;

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
  ExtCtrls, Buttons, Spin, UCommon, UCommon.Collections, UWallet, UCoreObjects,
  UFRMAccountSelect, UNode, UWizard, UWIZOperationPayload_Encryption, UWIZOperationSigner_Select;

type

  { TWIZOperationFee_Custom }

  TWIZOperationFee_Custom = class(TWizardForm<TExecuteOperationsModel>)
    fseFee: TFloatSpinEdit;
    gbTransactionFee: TGroupBox;
    lblestimatedfee: TLabel;
    lblPASC: TLabel;
    lblNote1: TLabel;
    lblNote2: TLabel;
    lblTotalFeeValue: TLabel;
    procedure UpdateUI();
    procedure fseFeeChange(Sender: TObject);


  public
    procedure OnPresent; override;
    procedure OnNext; override;
    function Validate(out message: ansistring): boolean; override;
  end;


implementation

{$R *.lfm}

uses
  UAccounts, UCoreUtils, UUserInterface, USettings;

{ TWIZOperationFee_Custom }

procedure TWIZOperationFee_Custom.UpdateUI();
var
  LOperationFee: int64;
begin
  TAccountComp.TxtToMoney(Trim(fseFee.ValueToStr(fseFee.Value)), LOperationFee);
  lblTotalFeeValue.Caption := Format('%s PASC', [TAccountComp.FormatMoney(LOperationFee * Length(Model.Account.SelectedAccounts))]);
end;

procedure TWIZOperationFee_Custom.fseFeeChange(Sender: TObject);
begin
  UpdateUI();
end;

procedure TWIZOperationFee_Custom.OnPresent;
begin
  UpdateUI();
  fseFee.SetFocus;
end;

procedure TWIZOperationFee_Custom.OnNext;
begin
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

function TWIZOperationFee_Custom.Validate(out message: ansistring): boolean;
var
  LOperationFee: int64;
  LAccount: TAccount;
begin
  Result := True;

  if not TAccountComp.TxtToMoney(Trim(fseFee.ValueToStr(fseFee.Value)), LOperationFee) then
  begin
    message := Format('Invalid Fee Value "%s"', [fseFee.ValueToStr(fseFee.Value)]);
    Result := False;
    Exit;
  end;

  Model.Fee.SingleOperationFee := LOperationFee;

  if Length(Model.Account.SelectedAccounts) > 1 then
    if not (Model.Fee.SingleOperationFee > 0) then
    begin
      message := 'Zero Fee Is Only Allowed For Single Operations.';
      Result := False;
      Exit;
    end;


  // get signer accounts from selected accounts
  Model.Signer.SignerCandidates := TCoreTool.GetSignerCandidates(Length(Model.Account.SelectedAccounts), Model.Fee.SingleOperationFee, Model.Account.SelectedAccounts);

  if Length(Model.Signer.SignerCandidates) < 1 then
  begin
    message := 'No Valid Signer Account Was Found With The Current Requirements.';
    Result := False;
    Exit;
  end;

end;

end.
