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
  ExtCtrls, Buttons, Spin, UCommon, UWallet, UCoreObjects,
  UNode, UWizard, UWIZOperation, UWIZOperationPayload_Encryption, UWIZOperationSigner_Select;

type

  { TWIZOperationFee_Custom }

  TWIZOperationFee_Custom = class(TWizardForm<TWIZOperationsModel>)
    cbOverrideDefaultFee: TCheckBox;
    fseFee: TFloatSpinEdit;
    gbTransactionFee: TGroupBox;
    lblestimatedfee: TLabel;
    lblPASC: TLabel;
    lblNote1: TLabel;
    lblTotalFeeValue: TLabel;
    procedure fseFeeChange(Sender: TObject);
  private
    procedure UpdateUI();
    procedure SetFee(const AValue : int64);
    function GetFee : Int64;
  public
    property Fee : Int64 read GetFee write SetFee;
    procedure Initialize; override;
    procedure OnPresent; override;
    procedure OnNext; override;
    function Validate(out message: ansistring): boolean; override;
  end;


implementation

{$R *.lfm}

uses
  UAccounts, UCoreUtils, UUserInterface, USettings;

{ TWIZOperationFee_Custom }

procedure TWIZOperationFee_Custom.Initialize;
begin
  Fee := TSettings.DefaultFee;
end;

procedure TWIZOperationFee_Custom.OnPresent;
begin
  UpdateUI();
  fseFee.SetFocus;
end;

procedure TWIZOperationFee_Custom.OnNext;
begin
  if cbOverrideDefaultFee.Checked then begin
    TSettings.DefaultFee := Fee;
    TSettings.Save;
  end;

  Model.Fee.SingleOperationFee := Fee;
  Model.Signer.SignerCandidates := TCoreTool.GetSignerCandidates(Length(Model.Account.SelectedAccounts), Fee, Model.Account.SelectedAccounts);

  // TODO: move this out -- inappropriate to have payload/signer considerations here
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
begin
  if (Length(Model.Account.SelectedAccounts) > 1) AND (Fee = 0) then begin
    message := 'Insufficient fee for multiple operations. Zero fees only allowed for single operation.';
    Exit(false);
  end;

  if Length(TCoreTool.GetSignerCandidates(Length(Model.Account.SelectedAccounts), Model.Fee.SingleOperationFee, Model.Account.SelectedAccounts)) = 0 then begin
    message := 'No Valid Signer Account Was Found With The Current Requirements.';
    Exit(false);
  end;
end;

procedure TWIZOperationFee_Custom.SetFee(const AValue : int64);
begin
  fseFee.Value := TAccountComp.FormatMoneyDecimal(AValue);
end;

function TWIZOperationFee_Custom.GetFee : Int64;
begin
  if NOT TAccountComp.TxtToMoney(Trim(fseFee.ValueToStr(fseFee.Value)), Result) then
    raise Exception.Create('Illegal value in fee selector');
end;

procedure TWIZOperationFee_Custom.UpdateUI();
begin
  lblTotalFeeValue.Caption := Format('%s PASC', [TAccountComp.FormatMoney(Fee * Length(Model.Account.SelectedAccounts))]);
end;

procedure TWIZOperationFee_Custom.fseFeeChange(Sender: TObject);
begin
  UpdateUI();
end;

end.
