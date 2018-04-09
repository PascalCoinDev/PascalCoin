unit UWIZFeeOverride;

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
  ExtCtrls, Buttons, Spin, UCommon, UCommon.Collections, UWallet,
  UFRMAccountSelect, UNode, UWizard, UWIZSendPASC, UWIZPayloadOverride, UWIZSelectSignerOverride, UWIZSendPASC_Confirmation, UWIZModels;

type

  { TWIZFeeOverride }

  TWIZFeeOverride = class(TWizardForm<TWIZOperationsModel>)
    fseFee: TFloatSpinEdit;
    gbTransactionFee: TGroupBox;
    lbltotalfee: TLabel;
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
  UAccounts, UUserInterface, USettings;

{ TWIZFeeOverride }

procedure TWIZFeeOverride.UpdateUI();
var
  opfee: int64;
begin
  TAccountComp.TxtToMoney(Trim(fseFee.ValueToStr(fseFee.Value)), opfee);
  lblTotalFeeValue.Caption := Format('%s PASC', [TAccountComp.FormatMoney(opfee *
    Length(Model.SendPASCModel.SelectedAccounts))]);
end;

procedure TWIZFeeOverride.fseFeeChange(Sender: TObject);
begin
  UpdateUI();
end;

procedure TWIZFeeOverride.OnPresent;
begin
  UpdateUI();
end;

procedure TWIZFeeOverride.OnNext;
begin
  TAccountComp.TxtToMoney(Trim(fseFee.ValueToStr(fseFee.Value)),
    Model.SendPASCModel.SingleOperationFee);
  if Model.SendPASCModel.HasPayload then
  begin
    UpdatePath(ptReplaceAllNext, [TWIZPayloadOverride,
      TWIZSendPASC_Confirmation]);
  end
  else
  begin
    UpdatePath(ptReplaceAllNext, [TWIZSelectSignerOverride,
      TWIZSendPASC_Confirmation]);
  end;
end;

function TWIZFeeOverride.Validate(out message: ansistring): boolean;
var
  opfee: int64;
begin
  Result := True;

  if not TAccountComp.TxtToMoney(Trim(fseFee.ValueToStr(fseFee.Value)), opfee) then
  begin
    message := 'Invalid fee value "' + fseFee.ValueToStr(fseFee.Value) + '"';
    Result := False;
    Exit;
  end;

end;

end.
