unit UWIZOperationFee_Custom;

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
  SysUtils, StdCtrls, Spin, UWizard, UWIZOperation;

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
    procedure SetFee(const AValue: int64);
    function GetFee: int64;
  public
    property Fee: int64 read GetFee write SetFee;
    procedure Initialize; override;
    procedure OnPresent; override;
    procedure OnNext; override;
    function Validate(out message: ansistring): boolean; override;
  end;


implementation

{$R *.lfm}

uses
  UAccounts,
  USettings,
  UCoreUtils,
  UCoreObjects,
  UWIZOperationSigner_Select,
  UWIZOperationPayload_Encryption;

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
var
  LAllUserAccountsExcludingPending: TArray<TAccount>;
begin
  if cbOverrideDefaultFee.Checked then
  begin
    TSettings.DefaultFee := Fee;
    TSettings.Save;
  end;
  LAllUserAccountsExcludingPending := TCoreTool.GetUserAccounts(False);
  Model.Signer.SignerCandidates := TCoreTool.GetSignerCandidates(Model.Account.Count, Fee, LAllUserAccountsExcludingPending);
  Model.Fee.SingleOperationFee := Fee;

  // TODO: move this out -- inappropriate to have payload/signer considerations here

  if Model.Payload.HasPayload then
  begin
    UpdatePath(ptInject, [TWIZOperationPayload_Encryption]);
  end
  else
  begin
    case Model.ExecuteOperationType of
      omtSendPasc, omtBuyAccount:
      begin
        // do nothing
      end;

      omtChangeInfo:
      begin
        if Model.Account.Count = 1 then
        begin
          UpdatePath(ptInject, [TWIZOperationSigner_Select]);
        end;
      end
      else
      begin
        UpdatePath(ptInject, [TWIZOperationSigner_Select]);
      end;
    end;

  end;

end;

function TWIZOperationFee_Custom.Validate(out message: ansistring): boolean;
var
  LAcc: TAccount;
begin
  Result := True;
  if (Model.Account.Count > 1) and (Fee = 0) then
  begin
    message := 'Zero fees only allowed for a single operation.';
    Exit(False);
  end;
end;

procedure TWIZOperationFee_Custom.SetFee(const AValue: int64);
begin
  fseFee.Value := TAccountComp.FormatMoneyDecimal(AValue);
end;

function TWIZOperationFee_Custom.GetFee: int64;
begin
  if not TAccountComp.TxtToMoney(Trim(fseFee.ValueToStr(fseFee.Value)), Result) then
    raise Exception.Create('Invalid value in fee selector');
end;

procedure TWIZOperationFee_Custom.UpdateUI();
begin
  lblTotalFeeValue.Caption := Format('%s PASC', [TAccountComp.FormatMoney(Fee * Model.Account.Count)]);
end;

procedure TWIZOperationFee_Custom.fseFeeChange(Sender: TObject);
begin
  UpdateUI();
end;

end.
