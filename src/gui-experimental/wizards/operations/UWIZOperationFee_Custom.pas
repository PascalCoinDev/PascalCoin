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

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}


{$IFDEF FPC}
{$modeswitch nestedprocvars}
{$ENDIF}

{ Chooses the operation fee and, for account-information changes, an optional
  single fee payer.
}

{ Public types in this unit

  Type                    | Purpose
  ------------------------+--------------------------------------------------------------------------
  TWIZOperationFee_Custom | Validates per-operation fees and optional batch fee-payer selection.
}

interface

uses
  Types, Math, Controls, ExtCtrls,
  SysUtils, StdCtrls, {$IFDEF FPC}Spin{$ELSE}Vcl.NumberBox{$ENDIF}, UPCDataTypes, UWizard, UWIZOperation;

type

  {$IFNDEF FPC}
  TFloatSpinEdit = TNumberBox;
  {$ENDIF}

  { TWIZOperationFee_Custom }

  TWIZOperationFee_Custom = class(TWizardForm<TWIZOperationsModel>)
    cbOverrideDefaultFee: TCheckBox;
    fseFee: TFloatSpinEdit;
    gbTransactionFee: TGroupBox;
    lblestimatedfee: TLabel;
    lblPASC: TLabel;
    lblNote1: TLabel;
    lblTotalFeeValue: TLabel;
    chkSingleFeePayer: TCheckBox;
    cbFeePayer: TComboBox;
    lblFeePayerBalance: TLabel;
    procedure fseFeeChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SingleFeePayerChange(Sender: TObject);
    procedure FeePayerChange(Sender: TObject);
  private
    FUpdatingFeePayer: Boolean;
    FFeePayerRequiresChoice: Boolean;
    FFeePayerCandidates: TArray<TAccount>;
    procedure UpdateFeePayers;
    procedure UpdateUI();
    procedure SetFee(const AValue: int64);
    function GetFee: int64;
  public
    property Fee: int64 read GetFee write SetFee;
    procedure Initialize; override;
    procedure OnPresent; override;
    procedure OnNext; override;
    function Validate(out message: String): boolean; override;
  end;


implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  UAccounts,
  UNode,
  USettings,
  UCoreUtils,
  UCoreObjects,
  UWIZOperationSigner_Select,
  UWIZOperationPayload_Encryption;

{ TWIZOperationFee_Custom }

type
  TCaptionGroupBoxAccess = class(TGroupBox);

procedure TWIZOperationFee_Custom.Initialize;
begin
  Fee := TSettings.DefaultFee;
end;

procedure TWIZOperationFee_Custom.FormResize(Sender: TObject);
var
  LContent: TRect;
  LPadding: Integer;

  procedure PositionContents(AGroup: TGroupBox);
  var
    LContent: TRect;
    LPadding, LTop, LDelta, I: Integer;
    LControl: TControl;
  begin
    LContent := AGroup.ClientRect;
    TCaptionGroupBoxAccess(AGroup).AdjustClientRect(LContent);
    Canvas.Font.Assign(AGroup.Font);
    LPadding := Max(6, Canvas.TextHeight('0') * 2 div 3);
    LTop := High(Integer);
    for I := 0 to AGroup.ControlCount - 1 do
      LTop := Min(LTop, AGroup.Controls[I].Top);
    LDelta := LContent.Top + LPadding - LTop;
    for I := 0 to AGroup.ControlCount - 1 do begin
      LControl := AGroup.Controls[I];
      LControl.Top := LControl.Top + LDelta;
      if (LControl is TPanel) and (akBottom in LControl.Anchors) then
        LControl.Height := Max(0, LContent.Bottom - LControl.Top - LPadding);
    end;
  end;

begin
  if not HandleAllocated or not Assigned(cbOverrideDefaultFee) then Exit;
  // VCL reserves its caption inside ClientRect; LCL already excludes it.
  // Use the native content inset so controls stay clear at either font scale.
  PositionContents(gbTransactionFee);
  // This fixed-width value must retain the full bold font line after scaling.
  Canvas.Font.Assign(lblTotalFeeValue.Font);
  lblTotalFeeValue.Height := Max(lblTotalFeeValue.Height, Canvas.TextHeight('Mg'));
  LContent := gbTransactionFee.ClientRect;
  TCaptionGroupBoxAccess(gbTransactionFee).AdjustClientRect(LContent);
  Canvas.Font.Assign(gbTransactionFee.Font);
  LPadding := Max(6, Canvas.TextHeight('0') * 2 div 3);
  // Keep the right-aligned summary inside the group box's native border.
  lblTotalFeeValue.Left := LContent.Right - LPadding - lblTotalFeeValue.Width;
  lblestimatedfee.Left := lblTotalFeeValue.Left - LPadding - lblestimatedfee.Width;
end;

procedure TWIZOperationFee_Custom.OnPresent;
begin
  chkSingleFeePayer.Visible := Model.ExecuteOperationType = omtChangeInfo;
  cbFeePayer.Visible := chkSingleFeePayer.Visible;
  lblFeePayerBalance.Visible := chkSingleFeePayer.Visible;
  if chkSingleFeePayer.Visible then
    chkSingleFeePayer.Checked := Model.ChangeAccountInfo.UseSingleFeePayer;
  FormResize(Self);
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
  if Model.ExecuteOperationType <> omtChangeInfo then
  begin
    LAllUserAccountsExcludingPending := TCoreTool.GetUserAccounts(True);
    Model.Signer.SignerCandidates := LAllUserAccountsExcludingPending;
  end;
  Model.Fee.SingleOperationFee := Fee;

  // TODO: move this out -- inappropriate to have payload/signer considerations here

  if Model.Payload.HasPayload then
  begin
    UpdatePath(ptInject, [TWIZOperationPayload_Encryption]);
  end
  else
  begin
    case Model.ExecuteOperationType of
      omtSendPasc, omtBuyAccount, omtChangeInfo:
      begin
        // do nothing
      end;

      else
      begin
        UpdatePath(ptInject, [TWIZOperationSigner_Select]);
      end;
    end;

  end;

end;

function TWIZOperationFee_Custom.Validate(out message: String): boolean;
var
  LAcc: TAccount;
  LIdx, LCount: Integer;
begin
  Result := True;
  if Fee < 0 then
  begin
    message := 'The operation fee cannot be negative.';
    Exit(False);
  end;
  LCount := Model.Account.Count;
  if Model.ExecuteOperationType = omtChangeInfo then
    LCount := Length(Model.ChangeAccountInfo.TargetAccounts);
  if (LCount > 1) and (Fee = 0) then
  begin
    message := 'Zero fees only allowed for a single operation.';
    Exit(False);
  end;
  if (Model.ExecuteOperationType = omtChangeInfo) and chkSingleFeePayer.Checked and
    not Model.ChangeAccountInfo.FeePayerSelected then
  begin
    message := 'Select an eligible account which will pay the fee.';
    Exit(False);
  end;
  if (Model.ExecuteOperationType = omtSendPasc) and
    (Model.SendPASC.SendPASCMode = akaSpecifiedAmount) then
    for LIdx := Low(Model.Account.SelectedAccounts) to High(Model.Account.SelectedAccounts) do
    begin
      LAcc := Model.Account.SelectedAccounts[LIdx];
      if (Model.SendPASC.SingleAmountToSend < 1) or
        (LAcc.balance < UInt64(Model.SendPASC.SingleAmountToSend)) or
        (LAcc.balance - UInt64(Model.SendPASC.SingleAmountToSend) < UInt64(Fee)) then
      begin
        message := Format('Account %s cannot pay the specified amount and fee.', [TAccountComp.AccountNumberToAccountTxtNumber(LAcc.account)]);
        Exit(False);
      end;
    end;
  if Model.ExecuteOperationType = omtBuyAccount then
    for LIdx := Low(Model.Account.SelectedAccounts) to High(Model.Account.SelectedAccounts) do
    begin
      LAcc := Model.Account.SelectedAccounts[LIdx];
      if (Model.BuyAccount.Amount < 1) or
        (LAcc.balance < UInt64(Model.BuyAccount.Amount)) or
        (LAcc.balance - UInt64(Model.BuyAccount.Amount) < UInt64(Fee)) then
      begin
        message := Format('Account %s cannot pay the purchase amount and fee.', [TAccountComp.AccountNumberToAccountTxtNumber(LAcc.account)]);
        Exit(False);
      end;
    end;
end;

procedure TWIZOperationFee_Custom.SetFee(const AValue: int64);
begin
  fseFee.Value := TAccountComp.FormatMoneyDecimal(AValue);
end;

function TWIZOperationFee_Custom.GetFee: int64;
begin
  if not TAccountComp.TxtToMoney(Trim(
    {$IFDEF FPC}fseFee.ValueToStr(fseFee.Value)
    {$ELSE}FloatToStr(fseFee.Value, TFormatSettings.Invariant){$ENDIF}), Result) then
    raise Exception.Create('Invalid value in fee selector');
end;

procedure TWIZOperationFee_Custom.UpdateUI();
var
  LCount: Integer;
begin
  LCount := Model.Account.Count;
  if Model.ExecuteOperationType = omtChangeInfo then
  begin
    LCount := Length(Model.ChangeAccountInfo.TargetAccounts);
    UpdateFeePayers;
  end;
  lblTotalFeeValue.Caption := Format('%s PASC', [TAccountComp.FormatMoney(Fee * LCount)]);
end;

procedure TWIZOperationFee_Custom.UpdateFeePayers;
var
  I, LSelected: Integer;
  LHadSelection: Boolean;
begin
  if FUpdatingFeePayer then Exit;
  FUpdatingFeePayer := True;
  try
    // Only approved targets contribute to the common payer's cumulative fee.
    FFeePayerCandidates := TWIZOperationsModel.FilterSignerCandidates(
      Model.Signer.SignerCandidates, Model.ChangeAccountInfo.TargetAccounts,
      TNode.Node.Bank.BlocksCount, Fee, akaPrimary);
    LSelected := 0;
    LHadSelection := Model.ChangeAccountInfo.FeePayerSelected;
    cbFeePayer.Items.BeginUpdate;
    try
      cbFeePayer.Items.Clear;
      cbFeePayer.Items.Add('Select fee payer');
      for I := 0 to High(FFeePayerCandidates) do
      begin
        cbFeePayer.Items.Add(TAccountComp.AccountNumberToAccountTxtNumber(FFeePayerCandidates[I].account));
        if Model.ChangeAccountInfo.FeePayerSelected and
          (FFeePayerCandidates[I].account = Model.ChangeAccountInfo.FeePayerAccount.account) then
          LSelected := I + 1;
      end;
      if LHadSelection and (LSelected = 0) then
        FFeePayerRequiresChoice := True;
      // A fee increase must not silently replace a now-ineligible selection.
      if (LSelected = 0) and not FFeePayerRequiresChoice and (Length(FFeePayerCandidates) = 1) then
        LSelected := 1;
      cbFeePayer.ItemIndex := LSelected;
    finally
      cbFeePayer.Items.EndUpdate;
    end;
    cbFeePayer.Enabled := chkSingleFeePayer.Checked;
  finally
    FUpdatingFeePayer := False;
  end;
  FeePayerChange(Self);
end;

procedure TWIZOperationFee_Custom.SingleFeePayerChange(Sender: TObject);
begin
  if FUpdatingFeePayer or not Assigned(Model) or
    (Model.ExecuteOperationType <> omtChangeInfo) then Exit;
  Model.ChangeAccountInfo.UseSingleFeePayer := chkSingleFeePayer.Checked;
  UpdateFeePayers;
end;

procedure TWIZOperationFee_Custom.FeePayerChange(Sender: TObject);
var
  I: Integer;
begin
  if FUpdatingFeePayer or not Assigned(Model) or
    (Model.ExecuteOperationType <> omtChangeInfo) then Exit;
  I := cbFeePayer.ItemIndex - 1;
  Model.ChangeAccountInfo.UseSingleFeePayer := chkSingleFeePayer.Checked;
  Model.ChangeAccountInfo.FeePayerSelected := (I >= 0) and (I < Length(FFeePayerCandidates));
  if Sender = cbFeePayer then
    FFeePayerRequiresChoice := not Model.ChangeAccountInfo.FeePayerSelected;
  if Model.ChangeAccountInfo.FeePayerSelected then
    Model.ChangeAccountInfo.FeePayerAccount := FFeePayerCandidates[I];
  if not chkSingleFeePayer.Checked then
    lblFeePayerBalance.Caption := 'Each updated account pays its own fee.'
  else if Model.ChangeAccountInfo.FeePayerSelected then
    lblFeePayerBalance.Caption := Format('Balance: %s PASC',
      [TAccountComp.FormatMoney(Model.ChangeAccountInfo.FeePayerAccount.balance)])
  else
    lblFeePayerBalance.Caption := 'Select an eligible fee payer.';
end;

procedure TWIZOperationFee_Custom.fseFeeChange(Sender: TObject);
begin
  UpdateUI();
end;

end.
