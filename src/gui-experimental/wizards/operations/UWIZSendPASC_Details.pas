unit UWIZSendPASC_Details;

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

{ Collects a transfer recipient and amount or send-all choice, with optional
  fee and payload settings.
}

{ Public types in this unit

  Type                 | Purpose
  ---------------------+--------------------------------------------------------------------------
  TWIZSendPASC_Details | Validates transfer inputs and inserts optional fee and payload steps.
}

interface

uses
  Types, Math, Controls,
  Classes, SysUtils, Forms, Dialogs, StdCtrls, ExtCtrls, Buttons, UWizard, UWIZOperation;

type

  { TWIZSendPASC_Details }

  TWIZSendPASC_Details = class(TWizardForm<TWIZOperationsModel>)
    chkSendAll: TCheckBox;
    chkPayload: TCheckBox;
    chkCustomFee: TCheckBox;
    txtAmount: TEdit;
    txtRecipient: TEdit;
    gbRecipient: TGroupBox;
    gpAmount: TGroupBox;
    gpOptions: TGroupBox;
    lblRecipientDetails: TLabel;
    lblDestNotice: TLabel;
    btnSearch: TSpeedButton;
    lblQuantityNotice: TLabel;
    procedure btnSearchClick(Sender: TObject);
    procedure chkSendAllChange(Sender: TObject);
    procedure txtRecipientChange(Sender: TObject);
    procedure UpdateUI();
    procedure FormResize(Sender: TObject);
  private
    FSpecifiedAmountText: String;
    FPresented: Boolean;
  public
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
  UNode,
  UWallet,
  UCommon,
  UMemory,
  UAccounts,
  USettings,
  UCoreUtils,
  UBaseTypes,
  UPCDataTypes,
  UCoreObjects,
  UBlockChain,
  UFRMAccountSelect,
  UCommon.Collections,
  Generics.Collections,
  UWIZOperationFee_Custom,
  UWIZOperationPayload_Encryption;

{ TWIZSendPASC_Details }

type
  TCaptionGroupBoxAccess = class(TGroupBox);

procedure TWIZSendPASC_Details.txtRecipientChange(Sender: TObject);
begin
  UpdateUI();
end;

procedure TWIZSendPASC_Details.FormResize(Sender: TObject);

  procedure FitEditText(AEdit: TEdit);
  var
    LTextHeight, LBorderHeight: Integer;
  begin
    Canvas.Font.Assign(AEdit.Font);
    LTextHeight := Canvas.TextHeight('Mg');
    LBorderHeight := Max(4, AEdit.Height - AEdit.ClientHeight);
    // The large entry font needs its full native line height plus edit borders.
    AEdit.Height := Max(AEdit.Height, LTextHeight + LBorderHeight);
  end;

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
  if not HandleAllocated or not Assigned(chkPayload) then Exit;
  FitEditText(txtRecipient);
  FitEditText(txtAmount);
  btnSearch.Height := txtRecipient.Height;
  // VCL reserves its caption inside ClientRect; LCL already excludes it.
  // Use the native content inset so controls stay clear at either font scale.
  PositionContents(gbRecipient);
  PositionContents(gpAmount);
  PositionContents(gpOptions);
end;

procedure TWIZSendPASC_Details.OnPresent;
begin
  FormResize(Self);
  if not FPresented then
  begin
    txtAmount.Text := IIF(Model.SendPASC.SingleAmountToSend = 0, '', TAccountComp.FormatMoney(Model.SendPASC.SingleAmountToSend));
    FSpecifiedAmountText := txtAmount.Text;
    FPresented := True;
  end;
  UpdateUI();

  // Recipient section
  txtRecipient.SetFocus;

  // Quantity section
  if Model.Account.Count > 1 then
  begin
    chkCustomFee.Checked := True;
    chkCustomFee.Enabled := False;
  end;
end;

procedure TWIZSendPASC_Details.UpdateUI();
var
  LTempAccount: TAccount;
  LAccountNumber: cardinal;
begin

  // Recipient section
  if TAccountComp.AccountTxtNumberToAccountNumber(txtRecipient.Text, LAccountNumber) then
    if ((LAccountNumber < 0) or (LAccountNumber >= TNode.Node.Bank.AccountsCount)) then
      lblRecipientDetails.Caption := ''
    else begin
      LTempAccount := TNode.Node.GetAccount(LAccountNumber);
      lblRecipientDetails.Caption := LTempAccount.Name.ToPrintable;
    end;

  // Quantity section
  if chkSendAll.Checked then
  begin
    if txtAmount.Enabled then
      FSpecifiedAmountText := txtAmount.Text;
    txtAmount.Text := 'ALL BALANCE';
    txtAmount.Enabled := False;
    Model.SendPASC.SendPASCMode := akaAllBalance;
  end
  else
  begin
    if not txtAmount.Enabled then
      txtAmount.Text := FSpecifiedAmountText;
    txtAmount.Enabled := True;
    Model.SendPASC.SendPASCMode := akaSpecifiedAmount;
  end;
end;

procedure TWIZSendPASC_Details.OnNext;
var
  LAccountNumber: cardinal;
  LAccount: TAccount;
  LWizStepsToInject: TList<TComponentClass>;
  LDisposables: TDisposables;
begin
  LWizStepsToInject := LDisposables.AddObject(TList<TComponentClass>.Create) as TList<TComponentClass>;

  // Recipient section
  TAccountComp.AccountTxtNumberToAccountNumber(txtRecipient.Text, LAccountNumber);
  Model.SendPASC.DestinationAccount := TNode.Node.GetAccount(LAccountNumber);

  // Quantity section
  if chkSendAll.Checked then
  begin
    Model.SendPASC.SendPASCMode := akaAllBalance;
    Model.SendPASC.SingleAmountToSend := 0; // all balance
  end
  else
  begin
    Model.SendPASC.SendPASCMode := akaSpecifiedAmount;
    TAccountComp.TxtToMoney(txtAmount.Text, Model.SendPASC.SingleAmountToSend);
  end;

  // Fee Section
  Model.Fee.SingleOperationFee := TSettings.DefaultFee;
  if chkCustomFee.Checked then
    LWizStepsToInject.Add(TWIZOperationFee_Custom);

  // Payload Section
  Model.Payload.HasPayload := IIF(chkPayload.Checked, True, False);
  if Model.Payload.HasPayload then
    LWizStepsToInject.Add(TWIZOperationPayload_Encryption);

  // Update wizard flow if applicable
  if LWizStepsToInject.Count > 0 then
    UpdatePath(ptInject, LWizStepsToInject.ToArray);
end;

function GetAccountNumberWithChecksum({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AAccount: TAccount): string;
begin
  Result := AAccount.AccountString;
end;

function TWIZSendPASC_Details.Validate(out message: String): boolean;
var
  LAccountTransform: TGlobalTransformFunc<TAccount, string>;
  LAccountNumbersWithChecksum: TArray<string>;
  LAccountNumber: cardinal;
  LAmount, LFee: int64;
  LIdx: integer;
  LAccount: TAccount;
begin
  Result := True;
  // Recipient section
  if not (TAccountComp.AccountTxtNumberToAccountNumber(txtRecipient.Text, LAccountNumber)) then
  begin
    message := Format('Invalid destination account "%s"', [txtRecipient.Text]);
    Exit(False);
  end;

  if (LAccountNumber < 0) or (LAccountNumber >= TNode.Node.Bank.AccountsCount) then
  begin
    message := Format('Invalid destination account "%s"', [TAccountComp.AccountNumberToAccountTxtNumber(LAccountNumber)]);
    Exit(False);
  end;

  LAccountTransform := GetAccountNumberWithChecksum;
  LAccountNumbersWithChecksum := TListTool<TAccount, string>.Transform(Model.Account.SelectedAccounts, LAccountTransform);

  if TArrayTool<string>.Contains(LAccountNumbersWithChecksum, txtRecipient.Text) then
  begin
    message := 'Sender and destination account are same';
    Exit(False);
  end;

  // Quantity section
  if not chkSendAll.Checked then
  begin
    if not TAccountComp.TxtToMoney(txtAmount.Text, LAmount) then
    begin
      message := Format('Invalid quantity to send "%s"', [txtAmount.Text]);
      Exit(False);
    end;

    if LAmount < 1 then
    begin
      message := 'You Must Send An Amount Greater Than Zero.';
      Exit(False);
    end;

    if chkCustomFee.Checked then
      LFee := Model.Fee.SingleOperationFee
    else
      LFee := TSettings.DefaultFee;
    if LFee < 0 then
    begin
      message := 'The operation fee cannot be negative.';
      Exit(False);
    end;
    for LIdx := Low(Model.Account.SelectedAccounts) to High(Model.Account.SelectedAccounts) do
    begin
      LAccount := Model.Account.SelectedAccounts[LIdx];
      if (LAccount.balance < UInt64(LAmount)) or
        (LAccount.balance - UInt64(LAmount) < UInt64(LFee)) then
      begin
        message := Format('Account %s needs enough balance for %s PASC and the %s PASC fee.', [LAccount.AccountString, TAccountComp.FormatMoney(LAmount), TAccountComp.FormatMoney(LFee)]);
        Exit(False);
      end;
    end;
  end;
end;

procedure TWIZSendPASC_Details.btnSearchClick(Sender: TObject);
var
  LFRMAccountSelect: TFRMAccountSelect;
  LAccountNumber: cardinal;
begin
  LFRMAccountSelect := TFRMAccountSelect.Create(Self);
  LFRMAccountSelect.Position := poMainFormCenter;
  try
    LFRMAccountSelect.Node := TNode.Node;
    LFRMAccountSelect.WalletKeys := TWallet.Keys;
    LFRMAccountSelect.Filters := txtRecipient.Tag;
    if TAccountComp.AccountTxtNumberToAccountNumber(txtRecipient.Text, LAccountNumber) then
      LFRMAccountSelect.DefaultAccount := LAccountNumber;
    LFRMAccountSelect.AllowSelect := True;
    if LFRMAccountSelect.ShowModal = mrOk then
      txtRecipient.Text := TAccountComp.AccountNumberToAccountTxtNumber(LFRMAccountSelect.GetSelected);
  finally
    LFRMAccountSelect.Free;
  end;
end;

procedure TWIZSendPASC_Details.chkSendAllChange(Sender: TObject);
begin
  UpdateUI();
end;

end.
