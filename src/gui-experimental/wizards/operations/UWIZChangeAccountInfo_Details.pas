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

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}


{$IFDEF FPC}
{$modeswitch nestedprocvars}
{$ENDIF}

interface

uses
  Types, Math, Classes, SysUtils, Forms, Dialogs, Controls, StdCtrls, ExtCtrls, Buttons, UWizard, UWIZOperation;

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
    procedure FormResize(Sender: TObject);
  private
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
  UPCDataTypes,
  USettings,
  UCoreUtils,
  UBaseTypes,
  UCoreObjects,
  UFRMAccountSelect,
  UCommon.Collections,
  Generics.Collections,
  UWIZOperationFee_Custom,
  UWIZOperationPayload_Encryption,
  UWIZOperationConfirmation,
  UWIZChangeAccountInfo_Skipped;

{ TWIZChangeAccountInfo_Details }

type
  TCaptionGroupBoxAccess = class(TGroupBox);

procedure TWIZChangeAccountInfo_Details.FormResize(Sender: TObject);

  procedure FitEditText(AEdit: TEdit);
  var
    LTextHeight, LBorderHeight: Integer;
  begin
    Canvas.Font.Assign(AEdit.Font);
    LTextHeight := Canvas.TextHeight('Mg');
    LBorderHeight := Max(4, AEdit.Height - AEdit.ClientHeight);
    // Preserve the large font while fitting its native line and edit borders.
    AEdit.Height := Max(AEdit.Height, LTextHeight + LBorderHeight);
  end;

  procedure PositionContents(AGroup: TGroupBox);
  var
    LContent: TRect;
    LPadding, LTop, LDelta, I: Integer;
  begin
    LContent := AGroup.ClientRect;
    TCaptionGroupBoxAccess(AGroup).AdjustClientRect(LContent);
    Canvas.Font.Assign(AGroup.Font);
    LPadding := Max(6, Canvas.TextHeight('0') * 2 div 3);
    LTop := High(Integer);
    for I := 0 to AGroup.ControlCount - 1 do
      LTop := Min(LTop, AGroup.Controls[I].Top);
    LDelta := LContent.Top + LPadding - LTop;
    for I := 0 to AGroup.ControlCount - 1 do
      AGroup.Controls[I].Top := AGroup.Controls[I].Top + LDelta;
  end;

begin
  if not HandleAllocated or not Assigned(chkPayload) then Exit;
  FitEditText(txtName);
  FitEditText(txtType);
  // VCL includes the caption in ClientRect; LCL already excludes it. Use each
  // toolkit's native content inset, including after a font/DPI change.
  PositionContents(gbName);
  PositionContents(gpType);
  PositionContents(gpOptions);
end;

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
  FormResize(Self);
  if not FPresented then
  begin
    if Model.Account.Count = 1 then
    begin
      txtName.Text := Model.Account.SelectedAccounts[0].name.ToPrintable;
      txtType.Text := IntToStr(Model.Account.SelectedAccounts[0].account_type);
    end
    else
      txtType.Text := IntToStr(Model.ChangeAccountInfo.NewType);
    FPresented := True;
  end;
  UpdateUI();

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

  // Rebuild the complete continuation after every Details validation. Going
  // back and changing the type must not reuse an old skip list or its approval.
  Model.ChangeAccountInfo.TargetAccounts := TWIZOperationsHelper.GetChangeAccountInfoTargets(
    Model.Account.SelectedAccounts, Model.ChangeAccountInfo.NewType);
  Model.ChangeAccountInfo.SkippedCount := Model.Account.Count - Length(Model.ChangeAccountInfo.TargetAccounts);
  Model.ChangeAccountInfo.SkippedAccountsApproved := False;
  if Model.ChangeAccountInfo.SkippedCount > 0 then
    LWizStepsToInject.Add(TWIZChangeAccountInfo_Skipped);

  // Fee Section
  Model.Fee.SingleOperationFee := TSettings.DefaultFee;
  if chkCustomFee.Checked then
    LWizStepsToInject.Add(TWIZOperationFee_Custom)
  else
    Model.ChangeAccountInfo.UseSingleFeePayer := False;

  // Payload Section
  Model.Payload.HasPayload := IIF(chkPayload.Checked, True, False);
  if Model.Payload.HasPayload then
    LWizStepsToInject.Add(TWIZOperationPayload_Encryption);

  LWizStepsToInject.Add(TWIZOperationConfirmation);
  UpdatePath(ptReplaceAllNext, LWizStepsToInject.ToArray);
end;

function TWIZChangeAccountInfo_Details.Validate(out message: String): boolean;
var
  LMessage: String;
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
          if (not TPascalCoinProtocol.IsValidAccountName(TNode.Node.Bank.SafeBox.CurrentProtocol, LNewName, LMessage)) then
          begin
            message := Format('New name "%s" is not a valid name: %s ', [LNewName.ToPrintable, LMessage]);
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
