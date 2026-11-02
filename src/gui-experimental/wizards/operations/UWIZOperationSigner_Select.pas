unit UWIZOperationSigner_Select;

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
  SysUtils, Graphics, StdCtrls, ExtCtrls, UCommon, UWizard, UWIZOperation, UPCDataTypes;

type

  { TWIZOperationSigner_Select }

  TWIZOperationSigner_Select = class(TWizardForm<TWIZOperationsModel>)
    cbSignerAccount: TComboBox;
    gbTransaction: TGroupBox;
    lblNote: TLabel;
    lblBalance: TLabel;
    rbPrimary: TRadioButton;
    rbSecondary: TRadioButton;
    procedure cbSignerAccountChange(Sender: TObject);
    procedure SigningModeChange(Sender: TObject);
  private
    FCandidates: TArray<TAccount>;
    procedure UpdateSignerCandidates;
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
  UAccounts,
  UNode,
  UCoreObjects;

{ TWIZOperationSigner_Select }

procedure TWIZOperationSigner_Select.cbSignerAccountChange(Sender: TObject);
begin
  if cbSignerAccount.ItemIndex < 1 then
  begin
    lblBalance.Font.Color := clRed;
    lblBalance.Caption := 'Please Select Signer Account';
  end
  else
  begin
    lblBalance.Font.Color := clGreen;
    lblBalance.Caption := Format('%s PASC',
      [TAccountComp.FormatMoney(FCandidates[PtrInt(
      cbSignerAccount.Items.Objects[cbSignerAccount.ItemIndex])].Balance)]);
  end;
end;

procedure TWIZOperationSigner_Select.OnPresent;
begin
  rbPrimary.Checked := Model.Signer.OperationSigningMode = akaPrimary;
  rbSecondary.Checked := Model.Signer.OperationSigningMode = akaSecondary;
  UpdateSignerCandidates;
  cbSignerAccount.SetFocus;
end;

procedure TWIZOperationSigner_Select.SigningModeChange(Sender: TObject);
begin
  if Assigned(Model) then
    UpdateSignerCandidates;
end;

procedure TWIZOperationSigner_Select.UpdateSignerCandidates;

  function GetAccountNumberWithChecksum(AAccountNumber: cardinal): string;
  begin
    Result := TAccountComp.AccountNumberToAccountTxtNumber(AAccountNumber);
  end;

var
  LAccount: TAccount;
  LAccountNumberWithChecksum: string;
  LIdx: integer;
  LMode: TOperationSigningMode;
begin
  if rbSecondary.Checked then
    LMode := akaSecondary
  else
    LMode := akaPrimary;
  FCandidates := TWIZOperationsModel.FilterSignerCandidates(Model.Signer.SignerCandidates,
    Model.Account.SelectedAccounts, TNode.Node.Bank.BlocksCount,
    Model.Fee.SingleOperationFee, LMode);
  cbSignerAccount.Items.BeginUpdate;
  try
    cbSignerAccount.Items.Clear;
    cbSignerAccount.Items.Add('Select Signer Account');
    for LIdx := Low(FCandidates) to High(FCandidates) do
    begin
      LAccount := FCandidates[LIdx];
      LAccountNumberWithChecksum := GetAccountNumberWithChecksum(LAccount.account);
      cbSignerAccount.Items.AddObject(LAccountNumberWithChecksum, TObject(LIdx));
    end;
  finally
    cbSignerAccount.Items.EndUpdate;
  end;
  if Length(FCandidates) = 1 then
    cbSignerAccount.ItemIndex := 1
  else
  begin
    cbSignerAccount.ItemIndex := 0;
    for LIdx := Low(FCandidates) to High(FCandidates) do
      if FCandidates[LIdx].account = Model.Signer.SignerAccount.account then
        cbSignerAccount.ItemIndex := LIdx + 1;
  end;
  cbSignerAccountChange(Self);
end;

procedure TWIZOperationSigner_Select.OnNext;
begin
  Model.Signer.SelectedIndex := cbSignerAccount.ItemIndex;
  Model.Signer.SignerAccount := FCandidates[PtrInt(
    cbSignerAccount.Items.Objects[cbSignerAccount.ItemIndex])];
  if rbPrimary.Checked then
    Model.Signer.OperationSigningMode := akaPrimary
  else if rbSecondary.Checked then
    Model.Signer.OperationSigningMode := akaSecondary;
end;

function TWIZOperationSigner_Select.Validate(out message: String): boolean;
begin
  Result := True;
  if cbSignerAccount.ItemIndex < 1 then
  begin
    message := 'A Signer Account Must Be Selected';
    Exit(False);
  end;

end;

end.
