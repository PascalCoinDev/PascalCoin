unit UWIZChangeAccountInfo;

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

{ Coordinates account name or type changes, approval of skipped targets, fees
  and execution.
}

{ Public types in this unit

  Type                        | Purpose
  ----------------------------+--------------------------------------------------------------------------
  TWIZChangeAccountInfoWizard | Runs account-information updates for the approved target accounts.
}

interface

uses
  SysUtils, Classes, UWizard, UWIZOperation;

type

  { TWIZChangeAccountInfoWizard }

  TWIZChangeAccountInfoWizard = class(TWizard<TWIZOperationsModel>)
  public
    constructor Create(AOwner: TComponent); override;
    function DetermineHasNext: boolean; override;
    function DetermineHasPrevious: boolean; override;
    function FinishRequested(out message: String): boolean; override;
    function CancelRequested(out message: String): boolean; override;
  end;

implementation

uses
  UWallet,
  UCommon,
  UCoreObjects,
  UAccounts,
  UPCDataTypes,
  UCoreUtils,
  UWIZOperationSelected,
  UWIZChangeAccountInfo_Details,
  UWIZOperationConfirmation;

{ TWIZChangeAccountInfoWizard }

constructor TWIZChangeAccountInfoWizard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, [
    TWIZOperationSelected,
    TWIZChangeAccountInfo_Details,
    TWIZOperationConfirmation
    ]);
  TitleText := 'Change Account Info';
  FinishText := 'Change Account Info';
end;

function TWIZChangeAccountInfoWizard.DetermineHasNext: boolean;
begin
  Result := not (CurrentScreen is TWIZOperationConfirmation);
end;

function TWIZChangeAccountInfoWizard.DetermineHasPrevious: boolean;
begin
  Result := inherited DetermineHasPrevious;
end;

function TWIZChangeAccountInfoWizard.FinishRequested(out message: String): boolean;
var
  LAccountArray: TArray<TAccount>;
begin
  if Length(Model.ChangeAccountInfo.TargetAccounts) = 0 then
  begin
    message := 'No accounts remain to change. Return to Details and choose another type.';
    Exit(False);
  end;
  if (Model.ChangeAccountInfo.SkippedCount > 0) and
    not Model.ChangeAccountInfo.SkippedAccountsApproved then
  begin
    message := 'Approve the skipped accounts before continuing.';
    Exit(False);
  end;
  if Model.ChangeAccountInfo.UseSingleFeePayer then
  begin
    if not Model.ChangeAccountInfo.FeePayerSelected then
    begin
      message := 'Select the account which will pay the fee.';
      Exit(False);
    end;
    SetLength(LAccountArray, 1);
    LAccountArray[0] := Model.ChangeAccountInfo.FeePayerAccount;
  end
  else
  begin
    LAccountArray := Model.Account.SelectedAccounts;
  end;
  // Execute the Change Account Info here
  Result := TWIZOperationsHelper.ExecuteChangeAccountInfo(Model.Account.SelectedAccounts, LAccountArray, Model.Fee.SingleOperationFee, Model.Payload.PayloadEncryptionMode, IIF(Model.Payload.HasPayload, Model.Payload.Content, ''), Model.Payload.Password, Model.ChangeAccountInfo.NewName, Model.ChangeAccountInfo.NewType, message, akaPrimary, Model.ChangeAccountInfo.TargetAccounts);
  if Result then
    Model.RelockWallet;
end;

function TWIZChangeAccountInfoWizard.CancelRequested(out message: String): boolean;
begin
  Model.RelockWallet;
  Result := True;
end;

end.
