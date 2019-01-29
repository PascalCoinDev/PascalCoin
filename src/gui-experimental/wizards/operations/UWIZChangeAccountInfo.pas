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

{$mode delphi}

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
    function FinishRequested(out message: ansistring): boolean; override;
    function CancelRequested(out message: ansistring): boolean; override;
  end;

implementation

uses
  UWallet,
  UCommon,
  UAccounts,
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

function TWIZChangeAccountInfoWizard.FinishRequested(out message: ansistring): boolean;
var
  LAccountArray: array of TAccount;
begin
  if Model.Account.Count = 1 then
  begin
    SetLength(LAccountArray, 1);
    LAccountArray[0] := Model.Signer.SignerAccount;
  end
  else
  begin
    LAccountArray := Model.Account.SelectedAccounts;
  end;
  // Execute the Change Account Info here
  Result := TWIZOperationsHelper.ExecuteChangeAccountInfo(Model.Account.SelectedAccounts, LAccountArray, Model.Fee.SingleOperationFee, Model.Payload.PayloadEncryptionMode, IIF(Model.Payload.HasPayload, Model.Payload.Content, ''), Model.Payload.Password, Model.ChangeAccountInfo.NewName, Model.ChangeAccountInfo.NewType, message);
  if TWIZOperationsModel.RelockOnFinish then
    TWallet.Keys.LockWallet;
end;

function TWIZChangeAccountInfoWizard.CancelRequested(out message: ansistring): boolean;
begin
  Result := True;
end;

end.
