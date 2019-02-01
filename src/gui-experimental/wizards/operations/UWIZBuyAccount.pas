unit UWIZBuyAccount;

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
  Classes, UWizard, UWIZOperation;

type

  { TWIZBuyAccountWizard }

  TWIZBuyAccountWizard = class(TWizard<TWIZOperationsModel>)
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
  UCoreUtils,
  UWIZOperationSelected,
  UWIZBuyAccount_Details,
  UWIZOperationConfirmation;

{ TWIZBuyAccountWizard }

constructor TWIZBuyAccountWizard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, [
    TWIZOperationSelected,
    TWIZBuyAccount_Details,
    TWIZOperationConfirmation
    ]);
  TitleText := 'Buy Account';
  FinishText := 'Buy Account';
end;

function TWIZBuyAccountWizard.DetermineHasNext: boolean;
begin
  Result := not (CurrentScreen is TWIZOperationConfirmation);
end;

function TWIZBuyAccountWizard.DetermineHasPrevious: boolean;
begin
  Result := inherited DetermineHasPrevious;
end;

function TWIZBuyAccountWizard.FinishRequested(out message: ansistring): boolean;
begin
  // Execute Buy Account here
  Result := TWIZOperationsHelper.ExecuteBuyAccount(Model.Account.SelectedAccounts, Model.BuyAccount.AccountToBuy, Model.Fee.SingleOperationFee, Model.Payload.PayloadEncryptionMode, IIF(Model.Payload.HasPayload, Model.Payload.Content, ''), Model.Payload.Password, Model.BuyAccount.Amount, Model.BuyAccount.NewOwnerPublicKey, message);
  if TWIZOperationsModel.RelockOnFinish then
    TWallet.Keys.LockWallet;
end;

function TWIZBuyAccountWizard.CancelRequested(out message: ansistring): boolean;
begin
  Result := True;
end;

end.
