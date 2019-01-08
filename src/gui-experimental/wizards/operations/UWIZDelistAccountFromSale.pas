unit UWIZDelistAccountFromSale;

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
  Classes, StdCtrls, UWizard, UWIZOperation;

type

  { TWIZDelistAccountFromSaleWizard }

  TWIZDelistAccountFromSaleWizard = class(TWizard<TWIZOperationsModel>)
  public
    constructor Create(AOwner: TComponent); override;
    function DetermineHasNext: boolean; override;
    function DetermineHasPrevious: boolean; override;
    function FinishRequested(out message: ansistring): boolean; override;
    function CancelRequested(out message: ansistring): boolean; override;
  end;

implementation

uses
  UCommon,
  UCrypto,
  UWallet,
  UAccounts,
  UCoreUtils,
  UCoreObjects,
  UWIZOperationSelected,
  UWIZDelistAccountFromSale_Options,
  UWIZOperationConfirmation;

{ TWIZDelistAccountFromSaleWizard }

constructor TWIZDelistAccountFromSaleWizard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner,
    [
    TWIZOperationSelected,
    TWIZDelistAccountFromSale_Options,
    TWIZOperationConfirmation
    ]
    );
  TitleText := 'Delist Account';
  FinishText := 'Delist Account';
end;

function TWIZDelistAccountFromSaleWizard.DetermineHasNext: boolean;
begin
  Result := not (CurrentScreen is TWIZOperationConfirmation);
end;

function TWIZDelistAccountFromSaleWizard.DetermineHasPrevious: boolean;
begin
  Result := inherited DetermineHasPrevious;
end;

function TWIZDelistAccountFromSaleWizard.FinishRequested(out message: ansistring): boolean;
begin
  // Execute the Delist Account From Sale Action here
  Result := TWIZOperationsHelper.ExecuteDelistAccountFromSale(Model.Account.SelectedAccounts, Model.Signer.SignerAccount, Model.Fee.SingleOperationFee, Model.Payload.PayloadEncryptionMode, IIF(Model.Payload.HasPayload, Model.Payload.Content, ''), Model.Payload.Password, message);
  if TWIZOperationsModel.RelockOnFinish then
   TWallet.Keys.LockWallet;
end;

function TWIZDelistAccountFromSaleWizard.CancelRequested(out message: ansistring): boolean;
begin
  Result := True;
end;

end.
