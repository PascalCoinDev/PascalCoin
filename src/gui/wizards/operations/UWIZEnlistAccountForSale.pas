unit UWIZEnlistAccountForSale;

{$mode delphi}

{ Copyright (c) 2018 Sphere 10 Software (http://www.sphere10.com/)

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
  Ugochukwu Mmaduekwe - main developer
  Herman Schoenfeld - designer
}

interface

uses
  Classes, SysUtils, Forms, Dialogs, UCrypto, UCommon,
  UWizard, UWIZOperation, UAccounts, UCoreObjects, LCLType;

type

  { TWIZEnlistAccountForSaleWizard }

  TWIZEnlistAccountForSaleWizard = class(TWizard<TWIZOperationsModel>)
  public
    constructor Create(AOwner: TComponent); override;
    function DetermineHasNext: boolean; override;
    function DetermineHasPrevious: boolean; override;
    function FinishRequested(out message: ansistring): boolean; override;
    function CancelRequested(out message: ansistring): boolean; override;
  end;

implementation

uses
  UCoreUtils,
  UWIZOperationSelected,
  UWIZEnlistAccountForSale_SelectOption,
  UWIZEnlistAccountForSale_EnterSeller,
  UWIZEnlistAccountForSale_EnterSaleAmount,
  UWIZOperationConfirmation;

{ TWIZEnlistAccountForSaleWizard }

constructor TWIZEnlistAccountForSaleWizard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner,
    [
    TWIZOperationSelected,
    TWIZEnlistAccountForSale_SelectOption,
    TWIZEnlistAccountForSale_EnterSeller,
    TWIZEnlistAccountForSale_EnterSaleAmount,
    TWIZOperationConfirmation
    ]
    );
  TitleText := 'Enlist Account';
  FinishText := 'Enlist Account';
end;

function TWIZEnlistAccountForSaleWizard.DetermineHasNext: boolean;
begin
  Result := not (CurrentScreen is TWIZOperationConfirmation);
end;

function TWIZEnlistAccountForSaleWizard.DetermineHasPrevious: boolean;
begin
  Result := inherited DetermineHasPrevious;
end;

function TWIZEnlistAccountForSaleWizard.FinishRequested(out message: ansistring): boolean;
var
  LPublicKey: TAccountKey;
begin
  // Execute the Enlist Account For Sale Action here
   case Model.EnlistAccountForSale.AccountSaleMode of
    akaPublicSale:
      LPublicKey := CT_TECDSA_Public_Nul;

    akaPrivateSale:
      LPublicKey := Model.EnlistAccountForSale.NewOwnerPublicKey;

  end;

  Result := TWIZOperationsHelper.ExecuteEnlistAccountForSale(Model.Account.SelectedAccounts, Model.Signer.SignerAccount, Model.EnlistAccountForSale.SellerAccount, LPublicKey, Model.Fee.SingleOperationFee, Model.EnlistAccountForSale.SalePrice, Model.EnlistAccountForSale.LockedUntilBlock, Model.EnlistAccountForSale.AccountSaleMode, Model.Payload.PayloadEncryptionMode, IIF(Model.Payload.HasPayload, Model.Payload.Content, ''), Model.Payload.Password, message);
end;

function TWIZEnlistAccountForSaleWizard.CancelRequested(out message: ansistring): boolean;

begin
  Result := True;
end;

end.
