unit UWIZChangeKey;

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
  Classes, SysUtils, Forms, Dialogs, UCrypto, UCommon, UWizard, UAccounts, UCoreObjects, UWIZOperation, LCLType;

type

  { TWIZChangeKeyWizard }

  TWIZChangeKeyWizard = class(TWizard<TWIZOperationsModel>)
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
  UCoreUtils,
  UWIZOperationSelected,
  UWIZChangeKey_SelectOption,
  UWIZOperationConfirmation;

{ TWIZChangeKeyWizard }

constructor TWIZChangeKeyWizard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner,
    [
    TWIZOperationSelected,
    TWIZChangeKey_SelectOption,
    TWIZOperationConfirmation
    ]
    );
  TitleText := 'Change Key';
  FinishText := 'Change Key';
end;

function TWIZChangeKeyWizard.DetermineHasNext: boolean;
begin
  Result := not (CurrentScreen is TWIZOperationConfirmation);
end;

function TWIZChangeKeyWizard.DetermineHasPrevious: boolean;
begin
  Result := inherited DetermineHasPrevious;
end;

function TWIZChangeKeyWizard.FinishRequested(out message: ansistring): boolean;
var
  LPublicKey: TAccountKey;
begin
  // Execute the Change Key Action here
  case Model.ChangeKey.ChangeKeyMode of
    akaTransferAccountOwnership:
      LPublicKey := Model.TransferAccount.AccountKey;

    akaChangeAccountPrivateKey:
      LPublicKey := Model.ChangeAccountPrivateKey.NewWalletKey.AccountKey;

  end;

  Result := TWIZOperationsHelper.ExecuteChangeKey(Model.Account.SelectedAccounts, Model.Signer.SignerAccount, LPublicKey, Model.Fee.SingleOperationFee, Model.Payload.PayloadEncryptionMode, IIF(Model.Payload.HasPayload, Model.Payload.Content, ''), Model.Payload.Password, message);
  if TWIZOperationsModel.RelockOnFinish then
   TWallet.Keys.LockWallet;
end;

function TWIZChangeKeyWizard.CancelRequested(out message: ansistring): boolean;
begin
  Result := True;
end;

end.
