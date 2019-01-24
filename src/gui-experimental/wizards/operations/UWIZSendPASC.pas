unit UWIZSendPASC;

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

  { TWIZSendPASCWizard }

  TWIZSendPASCWizard = class(TWizard<TWIZOperationsModel>)
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
  UWIZSendPASC_Details,
  UWIZOperationConfirmation;

{ TWIZSendPASCWizard }

constructor TWIZSendPASCWizard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner,[
    TWIZOperationSelected,
    TWIZSendPASC_Details,
    TWIZOperationConfirmation
   ]);
  TitleText := 'Send PASC';
  FinishText := 'Send PASC';
end;

function TWIZSendPASCWizard.DetermineHasNext: boolean;
begin
  Result := not (CurrentScreen is TWIZOperationConfirmation);
end;

function TWIZSendPASCWizard.DetermineHasPrevious: boolean;
begin
  Result := inherited DetermineHasPrevious;
end;

function TWIZSendPASCWizard.FinishRequested(out message: ansistring): boolean;
begin
  // Execute the PASC Sending here
  Result := TWIZOperationsHelper.ExecuteSendPASC(Model.Account.SelectedAccounts, Model.SendPASC.DestinationAccount, Model.SendPASC.SingleAmountToSend, Model.Fee.SingleOperationFee, Model.SendPASC.SendPASCMode, Model.Payload.PayloadEncryptionMode, IIF(Model.Payload.HasPayload, Model.Payload.Content, ''), Model.Payload.Password, message);
  if TWIZOperationsModel.RelockOnFinish then
  TWallet.Keys.LockWallet;
end;

function TWIZSendPASCWizard.CancelRequested(out message: ansistring): boolean;
begin
  Result := True;
end;

end.
