unit UWIZSendPASC;

{$mode delphi}

{ Copyright (c) 2018 Sphere 10 Software (http://www.sphere10.com/)

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
  Ugochukwu Mmaduekwe - main developer
  Herman Schoenfeld - designer <herman@sphere10.com>: added grid-based layout
}

interface

uses
  Classes, SysUtils, Forms, Dialogs, UWizard, UCommon, UWIZModels;

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
  UCoreUtils,
  UWIZOperationSelected,
  UWIZSendPASC_EnterRecipient,
  UWIZSendPASC_EnterQuantity,
  UWIZOperationConfirmation;

{ TWIZSendPASCWizard }

constructor TWIZSendPASCWizard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner,
    [
    TWIZOperationSelected,
    TWIZSendPASC_EnterRecipient,
    TWIZSendPASC_EnterQuantity,
    TWIZOperationConfirmation
    ]
    );
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
  Result := TOperationsManager.ExecuteSendPASC(Model.Account.SelectedAccounts, Model.SendPASC.DestinationAccount, Model.Signer.SignerAccount, Model.SendPASC.SingleAmountToSend, Model.Fee.SingleOperationFee, Model.SendPASC.SendPASCMode, Model.Payload.Mode, IIF(Model.Payload.HasPayload, Model.Payload.Content, ''), Model.Payload.Password, message);
end;

function TWIZSendPASCWizard.CancelRequested(out message: ansistring): boolean;
begin
  Result := True;
end;

end.
