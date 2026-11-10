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

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

{ Coordinates recipient and amount entry, optional fee and payload steps,
  confirmation and transfer execution.
}

{ Public types in this unit

  Type               | Purpose
  -------------------+--------------------------------------------------------------------------
  TWIZSendPASCWizard | Runs and executes confirmed PASC transfers from selected accounts.
}

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
    function FinishRequested(out message: String): boolean; override;
    function CancelRequested(out message: String): boolean; override;
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

function TWIZSendPASCWizard.FinishRequested(out message: String): boolean;
begin
  // Execute the PASC Sending here
  Result := TWIZOperationsHelper.ExecuteSendPASC(Model.Account.SelectedAccounts, Model.SendPASC.DestinationAccount, Model.SendPASC.SingleAmountToSend, Model.Fee.SingleOperationFee, Model.SendPASC.SendPASCMode, Model.Payload.PayloadEncryptionMode, IIF(Model.Payload.HasPayload, Model.Payload.Content, ''), Model.Payload.Password, message);
  if Result then
    Model.RelockWallet;
end;

function TWIZSendPASCWizard.CancelRequested(out message: String): boolean;
begin
  Model.RelockWallet;
  Result := True;
end;

end.
