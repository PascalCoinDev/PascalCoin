unit UWIZSendPASC;

{$mode delphi}

{ Copyright (c) 2018 by Ugochukwu Mmaduekwe

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
    Herman Schoenfeld <herman@sphere10.com>: added grid-based layout
}

interface

uses
  Classes, SysUtils, Forms, UWizard, UAccounts;

type

  { TWIZSendPASCModel }

  TWIZSendPASCModel = class(TComponent)
    public
      SelectedIndex: Integer;
      SelectedAccounts: TArray<TAccount>;
      SignerAccount, DestinationAccount: TAccount;
      AmountToSend: String;
      DefaultFee: Int64;
  end;

  { TWIZSendPASCWizard }

  TWIZSendPASCWizard = class(TWizard<TWIZSendPASCModel>)
    public
      constructor Create(AOwner: TComponent); override;
      function DetermineHasNext : boolean; override;
      function DetermineHasPrevious : boolean;  override;
      function FinishRequested(out message : AnsiString) : boolean; override;
      function CancelRequested(out message : AnsiString) : boolean; override;
  end;

implementation

uses
  UCrypto,
  UWallet,
  UWIZSendPASC_Start,
  UWIZSendPASC_Transaction,
  UWIZSendPASC_Confirmation;
  //UWIZSendPASC_Completion;

constructor TWIZSendPASCWizard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, [TWIZSendPASC_Start, TWIZSendPASC_Confirmation]);
  TitleText := 'Send PASC';
  FinishText := 'Send PASC';
end;

function TWIZSendPASCWizard.DetermineHasNext : boolean;
begin
  Result := NOT (CurrentScreen is TWIZSendPASC_Confirmation);
end;

function TWIZSendPASCWizard.DetermineHasPrevious : boolean;
begin
  Result := inherited DetermineHasPrevious;
end;

function TWIZSendPASCWizard.FinishRequested(out message : AnsiString) : boolean;
begin
  // Execute the PASC Sending here
    raise ENotImplemented.Create('Operation Not Yet Implemented');
end;

function TWIZSendPASCWizard.CancelRequested(out message : AnsiString) : boolean;
begin
  Result := true;
end;

end.

