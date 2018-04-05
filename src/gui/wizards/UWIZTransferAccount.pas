unit UWIZTransferAccount;

{$mode delphi}

{ Copyright (c) 2018 by Ugochukwu Mmaduekwe

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

}

interface

uses
  Classes, SysUtils, Forms, Dialogs, UCrypto, UWizard, UAccounts, LCLType;

type

  { TWIZTransferAccountModel }
  TWIZPayloadEncryptionMode = (akaEncryptWithOldEC, akaEncryptWithEC,
    akaEncryptWithPassword, akaNotEncrypt);

  TWIZTransferAccountModel = class(TComponent)
  public
    DefaultFee: int64;
    NewPublicKey, Payload, EncryptionPassword: string;
    SelectedIndex: integer;
    AccountKey: TAccountKey;
    EncodedPayload: TRawBytes;
    SignerAccount: TAccount;
    SelectedAccounts, SortedAccounts: TArray<TAccount>;
    PayloadEncryptionMode: TWIZPayloadEncryptionMode;
  end;

  { TWIZTransferAccountWizard }

  TWIZTransferAccountWizard = class(TWizard<TWIZTransferAccountModel>)
    // private
    //procedure TransferAccount();
  public
    constructor Create(AOwner: TComponent); override;
    function DetermineHasNext: boolean; override;
    function DetermineHasPrevious: boolean; override;
    function FinishRequested(out message: ansistring): boolean; override;
    function CancelRequested(out message: ansistring): boolean; override;
  end;

implementation

uses
  UBlockChain,
  UOpTransaction,
  UNode,
  UConst,
  UWallet,
  UECIES,
  UAES,
  UWIZTransferAccount_Start,
  UWIZTransferAccount_Confirmation;

{ TWIZTransferAccountWizard }

constructor TWIZTransferAccountWizard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, [TWIZTransferAccount_Start,
    TWIZTransferAccount_Confirmation]);
  TitleText := 'Transfer Account';
  FinishText := 'Transfer Account';
end;

function TWIZTransferAccountWizard.DetermineHasNext: boolean;
begin
  Result := not (CurrentScreen is TWIZTransferAccount_Confirmation);
end;

function TWIZTransferAccountWizard.DetermineHasPrevious: boolean;
begin
  Result := inherited DetermineHasPrevious;
end;

function TWIZTransferAccountWizard.FinishRequested(out message: ansistring): boolean;
begin
  // Execute the Transfer Account Action here
  raise ENotImplemented.Create('Not yet implemented.');
  //try
  //  Result := True;
  //  // TransferAccount();
  //except
  //  On E: Exception do
  //  begin
  //    Result := False;
  //    message := E.ToString;
  //  end;
  //end;
end;

function TWIZTransferAccountWizard.CancelRequested(out message: ansistring): boolean;
begin
  Result := True;
end;

end.
