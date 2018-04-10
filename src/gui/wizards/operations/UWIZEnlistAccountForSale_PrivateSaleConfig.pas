unit UWIZEnlistAccountForSale_PrivateSaleConfig;

{$mode delphi}
{$modeswitch nestedprocvars}

{ Copyright (c) 2018 by Ugochukwu Mmaduekwe

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, UWizard, UWIZEnlistAccountForSale,
  UWIZEnlistAccountForSale_Transaction,
  UWIZEnlistAccountForSale_Confirmation;

type

  { TWIZEnlistAccountForSale_PrivateSaleConfig }

  TWIZEnlistAccountForSale_PrivateSaleConfig =
  class(TWizardForm<TWIZEnlistAccountForSaleModel>)
    edtBlockLock: TEdit;
    edtPublicKey: TEdit;
    gbTransaction: TGroupBox;
    lblBlockLock: TLabel;
    lblPublicKey: TLabel;
  public
    procedure OnNext; override;
    function Validate(out message: ansistring): boolean; override;
  end;


implementation

{$R *.lfm}

uses
  UAccounts;

{ TWIZEnlistAccountForSale_PrivateSaleConfig }

procedure TWIZEnlistAccountForSale_PrivateSaleConfig.OnNext;
begin
  Model.NewPublicKey := edtPublicKey.Text;
  UpdatePath(ptReplaceAllNext, [TWIZEnlistAccountForSale_Transaction,
    TWIZEnlistAccountForSale_Confirmation]);
end;

function TWIZEnlistAccountForSale_PrivateSaleConfig.Validate(
  out message: ansistring): boolean;
var
  c, LockedUntilBlock: cardinal;
  i: integer;
begin
  Result := True;

  LockedUntilBlock := StrToIntDef(edtBlockLock.Text, 0);
  if LockedUntilBlock = 0 then
  begin
    message := 'Insert locking block';
    Result := False;
    exit;
  end;
  Model.LockedUntilBlock := LockedUntilBlock;

  if not TAccountComp.AccountKeyFromImport(edtPublicKey.Text,
    Model.NewOwnerPublicKey, message) then
  begin
    message := 'Public key: ' + message;
    Result := False;
    exit;

  end;

  for i := Low(Model.SelectedAccounts) to High(Model.SelectedAccounts) do
  begin
    if TAccountComp.EqualAccountKeys(Model.NewOwnerPublicKey,
      Model.SelectedAccounts[i].accountInfo.accountKey) then
    begin
      message := 'New public key for private sale is the same public key';
      Result := False;
      Exit;
    end;
  end;

end;

end.
