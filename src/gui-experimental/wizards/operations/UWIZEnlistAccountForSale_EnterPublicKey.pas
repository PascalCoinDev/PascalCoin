unit UWIZEnlistAccountForSale_EnterPublicKey;

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
{$modeswitch nestedprocvars}

interface

uses
  SysUtils, StdCtrls, UWizard, UWIZOperation;

type

  { TWIZEnlistAccountForSale_EnterPublicKey }

  TWIZEnlistAccountForSale_EnterPublicKey = class(TWizardForm<TWIZOperationsModel>)
    gbTransaction: TGroupBox;
    lblBlockLock: TLabel;
    lblPublicKey: TLabel;
    mmoPublicKey: TMemo;
  public
    procedure OnPresent; override;
    procedure OnNext; override;
    function Validate(out message: ansistring): boolean; override;
  end;


implementation

{$R *.lfm}

uses
  UAccounts,
  UWIZEnlistAccountForSale_EnterLockingBlock;

{ TWIZEnlistAccountForSale_EnterPublicKey }

procedure TWIZEnlistAccountForSale_EnterPublicKey.OnPresent;
begin
  mmoPublicKey.SetFocus;
end;

procedure TWIZEnlistAccountForSale_EnterPublicKey.OnNext;
begin
  UpdatePath(ptInject, [TWIZEnlistAccountForSale_EnterLockingBlock]);
end;

function TWIZEnlistAccountForSale_EnterPublicKey.Validate(out message: ansistring): boolean;
var
  LIdx: integer;
begin
  Result := True;

  if not TAccountComp.AccountKeyFromImport(Trim(mmoPublicKey.Lines.Text),
    Model.EnlistAccountForSale.NewOwnerPublicKey, message) then
  begin
    message := Format('Error Importing Public Key, Specific Error: %s', [message]);
    Exit(False);
  end;

  for LIdx := Low(Model.Account.SelectedAccounts) to High(Model.Account.SelectedAccounts) do
    if TAccountComp.EqualAccountKeys(Model.EnlistAccountForSale.NewOwnerPublicKey,
      Model.Account.SelectedAccounts[LIdx].accountInfo.accountKey) then
    begin
      message := 'You Cannot Sell To An Account That You Want To Enlist For Sale.';
      Exit(False);
    end;

end;

end.
