unit UWIZEnlistAccountForSale_EnterLockingBlock;

{ Copyright (c) 2018 by Sphere 10 Software <http://www.sphere10.com/>

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://pascalcoin.org
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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons,
  UWizard, UWIZOperation, UCoreObjects, UWIZEnlistAccountForSale_EnterSeller;

type

  { TWIZEnlistAccountForSale_EnterLockingBlock }

  TWIZEnlistAccountForSale_EnterLockingBlock = class(TWizardForm<TWIZOperationsModel>)
    edtBlockLock: TEdit;
    gbLockBlock: TGroupBox;
    lblBlockLock: TLabel;
  public
    procedure OnPresent; override;
    function Validate(out message: ansistring): boolean; override;
  end;


implementation

{$R *.lfm}

uses
  UAccounts;

{ TWIZEnlistAccountForSale_EnterLockingBlock }

procedure TWIZEnlistAccountForSale_EnterLockingBlock.OnPresent;
begin
  edtBlockLock.SetFocus;
end;

function TWIZEnlistAccountForSale_EnterLockingBlock.Validate(out message: ansistring): boolean;
var
  LLockedUntilBlock: cardinal;
begin
  Result := True;

  LLockedUntilBlock := StrToIntDef(edtBlockLock.Text, 0);
  if LLockedUntilBlock = 0 then
  begin
    message := 'You Didn''t Insert a Locking Block.';
    Result := False;
    Exit;
  end;
  Model.EnlistAccountForSale.LockedUntilBlock := LLockedUntilBlock;

end;

end.
