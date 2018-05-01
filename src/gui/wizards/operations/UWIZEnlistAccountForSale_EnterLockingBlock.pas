unit UWIZEnlistAccountForSale_EnterLockingBlock;

{$mode delphi}
{$modeswitch nestedprocvars}

{ Copyright (c) 2018 by Ugochukwu Mmaduekwe

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

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
