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
  ExtCtrls, Buttons, UWizard, UWIZModels, UWIZEnlistAccountForSale_EnterSeller;

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
  LockedUntilBlock: cardinal;
begin
  Result := True;

  LockedUntilBlock := StrToIntDef(edtBlockLock.Text, 0);
  if LockedUntilBlock = 0 then
  begin
    message := 'Insert locking block';
    Result := False;
    exit;
  end;
  Model.EnlistAccountForSale.LockedUntilBlock := LockedUntilBlock;

end;

end.
