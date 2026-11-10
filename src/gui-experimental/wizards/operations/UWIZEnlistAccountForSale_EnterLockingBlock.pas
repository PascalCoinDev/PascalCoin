unit UWIZEnlistAccountForSale_EnterLockingBlock;

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


{$IFDEF FPC}
{$modeswitch nestedprocvars}
{$ENDIF}

{ Collects the block height until which a private account sale remains locked.
}

{ Public types in this unit

  Type                                       | Purpose
  -------------------------------------------+--------------------------------------------------------------------------
  TWIZEnlistAccountForSale_EnterLockingBlock | Validates and stores the sale's locking block height.
}

interface

uses
  SysUtils, StdCtrls, UWizard, UWIZOperation;

type

  { TWIZEnlistAccountForSale_EnterLockingBlock }

  TWIZEnlistAccountForSale_EnterLockingBlock = class(TWizardForm<TWIZOperationsModel>)
    edtBlockLock: TEdit;
    gbLockBlock: TGroupBox;
    lblBlockLock: TLabel;
  public
    procedure OnPresent; override;
    function Validate(out message: String): boolean; override;
  end;


implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

{ TWIZEnlistAccountForSale_EnterLockingBlock }

procedure TWIZEnlistAccountForSale_EnterLockingBlock.OnPresent;
begin
  edtBlockLock.SetFocus;
end;

function TWIZEnlistAccountForSale_EnterLockingBlock.Validate(out message: String): boolean;
var
  LLockedUntilBlock: cardinal;
begin
  Result := True;

  LLockedUntilBlock := StrToIntDef(edtBlockLock.Text, 0);
  if LLockedUntilBlock = 0 then
  begin
    message := 'You Didn''t Insert a Valid Locking Block.';
    Exit(False);
  end;
  Model.EnlistAccountForSale.LockedUntilBlock := LLockedUntilBlock;

end;

end.
