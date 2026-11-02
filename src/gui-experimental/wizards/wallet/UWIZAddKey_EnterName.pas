unit UWIZAddKey_EnterName;

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

{ Collects a nonempty display name for the wallet key being added.
}

{ Public types in this unit

  Type                 | Purpose
  ---------------------+--------------------------------------------------------------------------
  TWIZAddKey_EnterName | Edits and validates the new wallet key's display name.
}

interface

uses
  SysUtils, StdCtrls, UWizard, UWIZAddKey;

type

  { TWIZAddKey_EnterName }

  TWIZAddKey_EnterName = class(TWizardForm<TWIZAddKeyModel>)
    txtName: TEdit;
    Label2: TLabel;
  public
    procedure OnPresent; override;
    procedure OnNext; override;
    procedure OnPrevious; override;
    function Validate(out message : String) : boolean; override;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

{ TWIZAddKey_EnterName }

procedure TWIZAddKey_EnterName.OnPresent;
begin
  txtName.Text := Model.Name;
  txtName.SetFocus;
end;

procedure TWIZAddKey_EnterName.OnNext;
begin
  Model.Name := Trim(txtName.Text);
end;

procedure TWIZAddKey_EnterName.OnPrevious;
begin
  OnNext;
end;

function TWIZAddKey_EnterName.Validate(out message : String) : boolean;
begin
  Result := Length(Trim(txtName.Text)) > 0;
  if not result then
    message := 'Name is empty or whitespace';
end;

end.

