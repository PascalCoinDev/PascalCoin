unit UWIZAddKey_ImportPubKey;

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

{ Collects and validates an imported public key for a watch-only wallet entry.
}

{ Public types in this unit

  Type                    | Purpose
  ------------------------+--------------------------------------------------------------------------
  TWIZAddKey_ImportPubKey | Edits and validates an imported public key.
}

interface

uses
  StdCtrls, UWizard, UWIZAddKey;

type

  { TWIZAddKey_ImportPubKey }

  TWIZAddKey_ImportPubKey = class(TWizardForm<TWIZAddKeyModel>)
    Label1: TLabel;
    txtPublicKey: TMemo;
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

uses
  SysUtils, UWallet,
  UPCDataTypes;

{ TWIZAddKey_ImportPubKey }


procedure TWIZAddKey_ImportPubKey.OnPresent;
begin
  txtPublicKey.Text := Model.KeyText;
  txtPublicKey.SetFocus;
end;

procedure TWIZAddKey_ImportPubKey.OnNext;
begin
  Model.KeyText := Trim(txtPublicKey.Text);
end;

procedure TWIZAddKey_ImportPubKey.OnPrevious;
begin
  OnNext;
end;

function TWIZAddKey_ImportPubKey.Validate(out message : String) : boolean;
var
  accountKey : TAccountKey;
begin
   Result := TWallet.TryParsePublicKey(txtPublicKey.Text, accountKey, message);
end;

end.

