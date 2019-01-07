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

{$mode delphi}

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
    function Validate(out message : AnsiString) : boolean; override;
  end;


implementation

{$R *.lfm}

uses
  UAccounts;

{ TWIZAddKey_ImportPubKey }


procedure TWIZAddKey_ImportPubKey.OnPresent;
begin
  txtPublicKey.Clear;
  txtPublicKey.SetFocus;
end;

procedure TWIZAddKey_ImportPubKey.OnNext;
begin
  Model.KeyText := txtPublicKey.Text;
end;

function TWIZAddKey_ImportPubKey.Validate(out message : AnsiString) : boolean;
var
  accountKey : TAccountKey;
begin
   Result := TAccountComp.AccountPublicKeyImport(txtPublicKey.Text, accountKey, message);
end;

end.

