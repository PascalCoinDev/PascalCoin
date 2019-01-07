unit UWIZAddKey_ImportPrivKey;

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

  { TWIZAddKey_ImportPrivKey }

  TWIZAddKey_ImportPrivKey = class(TWizardForm<TWIZAddKeyModel>)
    txtPassword: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    txtPrivateKey: TMemo;
  public
    procedure OnPresent; override;
    procedure OnNext; override;
    function Validate(out message : AnsiString) : boolean; override;
  end;

implementation

{$R *.lfm}

uses
  UCrypto,
  UWallet;

{ TWIZAddKey_ImportPrivKey }

procedure TWIZAddKey_ImportPrivKey.OnPresent;
begin
  txtPrivateKey.Clear;
  txtPassword.Clear;
  txtPrivateKey.SetFocus;
end;

procedure TWIZAddKey_ImportPrivKey.OnNext;
begin
  Model.KeyText := txtPrivateKey.Text;
  Model.Password := txtPassword.Text;
end;

function TWIZAddKey_ImportPrivKey.Validate(out message : AnsiString) : boolean;
var
  privateKey : TECPrivateKey;
begin
  try
    Result := TWallet.TryDecryptPrivateKey(txtPrivateKey.Text, txtPassword.Text, privateKey, message);
  finally
    if Assigned(privateKey) then
      privateKey.Free;
  end;
end;

end.

