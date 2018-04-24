unit UWIZAddKey_ImportPubKey;

{$mode delphi}

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UWizard, UWIZAddKey, UCoreObjects;

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

uses UAccounts;

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

