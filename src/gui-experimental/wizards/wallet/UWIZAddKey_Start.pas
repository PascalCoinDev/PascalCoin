unit UWIZAddKey_Start;

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

  { TWIZAddKey_Start }

  TWIZAddKey_Start = class(TWizardForm<TWIZAddKeyModel>)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    rbPrivateKey: TRadioButton;
    rbPublicKey: TRadioButton;
  private
    { private declarations }
  public
    { public declarations }
    procedure OnNext; override;
  end;

implementation

{$R *.lfm}

uses
  UWIZAddKey_EnterName,
  UWIZAddKey_ImportPubKey,
  UWIZAddKey_ImportPrivKey,
  UWIZAddKey_GenerateOrImport;

{ TWIZAddKey_Start }


procedure TWIZAddKey_Start.OnNext;
begin
  if rbPublicKey.Checked = true then begin
    Model.Action := akaImportPublicKey;
    UpdatePath(ptReplaceAllNext, [TWIZAddKey_ImportPubKey, TWIZAddKey_EnterName]);
  end else begin
    UpdatePath(ptReplaceAllNext, [TWIZAddKey_GenerateOrImport, TWIZAddKey_ImportPrivKey]);
  end;
end;

end.

