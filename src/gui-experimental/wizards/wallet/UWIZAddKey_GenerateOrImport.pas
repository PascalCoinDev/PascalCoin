unit UWIZAddKey_GenerateOrImport;

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

  { TWIZAddKey_GenerateOrImport }

  TWIZAddKey_GenerateOrImport = class(TWizardForm<TWIZAddKeyModel>)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    rbPrivateKey: TRadioButton;
    rbPublicKey: TRadioButton;
  public
    procedure OnNext; override;
  end;

implementation

{$R *.lfm}

uses
  UWIZAddKey_ImportPrivKey,
  UWIZAddKey_SelectEncryption,
  UWIZAddKey_EnterName;

procedure TWIZAddKey_GenerateOrImport.OnNext;
begin
  if rbPrivateKey.Checked then begin
    Model.Action := akaGenerateKey;
    UpdatePath(ptReplaceAllNext, [TWIZAddKey_SelectEncryption, TWIZAddKey_EnterName])
  end else begin
    Model.Action := akaImportPrivateKey;
    UpdatePath(ptReplaceAllNext, [TWIZAddKey_ImportPrivKey, TWIZAddKey_EnterName])
  end;
end;

end.

