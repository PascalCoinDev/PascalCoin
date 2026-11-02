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

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

{ Chooses between adding a private key and importing a watch-only public key.
}

{ Public types in this unit

  Type             | Purpose
  -----------------+--------------------------------------------------------------------------
  TWIZAddKey_Start | Routes Add Key to private-key setup or public-key import.
}

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
    procedure OnPresent; override;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  UWIZAddKey_EnterName,
  UWIZAddKey_ImportPubKey,
  UWIZAddKey_ImportPrivKey,
  UWIZAddKey_GenerateOrImport;

{ TWIZAddKey_Start }


procedure TWIZAddKey_Start.OnNext;
begin
  if rbPublicKey.Checked = true then begin
    if Model.Action <> akaImportPublicKey then
    begin
      Model.KeyText := '';
      Model.Password := '';
    end;
    Model.Action := akaImportPublicKey;
    UpdatePath(ptReplaceAllNext, [TWIZAddKey_ImportPubKey, TWIZAddKey_EnterName]);
  end else begin
    if Model.Action = akaImportPublicKey then
    begin
      Model.KeyText := '';
      Model.Password := '';
      Model.Action := akaGenerateKey;
    end;
    UpdatePath(ptReplaceAllNext, [TWIZAddKey_GenerateOrImport, TWIZAddKey_ImportPrivKey]);
  end;
end;

procedure TWIZAddKey_Start.OnPresent;
begin
  rbPublicKey.Checked := Model.Action = akaImportPublicKey;
  rbPrivateKey.Checked := not rbPublicKey.Checked;
end;

end.

