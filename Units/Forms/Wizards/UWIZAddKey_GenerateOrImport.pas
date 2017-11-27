unit UWIZAddKey_GenerateOrImport;

{$mode delphi}

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UWizard, UWIZAddKey;

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

uses UWIZAddKey_ImportPrivKey, UWIZAddKey_SelectEncryption, UWIZAddKey_EnterName;

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

