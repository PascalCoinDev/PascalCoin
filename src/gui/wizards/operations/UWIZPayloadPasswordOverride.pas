unit UWIZPayloadPasswordOverride;

{$mode delphi}
{$modeswitch nestedprocvars}

{ Copyright (c) 2018 Sphere 10 Software (http://www.sphere10.com/)

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
  Ugochukwu Mmaduekwe - main developer
  Herman Schoenfeld - designer
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, UCommon, UCommon.Collections,
  UWizard, UWIZSelectSignerOverride, UWIZModels;

type

  { TWIZPayloadPasswordOverride }

  TWIZPayloadPasswordOverride = class(TWizardForm<TWIZOperationsModel>)
    edtPassword: TEdit;
    grpPayload: TGroupBox;
    lblNote: TLabel;
    lblPassword: TLabel;
    paPayload: TPanel;
  public
    procedure OnNext; override;
    function Validate(out message: ansistring): boolean; override;
  end;


implementation

{$R *.lfm}

uses
  UAccounts, UUserInterface;

{ TWIZPayloadPasswordOverride }

procedure TWIZPayloadPasswordOverride.OnNext;
begin
  Model.Payload.Password := edtPassword.Text;
   UpdatePath(ptInject, [TWIZSelectSignerOverride]);
end;

function TWIZPayloadPasswordOverride.Validate(out message: ansistring): boolean;
begin
  Result := True;
  if Length(Trim(edtPassword.Text)) = 0 then
  begin
    message := 'password cannot be empty';
    Result := False;
    Exit;
  end;
end;

end.
