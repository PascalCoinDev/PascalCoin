unit UWIZOperationPayload_Password;

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
  UWizard, UWIZOperation, UWIZOperationSigner_Select, UWIZOperationPayload_Content, UCoreObjects;

type

  { TWIZOperationPayload_Password }

  TWIZOperationPayload_Password = class(TWizardForm<TWIZOperationsModel>)
    edtPassword: TEdit;
    grpPayload: TGroupBox;
    lblNote: TLabel;
    lblPassword: TLabel;
    paPayload: TPanel;
  public
    procedure OnPresent; override;
    procedure OnNext; override;
    function Validate(out message: ansistring): boolean; override;
  end;


implementation

{$R *.lfm}

uses
  UAccounts, UUserInterface;

{ TWIZOperationPayload_Password }

procedure TWIZOperationPayload_Password.OnPresent;
begin
  edtPassword.SetFocus;
end;

procedure TWIZOperationPayload_Password.OnNext;
begin
  Model.Payload.Password := edtPassword.Text;
  UpdatePath(ptInject, [TWIZOperationPayload_Content]);
end;

function TWIZOperationPayload_Password.Validate(out message: ansistring): boolean;
begin
  Result := True;
  if Length(Trim(edtPassword.Text)) = 0 then
  begin
    message := 'Password Cannot Be Empty';
    Result := False;
    Exit;
  end;
end;

end.
