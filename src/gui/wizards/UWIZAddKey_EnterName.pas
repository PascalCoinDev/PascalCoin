unit UWIZAddKey_EnterName;

{$mode delphi}

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, UWizard, UWIZAddKey, UWIZModels;

type

  { TWIZAddKey_EnterName }

  TWIZAddKey_EnterName = class(TWizardForm<TWIZAddKeyModel>)
    txtName: TEdit;
    Label2: TLabel;
  public
    procedure OnPresent; override;
    procedure OnNext; override;
    function Validate(out message : AnsiString) : boolean; override;
  end;

implementation

{$R *.lfm}

{ TWIZAddKey_EnterName }

procedure TWIZAddKey_EnterName.OnPresent;
begin
  txtName.Clear;
  txtName.SetFocus;
end;

procedure TWIZAddKey_EnterName.OnNext;
begin
  Model.Name := Trim(txtName.Text);
end;

function TWIZAddKey_EnterName.Validate(out message : AnsiString) : boolean;
begin
  Result := Length(Trim(txtName.Text)) > 0;
  if not result then
    message := 'Name is empty or whitespace';
end;

end.

