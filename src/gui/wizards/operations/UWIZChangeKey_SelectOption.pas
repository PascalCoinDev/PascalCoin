unit UWIZChangeKey_SelectOption;

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
  ExtCtrls, UWizard, UWIZModels, UWIZChangeKey_EnterKey, UWIZChangeKey_SelectKey;

type

  { TWIZChangeKey_SelectOption }

  TWIZChangeKey_SelectOption = class(TWizardForm<TWIZOperationsModel>)
    gbChangeKeyOptions: TGroupBox;
    lblTransferAccountOwnership: TLabel;
    lblNote: TLabel;
    lblChangeAccountPrivateKey: TLabel;
    rbTransferAccountOwnership: TRadioButton;
    rbChangeAccountPrivateKey: TRadioButton;
  public
    procedure OnNext; override;
  end;



implementation

{$R *.lfm}

uses UAccounts, USettings, UDataSources, UCommon, UCommon.UI, Generics.Collections;

{ TWIZChangeKey_SelectOption }

procedure TWIZChangeKey_SelectOption.OnNext;
begin
  if rbTransferAccountOwnership.Checked then
  begin
    Model.ChangeKey.ChangeKeyMode := akaTransferAccountOwnership;
    UpdatePath(ptInject, [TWIZChangeKey_EnterKey]);
  end
  else if rbChangeAccountPrivateKey.Checked then
  begin
    Model.ChangeKey.ChangeKeyMode := akaChangeAccountPrivateKey;
    UpdatePath(ptInject, [TWIZChangeKey_SelectKey]);
  end;
end;

end.
