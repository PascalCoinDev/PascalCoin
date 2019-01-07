unit UWIZChangeKey_SelectOption;

{ Copyright (c) 2018 by Sphere 10 Software <http://www.sphere10.com/>

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  Acknowledgements:
  - Ugochukwu Mmaduekwe - main developer
  - Herman Schoenfeld - designer

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$mode delphi}
{$modeswitch nestedprocvars}

interface

uses
  StdCtrls, UWizard, UWIZOperation;

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

uses
  UCoreObjects,
  UWIZChangeKey_EnterKey,
  UWIZChangeKey_SelectKey;

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
