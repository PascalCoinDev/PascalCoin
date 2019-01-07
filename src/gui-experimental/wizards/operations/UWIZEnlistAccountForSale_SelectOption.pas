unit UWIZEnlistAccountForSale_SelectOption;

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

  { TWIZEnlistAccountForSale_SelectOption }

  TWIZEnlistAccountForSale_SelectOption = class(TWizardForm<TWIZOperationsModel>)
    grpEnlistAccountForSale: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    rbPublicSale: TRadioButton;
    rbPrivateSale: TRadioButton;
  public
    procedure OnNext; override;
  end;



implementation

{$R *.lfm}

uses
  UCoreObjects,
  UWIZEnlistAccountForSale_EnterPublicKey;

{ TWIZEnlistAccountForSale_SelectOption }

procedure TWIZEnlistAccountForSale_SelectOption.OnNext;
begin
  if rbPublicSale.Checked then
    Model.EnlistAccountForSale.AccountSaleMode := akaPublicSale
  else
  begin
    Model.EnlistAccountForSale.AccountSaleMode := akaPrivateSale;
    UpdatePath(ptInject, [TWIZEnlistAccountForSale_EnterPublicKey]);
  end;

end;

end.
