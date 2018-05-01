unit UWIZEnlistAccountForSale_SelectOption;

{$mode delphi}
{$modeswitch nestedprocvars}

{ Copyright (c) 2018 by Ugochukwu Mmaduekwe

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, UWizard, UCoreObjects, UWIZOperation, UWIZEnlistAccountForSale_EnterPublicKey;

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

uses UAccounts, USettings, UDataSources, UCommon, UCommon.UI, Generics.Collections;

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
