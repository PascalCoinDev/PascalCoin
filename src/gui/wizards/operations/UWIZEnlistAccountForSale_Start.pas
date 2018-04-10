unit UWIZEnlistAccountForSale_Start;

{$mode delphi}
{$modeswitch nestedprocvars}

{ Copyright (c) 2018 by Ugochukwu Mmaduekwe

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, UWizard, UWIZEnlistAccountForSale, UWIZEnlistAccountForSale_List,
  UWIZEnlistAccountForSale_Confirmation;

type

  { TWIZEnlistAccountForSale_Start }

  TWIZEnlistAccountForSale_Start = class(TWizardForm<TWIZEnlistAccountForSaleModel>)
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

{ TWIZEnlistAccountForSale_Start }

procedure TWIZEnlistAccountForSale_Start.OnNext;
begin
  if rbPublicSale.Checked = True then
  begin
    Model.AccountSaleMode := akaPublicSale;
  end
  else
  begin
    Model.AccountSaleMode := akaPrivateSale;
  end;
  UpdatePath(ptReplaceAllNext, [TWIZEnlistAccountForSale_List,
    TWIZEnlistAccountForSale_Confirmation]);
end;

end.
