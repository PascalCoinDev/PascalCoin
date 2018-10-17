unit UCTRLNoAccount;

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, UCommon.UI, UCoreObjects;

type

  { TCTRLNoAccounts }

  TCTRLNoAccounts = class(TApplicationForm)
    btnGetAcc: TBitBtn;
    lblGetFirstAccount: TLabel;
  private
    FPBalanceSummary : PBalanceSummary;
  public
    property BalanceSummary : PBalanceSummary read FPBalanceSummary write FPBalanceSummary;
    procedure ActivateFirstTime; override;
  end;

implementation

{$R *.lfm}

const
  GNoAccountsText = 'No Accounts';
  GHasAccountsText = 'You own %d accounts';

procedure TCTRLNoAccounts.ActivateFirstTime;
begin
  case FPBalanceSummary^.TotalPASA of
    0: lblGetFirstAccount.Caption := GNoAccountsText;
    else lblGetFirstAccount.Caption := Format(GHasAccountsText, [FPBalanceSummary^.TotalPASA]);
  end;
end;

end.

