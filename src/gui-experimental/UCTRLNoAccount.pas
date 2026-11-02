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

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

{ Presents the first-account view when the wallet has no accounts and opens
  the account-acquisition page. }

{ Public types in this unit

  Type            | Purpose
  ----------------+-------------------------------------------------------------------------
  TCTRLNoAccounts | Shows the first-account prompt and account-acquisition action.
}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}FileUtil,{$ENDIF} Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, UCommon.UI, UCoreObjects;

type

  { TCTRLNoAccounts }

  TCTRLNoAccounts = class(TApplicationForm)
    btnGetAcc: TBitBtn;
    lblGetFirstAccount: TLabel;
    procedure btnGetAccClick(Sender: TObject);
  private
    FPBalanceSummary : PBalanceSummary;
  public
    property BalanceSummary : PBalanceSummary read FPBalanceSummary write FPBalanceSummary;
    procedure ActivateFirstTime; override;
  end;

implementation

uses UUserInterface;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

const
  GNoAccountsText = 'No Accounts';
  GHasAccountsText = 'You own %d accounts';

procedure TCTRLNoAccounts.ActivateFirstTime;
begin
  if not Assigned(FPBalanceSummary) then begin
    lblGetFirstAccount.Caption := GNoAccountsText;
    exit;
  end;
  case FPBalanceSummary^.TotalPASA of
    0: lblGetFirstAccount.Caption := GNoAccountsText;
    else lblGetFirstAccount.Caption := Format(GHasAccountsText, [FPBalanceSummary^.TotalPASA]);
  end;
end;

procedure TCTRLNoAccounts.btnGetAccClick(Sender: TObject);
begin
  // A PASA must be transferred or purchased; creating a key alone creates no account.
  TUserInterface.ShowInfo(Self, 'Get an account',
    'Create or select a key in Wallet Keys, then copy its public key.' + sLineBreak + sLineBreak +
    'Ask an account owner to transfer a PASA to that public key. You can also buy a listed ' +
    'account in Account Explorer once you have a funded account.' + sLineBreak + sLineBreak +
    'The public key is safe to share. Keep your private key and wallet password secret.');
  TUserInterface.ShowPrivateKeysDialog(Self);
end;

end.

