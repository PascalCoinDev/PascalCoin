unit UCTRLNoAccount;

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

