unit UWIZSendPASC_Start;

{$mode delphi}

{ Copyright (c) 2018 by Ugochukwu Mmaduekwe

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UWizard, UWIZSendPASC, UWIZSendPASC_Transaction, UWIZSendPASC_Completion;

type

  { TWIZSendPASC_Start }

  TWIZSendPASC_Start = class(TWizardForm<TWIZSendPASCModel>)
    Label1: TLabel;
    txtSelectedAccountNumbersAndBalances: TMemo;
  public
    procedure OnPresent; override;
    procedure OnNext; override;
  end;


implementation

{$R *.lfm}

uses UAccounts;

{ TWIZSendPASC_Start }


procedure TWIZSendPASC_Start.OnPresent;

function GetAccNoWithChecksum(const AAccountNumber: Cardinal): String;
begin
  result := TAccountComp.AccountNumberToAccountTxtNumber(AAccountNumber);
end;
var
  acc: TAccount;
  i: Integer;
begin
  txtSelectedAccountNumbersAndBalances.Clear;
  for i := 0 to High(Model.SelectedAccounts) do
  begin
    acc := Model.SelectedAccounts[i];
    txtSelectedAccountNumbersAndBalances.Append(Format('This Account %s has %s PASC', [GetAccNoWithChecksum(acc.Account), TAccountComp.FormatMoney(acc.balance)]));
  end;
end;

procedure TWIZSendPASC_Start.OnNext;
begin
  UpdatePath(ptReplaceAllNext, [TWIZSendPASC_Transaction, TWIZSendPASC_Completion]);
end;

end.

