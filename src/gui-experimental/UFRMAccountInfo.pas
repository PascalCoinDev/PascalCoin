unit UFRMAccountInfo;

{ Copyright (c) 2016 by PascalCoin Developers

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

interface

{$I ..\config.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFRMAccountInfo = class(TApplicationForm)
    Label1: TLabel;
    lblAccount: TLabel;
    Label6: TLabel;
    lblBalance: TLabel;
    Label3: TLabel;
    ebPublicKey: TEdit;
    Label2: TLabel;
    Label4: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.lfm}

end.
