unit UFRMMemoText;

{ Copyright (c) 2016 by PacalCoin Developers

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

interface

{$I ..\config.inc}

uses
  LCLIntf, LCLType, LMessages,
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, UCommon.UI;

type
  TFRMMemoText = class(TApplicationForm)
    pnlBottom: TPanel;
    Memo: TMemo;
    bbCancel: TBitBtn;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Procedure InitData(Title : String; text : String);
  end;

implementation

{$R *.lfm}


procedure TFRMMemoText.FormCreate(Sender: TObject);
begin
  Memo.Clear;
end;

procedure TFRMMemoText.InitData(Title, text: String);
begin
  Caption := Title;
  Memo.Lines.Text := text;
end;

end.
