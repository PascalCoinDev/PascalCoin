unit UFRMMemoText;

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
