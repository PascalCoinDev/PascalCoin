unit UFRMMemoText;

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TFRMMemoText = class(TForm)
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

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

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
