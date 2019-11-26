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
    FAllowInput: Boolean;
    FbbOk : TBitBtn;
    procedure SetAllowInput(const Value: Boolean);
    procedure SetDataText(const Value: String);
    function GetDataText: String;
    { Private declarations }
  public
    { Public declarations }
    Procedure InitData(const Title : String; const text : String);
    property AllowInput : Boolean read FAllowInput write SetAllowInput;
    property DataText : String read GetDataText write SetDataText;
  end;


function InputMemoQuery(const ATitle : String; AAllowMultiline : Boolean; var AText : String) : Boolean;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function InputMemoQuery(const ATitle : String; AAllowMultiline : Boolean; var AText : String) : Boolean;
Var LFRM : TFRMMemoText;
begin
  LFRM := TFRMMemoText.Create(Nil);
  try
    LFRM.InitData(ATitle,AText);
    LFRM.AllowInput := True;
    if AAllowMultiline then begin
      LFRM.Memo.ScrollBars := ssBoth;
    end else begin
      LFRM.Memo.ScrollBars := ssNone;
      LFRM.Memo.WordWrap := True;
    end;
    if LFRM.ShowModal=MrOk then begin
      AText := LFRM.DataText;
      Result := True;
    end else Result := False;
  finally
    LFRM.Free;
  end;
end;

procedure TFRMMemoText.FormCreate(Sender: TObject);
begin
  Memo.Clear;
  FbbOk := Nil;
end;

function TFRMMemoText.GetDataText: String;
begin
  Result := Memo.Lines.Text;
end;

procedure TFRMMemoText.InitData(const Title, text: String);
begin
  Caption := Title;
  Memo.Lines.Text := text;
end;

procedure TFRMMemoText.SetAllowInput(const Value: Boolean);
begin
  FAllowInput := Value;
  Memo.ReadOnly := Not FAllowInput;
  if FAllowInput then begin
    if Not Assigned(FbbOk) then begin
      FbbOk := TBitBtn.Create(Self);
      FbbOk.Parent := bbCancel.Parent;
      FbbOk.Anchors := bbCancel.Anchors;
      FbbOk.Left := bbCancel.Left - bbCancel.Width - 10;
      FbbOk.Top := bbCancel.Top;
      FbbOk.Width := bbCancel.Width;
      FbbOk.Height := bbCancel.Height;
      FbbOk.Kind := bkOK;
      FbbOk.ModalResult := MrOk;
      FbbOk.Caption := 'Ok';
    end;
  end else FreeAndNil(FbbOk);

end;

procedure TFRMMemoText.SetDataText(const Value: String);
begin
  Memo.Lines.Text := Value;
end;

end.
