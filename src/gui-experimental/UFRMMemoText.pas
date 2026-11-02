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
    pnlPrompt: TPanel;
    lblPrompt: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FAllowInput: Boolean;
    FbbOk : TBitBtn;
    FMessageLayout, FArrangingMessage: Boolean;
    procedure EnsureOkButton;
    procedure InitMessage(AParent: TForm; const ATitle, APrompt,
      ADetails: String; AConfirm: Boolean);
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
function ConfirmMemoQuery(AParent: TForm; const ATitle, APrompt,
  ADetails: String): Boolean;
procedure ShowResultMemo(AParent: TForm; const ATitle, APrompt, ADetails: String);

implementation

uses UMemory, Math, Types;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

function InputMemoQuery(const ATitle : String; AAllowMultiline : Boolean; var AText : String) : Boolean;
Var LFRM : TFRMMemoText;
  LDisposables: TDisposables;
begin
  LFRM := LDisposables.AddObject(TFRMMemoText.Create(Nil)) as TFRMMemoText;
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

end;

function ConfirmMemoQuery(AParent: TForm; const ATitle, APrompt,
  ADetails: String): Boolean;
var
  LDisposables: TDisposables;
  LForm: TFRMMemoText;
begin
  // The local bag owns this modal form; PopupParent supplies presentation only.
  LForm := LDisposables.AddObject(TFRMMemoText.Create(nil)) as TFRMMemoText;
  LForm.InitMessage(AParent, ATitle, APrompt, ADetails, True);
  // Only the affirmative button authorizes submission; close and Esc decline.
  Result := LForm.ShowModal = mrYes;
end;

procedure ShowResultMemo(AParent: TForm; const ATitle, APrompt, ADetails: String);
var
  LDisposables: TDisposables;
  LForm: TFRMMemoText;
begin
  LForm := LDisposables.AddObject(TFRMMemoText.Create(nil)) as TFRMMemoText;
  LForm.InitMessage(AParent, ATitle, APrompt, ADetails, False);
  LForm.ShowModal;
end;

procedure TFRMMemoText.InitMessage(AParent: TForm; const ATitle, APrompt,
  ADetails: String; AConfirm: Boolean);
var
  LMonitor: TMonitor;
  LWork: TRect;
  LWidth, LHeight: Integer;
begin
  // A stable modal owner avoids LCL's pmNone handle recreation, which would
  // recreate the loaded memo with its complete text as an initial window caption.
  PopupMode := pmExplicit;
  if Assigned(AParent) then
    PopupParent := AParent
  else if Screen.ActiveForm <> Self then
    PopupParent := Screen.ActiveForm;
  AllowInput := False;
  Memo.WordWrap := False;
  Memo.ScrollBars := ssBoth;
  Memo.MaxLength := MaxInt;
  InitData(ATitle, '');
  lblPrompt.Caption := APrompt;
  // This mode owns child bounds; right anchors would reapply deferred panel sizing.
  lblPrompt.Anchors := [akLeft, akTop];
  bbCancel.Anchors := [akLeft, akTop];
  pnlPrompt.Visible := True;
  if AConfirm then begin
    EnsureOkButton;
    FbbOk.Name := 'bbConfirmYes';
    FbbOk.Kind := bkYes;
    FbbOk.Caption := 'Yes';
    FbbOk.ModalResult := mrYes;
    FbbOk.Default := False;
    FbbOk.Cancel := False;
    FbbOk.Anchors := [akLeft, akTop];
    bbCancel.Kind := bkNo;
    bbCancel.Caption := 'No';
    bbCancel.ModalResult := mrNo;
  end else begin
    bbCancel.Kind := bkCancel;
    bbCancel.Caption := 'Close';
    bbCancel.ModalResult := mrCancel;
  end;
  bbCancel.Cancel := True;
  bbCancel.Default := True;
  ActiveControl := bbCancel;
  BorderStyle := bsSizeable;
  Position := poDesigned;
  if Assigned(AParent) then
    LMonitor := AParent.Monitor
  else if Assigned(Screen.ActiveForm) then
    LMonitor := Screen.ActiveForm.Monitor
  else
    LMonitor := Screen.PrimaryMonitor;
  LWork := LMonitor.WorkareaRect;
  // Bound the outer window, including its native frame, to this work area.
  LWidth := Min(MulDiv(900, PixelsPerInch, 96), LWork.Right - LWork.Left);
  LHeight := Min(MulDiv(620, PixelsPerInch, 96), LWork.Bottom - LWork.Top);
  Constraints.MinWidth := Min(MulDiv(480, PixelsPerInch, 96), LWidth);
  Constraints.MinHeight := Min(MulDiv(300, PixelsPerInch, 96), LHeight);
  SetBounds(LWork.Left + (LWork.Right - LWork.Left - LWidth) div 2,
    LWork.Top + (LWork.Bottom - LWork.Top - LHeight) div 2, LWidth, LHeight);
  // LCL Win32 passes preloaded memo text to CreateWindowEx. Create the native
  // control empty, then load the complete list through its existing text API.
  HandleNeeded;
  Memo.HandleNeeded;
  Memo.Lines.Text := ADetails;
  FMessageLayout := True;
  FormResize(Self);
end;

procedure TFRMMemoText.FormResize(Sender: TObject);
var
  LCanvas: TControlCanvas;
  LDisposables: TDisposables;
  LText: TRect;
  LMargin, LButtonHeight, LButtonWidth, LTextHeight: Integer;
begin
  if not FMessageLayout or FArrangingMessage then
    Exit;
  FArrangingMessage := True;
  DisableAlign;
  try
    LMargin := MulDiv(12, PixelsPerInch, 96);
    LCanvas := LDisposables.AddObject(TControlCanvas.Create) as TControlCanvas;
    LCanvas.Control := Self;
    LCanvas.Font.Assign(lblPrompt.Font);
    LText := Rect(0, 0, Max(1, ClientWidth - LMargin * 2), 0);
    DrawText(LCanvas.Handle, PChar(lblPrompt.Caption), Length(lblPrompt.Caption),
      LText, DT_CALCRECT or DT_WORDBREAK or DT_NOPREFIX);
    LTextHeight := Max(LCanvas.TextHeight('Wg'), LText.Bottom - LText.Top);
    // Coalesce wrapped-label bounds before LCL can re-enter parent alignment.
    pnlPrompt.Height := LTextHeight + LMargin * 2;
    lblPrompt.SetBounds(LMargin, LMargin, Max(1, ClientWidth - LMargin * 2), LTextHeight);
    LCanvas.Font.Assign(bbCancel.Font);
    LButtonHeight := Max(MulDiv(31, PixelsPerInch, 96),
      LCanvas.TextHeight('Wg') + LMargin);
    LButtonWidth := Max(MulDiv(116, PixelsPerInch, 96),
      LCanvas.TextWidth(bbCancel.Caption) + LMargin * 2);
    pnlBottom.Height := LButtonHeight + LMargin * 2;
    bbCancel.SetBounds(Max(LMargin, ClientWidth - LMargin - LButtonWidth),
      LMargin, LButtonWidth, LButtonHeight);
    if Assigned(FbbOk) then
      FbbOk.SetBounds(Max(LMargin, bbCancel.Left - LMargin - LButtonWidth),
        LMargin, LButtonWidth, LButtonHeight);
  finally
    EnableAlign;
    FArrangingMessage := False;
  end;
end;

procedure TFRMMemoText.FormCreate(Sender: TObject);
begin
  Memo.Clear;
  FbbOk := Nil;
  FMessageLayout := False;
  FArrangingMessage := False;
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
    EnsureOkButton;
  end else FreeAndNil(FbbOk);

end;

procedure TFRMMemoText.EnsureOkButton;
begin
  if Assigned(FbbOk) then
    Exit;
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

procedure TFRMMemoText.SetDataText(const Value: String);
begin
  Memo.Lines.Text := Value;
end;

end.
