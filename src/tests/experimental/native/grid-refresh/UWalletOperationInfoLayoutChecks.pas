unit UWalletOperationInfoLayoutChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Exercises actual experimental Operation Information controls with large
  operation counters and monetary values. It never searches or executes an
  operation, changes wallet settings, or broadcasts. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

procedure CheckWalletOperationInfoLayout(const ALogPath: String);

implementation

uses Windows, Messages, Classes, SysUtils, Types, Math, Forms, Controls,
  StdCtrls, ExtCtrls, ComCtrls, Graphics, {$IFDEF FPC}LCLIntf, LMessages,{$ENDIF}
  UMemory, UBlockChain, UFRMPayloadDecoder;

type
  TOperationInfoLayoutChecks = class(TComponent)
  private
    FLog: TStringList;
    FLogPath, FError: String;
    FPreviousException: TExceptionEvent;
    procedure Require(ACondition: Boolean; const AMessage: String);
    procedure Log(const AMessage: String);
    procedure OnException(Sender: TObject; E: Exception);
    procedure Pump;
    procedure CheckLayout(AForm: TFRMPayloadDecoder; const AContext: String);
    procedure Capture(AForm: TForm; const AName: String);
    procedure RunCase(AScale, ACase: Integer);
  public
    constructor Create(const ALogPath: String); reintroduce;
    destructor Destroy; override;
    procedure Run;
  end;

function CaptionUnicode(const ACaption: String): UnicodeString;
begin
  {$IFDEF FPC}
  Result := UTF8Decode(ACaption);
  {$ELSE}
  Result := ACaption;
  {$ENDIF}
end;

constructor TOperationInfoLayoutChecks.Create(const ALogPath: String);
begin
  inherited Create(nil);
  FLogPath := ChangeFileExt(ALogPath, '') + '.operation-info-layout.log';
  FLog := TStringList.Create;
  FPreviousException := Application.OnException;
  Application.OnException := OnException;
end;

destructor TOperationInfoLayoutChecks.Destroy;
begin
  Application.OnException := FPreviousException;
  FLog.Free;
  inherited Destroy;
end;

procedure TOperationInfoLayoutChecks.Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise Exception.Create(AMessage);
end;

procedure TOperationInfoLayoutChecks.Log(const AMessage: String);
begin
  FLog.Add(AMessage);
  FLog.SaveToFile(FLogPath);
end;

procedure TOperationInfoLayoutChecks.OnException(Sender: TObject; E: Exception);
begin
  if FError = '' then
    FError := E.ClassName + ': ' + E.Message;
end;

procedure TOperationInfoLayoutChecks.Pump;
var
  LUntil: UInt64;
begin
  LUntil := GetTickCount64 + 60;
  repeat
    Application.ProcessMessages;
    CheckSynchronize(0);
    Sleep(1);
  until GetTickCount64 >= LUntil;
  Require(FError = '', FError);
end;

procedure TOperationInfoLayoutChecks.CheckLayout(AForm: TFRMPayloadDecoder; const AContext: String);
var
  I, J: Integer;
  A, B: TControl;
  LText, LOverlap, LWindow, LWork: TRect;
  LCanvas: TControlCanvas;
  LBag: TDisposables;
begin
  LCanvas := LBag.AddObject(TControlCanvas.Create) as TControlCanvas;
  LCanvas.Control := AForm;
  for I := 0 to AForm.ControlCount - 1 do begin
    A := AForm.Controls[I];
    if not A.Visible or ((A is TLabel) and (TLabel(A).Caption = '')) then
      Continue;
    Require((A.Left >= 0) and (A.Top >= 0) and (A.Left + A.Width <= AForm.ClientWidth) and (A.Top + A.Height <= AForm.ClientHeight),
      AContext + ': control exceeds the form client: ' + A.Name);
    if A is TLabel then begin
      LCanvas.Font.Assign(TLabel(A).Font);
      LText := Types.Rect(0, 0, A.Width, 0);
      // LCL label text is UTF-8; its widgetset DrawText converts to native
      // Unicode. Windows.DrawText's ANSI path mismeasures translated captions.
      if TLabel(A).WordWrap then
        DrawText(LCanvas.Handle, PChar(TLabel(A).Caption), Length(TLabel(A).Caption), LText, DT_CALCRECT or DT_NOPREFIX or DT_WORDBREAK)
      else
        DrawText(LCanvas.Handle, PChar(TLabel(A).Caption), Length(TLabel(A).Caption), LText, DT_CALCRECT or DT_NOPREFIX);
      Require((LText.Right <= A.Width + 1) and (LText.Bottom <= A.Height + 1), AContext + ': text clipped in ' + A.Name);
      Log(Format('LABEL %s %s=%s rect=%d,%d,%d,%d text=%d,%d', [AContext, A.Name, TLabel(A).Caption, A.Left, A.Top, A.Width, A.Height, LText.Right, LText.Bottom]));
    end;
    for J := I + 1 to AForm.ControlCount - 1 do begin
      B := AForm.Controls[J];
      if not B.Visible or ((B is TLabel) and (TLabel(B).Caption = '')) then
        Continue;
      if IntersectRect(LOverlap, A.BoundsRect, B.BoundsRect) then
        Require((LOverlap.Right <= LOverlap.Left) or (LOverlap.Bottom <= LOverlap.Top), AContext + ': overlapping controls ' + A.Name + '/' + B.Name);
    end;
  end;
  Require(AForm.PageControl.Top > AForm.lblOperationTxt.Top + AForm.lblOperationTxt.Height, AContext + ': payload overlaps operation text');
  Require(AForm.bbClose.Top >= AForm.PageControl.Top + AForm.PageControl.Height, AContext + ': Close overlaps payload');
  GetWindowRect(AForm.Handle, LWindow);
  LWork := AForm.Monitor.WorkareaRect;
  Require((LWindow.Left >= LWork.Left) and (LWindow.Top >= LWork.Top) and (LWindow.Right <= LWork.Right) and (LWindow.Bottom <= LWork.Bottom), AContext + ': window exceeds native workarea');
  Log(Format('PASS %s client=%dx%d native=%d,%d,%d,%d', [AContext, AForm.ClientWidth, AForm.ClientHeight, LWindow.Left, LWindow.Top, LWindow.Right, LWindow.Bottom]));
end;

procedure TOperationInfoLayoutChecks.Capture(AForm: TForm; const AName: String);
var
  LBitmap: Graphics.TBitmap;
  X, Y, LInk: Integer;
  LBackground: TColor;
{$IFDEF FPC}
  LCanvasDC: HDC;

  procedure PaintControl(AControl: TWinControl);
  var
    LSaved, I: Integer;
    LOrigin, LWindowOrigin: TPoint;
    LWindow: TRect;
  begin
    if not AControl.HandleAllocated or not IsWindowVisible(AControl.Handle) then
      Exit;
    LSaved := SaveDC(LCanvasDC);
    try
      GetWindowRect(AControl.Handle, LWindow);
      LOrigin := AForm.ScreenToClient(LWindow.TopLeft);
      LWindowOrigin := LOrigin;
      SetViewportOrgEx(LCanvasDC, LOrigin.X, LOrigin.Y, nil);
      if AControl is TPageControl then
      begin
        // LCL's logical client excludes the tab strip. Paint the real Win32
        // tab control in its full window coordinates before its page children.
        IntersectClipRect(LCanvasDC, 0, 0, LWindow.Right - LWindow.Left,
          LWindow.Bottom - LWindow.Top);
        SendMessage(AControl.Handle, WM_PRINT, WPARAM(LCanvasDC),
          PRF_CLIENT or PRF_NONCLIENT or PRF_ERASEBKGND);
      end
      else
      begin
        SendMessage(AControl.Handle, WM_PRINT, WPARAM(LCanvasDC), PRF_NONCLIENT);
        LOrigin := AForm.ScreenToClient(AControl.ClientToScreen(Types.Point(0, 0)));
        SetViewportOrgEx(LCanvasDC, LOrigin.X, LOrigin.Y, nil);
        if AControl is TCustomGroupBox then
          IntersectClipRect(LCanvasDC, LWindowOrigin.X - LOrigin.X,
            LWindowOrigin.Y - LOrigin.Y, LWindowOrigin.X - LOrigin.X +
            LWindow.Right - LWindow.Left, LWindowOrigin.Y - LOrigin.Y +
            LWindow.Bottom - LWindow.Top)
        else
          IntersectClipRect(LCanvasDC, 0, 0, AControl.ClientWidth, AControl.ClientHeight);
        AControl.Perform(LM_ERASEBKGND, WPARAM(LCanvasDC), 0);
        AControl.Perform(LM_PAINT, WPARAM(LCanvasDC), 0);
        if not (AControl is TCustomControl) and not (AControl is TCustomForm) and
          not (AControl is TCustomGroupBox) then
          SendMessage(AControl.Handle, WM_PRINTCLIENT, WPARAM(LCanvasDC), PRF_CLIENT);
      end;
      for I := 0 to AControl.ControlCount - 1 do
        if AControl.Controls[I] is TWinControl then
          PaintControl(TWinControl(AControl.Controls[I]));
    finally
      RestoreDC(LCanvasDC, LSaved);
    end;
  end;
{$ENDIF}
begin
  AForm.Repaint;
  UpdateWindow(AForm.Handle);
{$IFDEF FPC}
  LBitmap := Graphics.TBitmap.Create;
{$ELSE}
  LBitmap := AForm.GetFormImage;
{$ENDIF}
  try
{$IFDEF FPC}
    LBitmap.PixelFormat := pf24bit;
    LBitmap.SetSize(AForm.ClientWidth, AForm.ClientHeight);
    LBitmap.Canvas.Lock;
    try
      // Initialize the bitmap before asking the real LCL controls to paint.
      LBitmap.Canvas.Brush.Color := AForm.GetRGBColorResolvingParent;
      LBitmap.Canvas.FillRect(AForm.ClientRect);
      LCanvasDC := LBitmap.Canvas.Handle;
      PaintControl(AForm);
    finally
      LBitmap.Canvas.Unlock;
    end;
{$ENDIF}
    LBitmap.SaveToFile(ChangeFileExt(FLogPath, '') + '.' + AName + '.bmp');
    LBackground := ColorToRGB(LBitmap.Canvas.Pixels[0, 0]);
    LInk := 0;
    for Y := 0 to (LBitmap.Height - 1) div 4 do
      for X := 0 to (LBitmap.Width - 1) div 4 do
        if ColorToRGB(LBitmap.Canvas.Pixels[X * 4, Y * 4]) <> LBackground then
          Inc(LInk);
    Require(LInk > 100, AName + ': native capture is blank');
  finally
    LBitmap.Free;
  end;
end;

procedure TOperationInfoLayoutChecks.RunCase(AScale, ACase: Integer);
var
  LForm: TFRMPayloadDecoder;
  LBag: TDisposables;
  LResume: TOperationResume;
  LWork: TRect;
  LContext, LBlock: String;
  LHeight, LPageHeight, LExtraHeight, I: Integer;
begin
  LForm := LBag.AddObject(TFRMPayloadDecoder.Create(nil)) as TFRMPayloadDecoder;
  LResume := CT_TOperationResume_NUL;
  LResume.valid := True;
  LResume.Block := 1127529;
  LResume.NOpInsideBlock := 1;
  LResume.n_operation := 186837;
  LResume.time := 1793490000;
  LResume.AffectedAccount := 4029;
  LResume.SignerAccount := 4029;
  LResume.DestAccount := 4195;
  LResume.OperationTxt := 'Changed type of account 4195-65';
  LResume.Fee := -1;
  SetLength(LResume.OperationHash, 32);
  if ACase > 0 then begin
    LResume.Block := High(Cardinal);
    LResume.NOpInsideBlock := High(Integer) - 1;
    LResume.n_operation := High(Cardinal);
    LResume.AffectedAccount := High(Cardinal);
    LResume.SignerAccount := High(Cardinal);
    LResume.DestAccount := High(Cardinal) - 1;
    LResume.Amount := High(Int64);
    LResume.Fee := -High(Int64);
    LResume.OperationTxt := 'Changed account information and ownership for account 4294967295, including its public key, type and transaction payload.';
  end;
  if ACase = 3 then
    LResume.AffectedAccount := High(Cardinal) - 1;
  LForm.Init(LResume);
  LBlock := IntToStr(LResume.Block) + '/' + IntToStr(LResume.NOpInsideBlock + 1) + ' ' + IntToStr(LResume.n_operation);
  Require(LForm.lblBlock.Caption = LBlock, 'Operation block/index/counter text changed');
  if ACase = 2 then begin
    // Set translated text before first Show. Production DoShow must lay it out;
    // calling its layout/Resize directly would conceal a first-show defect.
    LForm.Label1.Caption := 'Block und Operation:';
    LForm.Label6.Caption := 'Datum und Uhrzeit:';
    LForm.lblDateTime.Caption := 'Wednesday, 31 December 2026 23:59:59';
    LForm.lblSenderCaption.Caption := 'Absenderkonto:';
    LForm.lblReceiverCaption.Caption := 'Empfängerkonto:';
    LForm.lblAmountCaption.Caption := 'Gesamtbetrag:';
    LForm.lblFeeCaption.Caption := 'Transaktionsgebühr:';
    Require(Pos(UnicodeString(WideChar($00FC)), CaptionUnicode(LForm.lblFeeCaption.Caption)) > 0, 'Fee caption lost its translated Unicode character');
    Require(Pos(UnicodeString(WideChar($00E4)), CaptionUnicode(LForm.lblReceiverCaption.Caption)) > 0, 'Receiver caption lost its translated Unicode character');
  end;
  LForm.ActiveControl := LForm.bbClose;
  if AScale <> 100 then
    LForm.ScaleBy(AScale, 100);
  LForm.Position := poDesigned;
  LWork := Screen.PrimaryMonitor.WorkareaRect;
  LForm.Left := LWork.Left + 8;
  LForm.Top := LWork.Top + 8;
  LForm.Show;
  Pump;
  LContext := Format('operation-info-%d-case%d-first-show', [AScale, ACase]);
  CheckLayout(LForm, LContext);
  Capture(LForm, LContext);
  if ACase = 0 then begin
    LHeight := LForm.ClientHeight;
    LPageHeight := LForm.PageControl.Height;
    LExtraHeight := MulDiv(80, AScale, 100);
    LForm.ClientHeight := LHeight + LExtraHeight;
    Pump;
    Require(LForm.ClientHeight = LHeight + LExtraHeight, 'Details layout undid a larger client height');
    Require(LForm.PageControl.Height = LPageHeight + LExtraHeight, 'Payload did not receive the extra client height');
    CheckLayout(LForm, LContext + '-larger');
    LForm.ClientHeight := LHeight;
    Pump;
    CheckLayout(LForm, LContext + '-restored');
    LResume.n_operation := High(Cardinal);
    LResume.Block := High(Cardinal);
    LResume.NOpInsideBlock := High(Integer) - 1;
    LForm.OpResume := LResume;
    Pump;
    CheckLayout(LForm, LContext + '-updated');
    LHeight := LForm.ClientHeight;
    for I := 0 to 4 do
      LForm.OpResume := LResume;
    Pump;
    Require(LForm.ClientHeight = LHeight, 'Repeated details updates grew the dialog');
    // Both directions must reflow through actual scaling/resize notifications.
    if AScale = 150 then begin
      LForm.ScaleBy(2, 3);
      Pump;
      CheckLayout(LForm, LContext + '-scale-down');
      LForm.ScaleBy(3, 2);
      Pump;
      CheckLayout(LForm, LContext + '-scale-up');
    end else begin
      LForm.ScaleBy(150, AScale);
      Pump;
      CheckLayout(LForm, LContext + '-scale-up');
      LForm.ScaleBy(AScale, 150);
      Pump;
      CheckLayout(LForm, LContext + '-scale-down');
    end;
  end;
  LForm.Close;
  Pump;
  Require(not LForm.Visible, 'Operation Information remained visible after Close');
end;

procedure TOperationInfoLayoutChecks.Run;
var
  LScale, LCase: Integer;
begin
  for LScale in [100, 125, 150] do
    for LCase := 0 to 3 do
      RunCase(LScale, LCase);
  Log('PASS 12 Operation Information first-show cases, live updates and DPI roundtrips');
end;

procedure CheckWalletOperationInfoLayout(const ALogPath: String);
var
  LBag: TDisposables;
  LChecks: TOperationInfoLayoutChecks;
begin
  LChecks := LBag.AddObject(TOperationInfoLayoutChecks.Create(ALogPath)) as TOperationInfoLayoutChecks;
  LChecks.Run;
end;

end.
