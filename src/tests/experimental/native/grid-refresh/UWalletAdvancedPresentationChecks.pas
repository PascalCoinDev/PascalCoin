unit UWalletAdvancedPresentationChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Presentation checks use the actual Advanced Operation controls without a node
  or wallet. They never invoke Execute or construct a financial operation.
  The fixed-size dialog at 150% needs a Full HD test viewport; a 1024x720
  hosted desktop cannot contain its 1140x975 client area and native frame. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

procedure CheckAdvancedOperationPresentation(const ALogPath: String);

implementation

uses
  Windows, Messages, Classes, SysUtils, Types, Math, Forms, Controls, StdCtrls,
  ExtCtrls, ComCtrls, Graphics, {$IFDEF FPC}LMessages,{$ENDIF}
  UMemory, UNode, UFRMOperation;

type
  TControlFontAccess = class(TControl)
  public
    property Font;
  end;

  TGroupContentAccess = class(TGroupBox);

  TAdvancedPresentationChecks = class(TComponent)
  private
    FLog: TStringList;
    FLogPath, FError: String;
    FPreviousException: TExceptionEvent;
    procedure Require(ACondition: Boolean; const AMessage: String);
    procedure Log(const AMessage: String);
    procedure OnException(Sender: TObject; E: Exception);
    procedure Pump;
    procedure CheckControls(AParent: TWinControl; ACanvas: TCanvas;
      const AContext: String; ACheckOverlap: Boolean);
    procedure Capture(AForm: TForm; const AName: String);
    procedure RunScale(ANumerator, ADenominator: Integer);
  public
    constructor Create(const ALogPath: String); reintroduce;
    destructor Destroy; override;
    procedure Run;
  end;

constructor TAdvancedPresentationChecks.Create(const ALogPath: String);
begin
  inherited Create(nil);
  FLogPath := ALogPath;
  FLog := TStringList.Create;
  FPreviousException := Application.OnException;
  Application.OnException := OnException;
end;

destructor TAdvancedPresentationChecks.Destroy;
begin
  Application.OnException := FPreviousException;
  FLog.Free;
  inherited Destroy;
end;

procedure TAdvancedPresentationChecks.Require(ACondition: Boolean;
  const AMessage: String);
begin
  if not ACondition then
    raise Exception.Create(AMessage);
end;

procedure TAdvancedPresentationChecks.Log(const AMessage: String);
begin
  FLog.Add(AMessage);
  FLog.SaveToFile(FLogPath);
end;

procedure TAdvancedPresentationChecks.OnException(Sender: TObject; E: Exception);
begin
  if FError = '' then
    FError := E.ClassName + ': ' + E.Message;
end;

procedure TAdvancedPresentationChecks.Pump;
var
  LUntil: UInt64;
begin
  LUntil := GetTickCount64 + 50;
  repeat
    Application.ProcessMessages;
    CheckSynchronize(0);
    Sleep(1);
  until GetTickCount64 >= LUntil;
  Require(FError = '', FError);
end;

procedure TAdvancedPresentationChecks.CheckControls(AParent: TWinControl;
  ACanvas: TCanvas; const AContext: String; ACheckOverlap: Boolean);
var
  LContent, LOverlap, LText: TRect;
  A, B: TControl;
  I, J, LNeeded: Integer;
  LCaption: String;
begin
  LContent := AParent.ClientRect;
  if AParent is TGroupBox then
    TGroupContentAccess(AParent).AdjustClientRect(LContent);
  for I := 0 to AParent.ControlCount - 1 do
  begin
    A := AParent.Controls[I];
    if not A.Visible then
      Continue;
    if (A is TLabel) and (TLabel(A).Caption = '') then
      Continue;
    Require((A.Left >= LContent.Left) and (A.Top >= LContent.Top) and
      (A.Left + A.Width <= LContent.Right + 1) and
      (A.Top + A.Height <= LContent.Bottom + 1),
      Format('%s: %s exceeds native content (%d,%d,%d,%d in %d,%d,%d,%d)',
        [AContext, A.Name, A.Left, A.Top, A.Width, A.Height,
         LContent.Left, LContent.Top, LContent.Right, LContent.Bottom]));
    ACanvas.Font.Assign(TControlFontAccess(A).Font);
    if A is TLabel then
    begin
      LCaption := StringReplace(TLabel(A).Caption, '&', '', [rfReplaceAll]);
      LText := Rect(0, 0, A.Width, 0);
      if TLabel(A).WordWrap then
        DrawText(ACanvas.Handle, PChar(LCaption), Length(LCaption), LText,
          DT_CALCRECT or DT_NOPREFIX or DT_WORDBREAK)
      else
        DrawText(ACanvas.Handle, PChar(LCaption), Length(LCaption), LText,
          DT_CALCRECT or DT_NOPREFIX);
      Require((LText.Right - LText.Left <= A.Width + 1) and
        (LText.Bottom - LText.Top <= A.Height + 1),
        AContext + ': label text is clipped in ' + A.Name);
    end;
    if A is TRadioButton then
    begin
      LCaption := StringReplace(TRadioButton(A).Caption, '&', '', [rfReplaceAll]);
      LNeeded := ACanvas.TextWidth(LCaption) + GetSystemMetrics(SM_CXMENUCHECK) + 8;
      Require(A.Width >= LNeeded,
        Format('%s: %s truncates its radio caption (%d < %d)',
          [AContext, A.Name, A.Width, LNeeded]));
      Require(A.Height >= ACanvas.TextHeight('Wg'), AContext + ': radio font is clipped');
    end;
    if A is TCustomEdit then
      Require(TWinControl(A).ClientHeight >= ACanvas.TextHeight('Wg'),
        AContext + ': edit font is clipped in ' + A.Name);
    if ACheckOverlap then
      for J := I + 1 to AParent.ControlCount - 1 do
      begin
        B := AParent.Controls[J];
        if not B.Visible or ((B is TLabel) and (TLabel(B).Caption = '')) then
          Continue;
        if IntersectRect(LOverlap, A.BoundsRect, B.BoundsRect) then
          Require((LOverlap.Right - LOverlap.Left <= 1) or
            (LOverlap.Bottom - LOverlap.Top <= 1),
            AContext + ': overlapping controls ' + A.Name + ' and ' + B.Name);
      end;
  end;
end;

procedure TAdvancedPresentationChecks.Capture(AForm: TForm; const AName: String);
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

procedure TAdvancedPresentationChecks.RunScale(ANumerator, ADenominator: Integer);
const
  CPageNames: array[0..5] of String = ('tsTransaction', 'tsChangePrivateKey',
    'tsListAccount', 'tsDelistAccount', 'tsBuyAccount', 'tsChangeInfo');
var
  LBag: TDisposables;
  LForm: TFRMOperation;
  LCanvas: TControlCanvas;
  LWork, LWindow: TRect;
  I: Integer;
  LContext: String;
begin
  Require(not TNode.NodeExists, 'Advanced UI fixture must not initialize a node');
  LForm := LBag.AddObject(TFRMOperation.Create(nil, nil)) as TFRMOperation;
  LCanvas := LBag.AddObject(TControlCanvas.Create) as TControlCanvas;
  LCanvas.Control := LForm;
  Require(LForm.PageControlOpType.PageCount = Length(CPageNames),
    'Advanced Operation must retain all six tabs');
  Require(Assigned(LForm.tsDelistAccount) and
    (LForm.FindComponent('tsDelistAccount') = LForm.tsDelistAccount),
    'Delist resource is not bound to its published operation field');
  // Cancel receives focus before display; no account validation or execution
  // occurs when this presentation-only form is closed without a node.
  LForm.ActiveControl := LForm.bbCancel;
  // Match UpdateAccountsInfo's empty-sender presentation without reading a bank.
  LForm.memoAccounts.Visible := False;
  LForm.lblAccountsCount.Visible := False;
  LForm.ebSenderAccount.Text := '';
  LForm.lblAccountBalance.Caption := '0.0000';
  if ANumerator <> ADenominator then
    LForm.ScaleBy(ANumerator, ADenominator);
  LForm.Position := poDesigned;
  LWork := Screen.PrimaryMonitor.WorkareaRect;
  LForm.Left := LWork.Left + 12;
  LForm.Top := LWork.Top + 12;
  LForm.Show;
  Pump;
  GetWindowRect(LForm.Handle, LWindow);
  Log(Format('GEOMETRY scale=%d ppi=%d client=%dx%d native=%d,%d,%d,%d workarea=%d,%d,%d,%d',
    [ANumerator * 100 div ADenominator, LForm.PixelsPerInch,
     LForm.ClientWidth, LForm.ClientHeight, LWindow.Left, LWindow.Top,
     LWindow.Right, LWindow.Bottom, LWork.Left, LWork.Top, LWork.Right,
     LWork.Bottom]));
  Require((LWindow.Left >= LWork.Left) and (LWindow.Top >= LWork.Top) and
    (LWindow.Right <= LWork.Right) and (LWindow.Bottom <= LWork.Bottom),
    'Advanced Operation window exceeds the native work area');
  for I := 0 to High(CPageNames) do
  begin
    Require(LForm.PageControlOpType.Pages[I].Name = CPageNames[I],
      'Advanced operation tab/resource identity changed');
    LForm.PageControlOpType.ActivePageIndex := I;
    Pump;
    // The explicit nil-node constructor intentionally has no eligible sender.
    // Reveal the operation page solely to inspect its real native controls.
    LForm.tsOperation.Visible := True;
    LForm.PageControlLocked.ActivePage := LForm.tsOperation;
    LForm.rbEncryptedWithOldEC.Caption := 'Encrypted with sender public key';
    LForm.rbEncryptedWithEC.Caption := 'Encrypted with destination public key';
    LForm.lblPayloadLength.Caption := '(1024b -> 1040b)';
    LForm.lblEncryptionErrors.Caption := 'Payload status';
    // Production payload changes finish by laying out the refreshed captions.
    // The empty sender list keeps this real handler independent of node data.
    LForm.memoPayloadClick(nil);
    LCanvas.Font.Assign(LForm.rbEncryptedWithEC.Font);
    Log(Format('RADIO after layout width=%d text=%d glyph=%d font=%d formfont=%d',
      [LForm.rbEncryptedWithEC.Width,
       LCanvas.TextWidth(LForm.rbEncryptedWithEC.Caption),
       GetSystemMetrics(SM_CXMENUCHECK), LForm.rbEncryptedWithEC.Font.Height,
       LForm.Font.Height]));
    LForm.Repaint;
    Pump;
    Log(Format('RADIO after paint width=%d%s', [LForm.rbEncryptedWithEC.Width,
      {$IFDEF FPC}' autosize=' + BoolToStr(LForm.rbEncryptedWithEC.AutoSize, True){$ELSE}''{$ENDIF}]));
    LContext := Format('advanced-%s-%d', [CPageNames[I], ANumerator * 100 div ADenominator]);
    Require(LForm.PageControlOpType.Showing and LForm.gbPayload.Showing,
      LContext + ': operation page is not visible');
    CheckControls(LForm, LCanvas, LContext, True);
    CheckControls(LForm.tsOperation, LCanvas, LContext, True);
    CheckControls(LForm.PageControlOpType.ActivePage, LCanvas, LContext, True);
    CheckControls(LForm.gbPayload, LCanvas, LContext, True);
    Require((LForm.bbCancel.Top + LForm.bbCancel.Height <= LForm.ClientHeight) and
      (LForm.bbExecute.Top + LForm.bbExecute.Height <= LForm.ClientHeight) and
      (LForm.bbCancel.Left >= LForm.bbExecute.Left + LForm.bbExecute.Width),
      LContext + ': footer buttons are clipped or overlap');
    Capture(LForm, LContext);
    Log('PASS ' + LContext + ' native caption/content/font/bounds');
  end;
  LForm.Close;
  Pump;
  Require(not LForm.Visible, 'Advanced Operation remained open after Close');
  Require(not TNode.NodeExists, 'Advanced presentation unexpectedly created a node');
end;

procedure TAdvancedPresentationChecks.Run;
begin
  try
    RunScale(1, 1);
    RunScale(3, 2);
    Log('PASS Advanced Operation all six tabs at 100 and 150 percent; no node or execution');
  except
    on E: Exception do
    begin
      Log('FAIL ' + E.ClassName + ': ' + E.Message);
      raise;
    end;
  end;
end;

procedure CheckAdvancedOperationPresentation(const ALogPath: String);
var
  LBag: TDisposables;
  LChecks: TAdvancedPresentationChecks;
begin
  LChecks := LBag.AddObject(TAdvancedPresentationChecks.Create(ALogPath)) as TAdvancedPresentationChecks;
  LChecks.Run;
end;

end.
