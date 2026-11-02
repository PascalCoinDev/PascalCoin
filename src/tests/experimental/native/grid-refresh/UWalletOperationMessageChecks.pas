unit UWalletOperationMessageChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Native operation-message checks without a node, wallet, signing or broadcast.
  Only owned dialogs are observed, scaled, captured and closed. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses Forms;

procedure CheckWalletOperationMessages(AParent: TForm; const ALogPath: String);

implementation

uses
  Windows, Messages, Classes, SysUtils, Types, Math, Controls, StdCtrls,
  ExtCtrls, Buttons, Graphics, {$IFDEF FPC}LMessages,{$ENDIF}
  UFRMMemoText, UMemory, UNode;

type
  TOperationMessageHost = class(TForm)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TMessageAction = (maNo, maEnter, maEscape, maClose, maYes);
  TMessageMode = (mmConfirm, mmResult, mmInput, mmLegacyReadOnly);

  TOperationMessageChecks = class(TComponent)
  private
    FParent: TForm;
    FTimer: TTimer;
    FLog: TStringList;
    FLogPath, FTitle, FPrompt, FDetails, FError, FCaseName, FPhase: String;
    FObserved: TFRMMemoText;
    FPreviousException: TExceptionEvent;
    FAction: TMessageAction;
    FMode: TMessageMode;
    FDeadline: UInt64;
    FProcessing, FActionSent, FScaled, FSmall, FCapture: Boolean;
    FObservedCount: Integer;
    procedure Require(ACondition: Boolean; const AMessage: String);
    procedure Log(const AMessage: String);
    procedure LogException(E: Exception);
    procedure OnException(Sender: TObject; E: Exception);
    procedure TimerTick(Sender: TObject);
    procedure LogHandles(AForm: TFRMMemoText; const AStage: String);
    procedure CheckLayout(AForm: TFRMMemoText; AInitial: Boolean);
    procedure Capture(AForm: TFRMMemoText);
    procedure RunCase(const AName: String; AMode: TMessageMode;
      AAction: TMessageAction; AScaled, ASmall, ACapture: Boolean);
    procedure SendAction(AForm: TFRMMemoText);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AParent: TForm; const ALogPath: String); reintroduce;
    destructor Destroy; override;
    procedure Run;
  end;

function NormalizeMemoText(const AText: String): String;
begin
  Result := StringReplace(AText, #13#10, #10, [rfReplaceAll]);
  Result := StringReplace(Result, #13, #10, [rfReplaceAll]);
  while (Length(Result) > 0) and (Result[Length(Result)] = #10) do
    Delete(Result, Length(Result), 1);
end;

constructor TOperationMessageHost.Create(AOwner: TComponent);
var
  LWork: TRect;
begin
  inherited CreateNew(AOwner);
  Caption := 'Operation message native fixture';
  // Legacy dialogs center on the host; keep that smaller host away from edges.
  LWork := Screen.PrimaryMonitor.WorkareaRect;
  SetBounds(LWork.Left + (LWork.Right - LWork.Left - 640) div 2,
    LWork.Top + (LWork.Bottom - LWork.Top - 480) div 2, 640, 480);
end;

constructor TOperationMessageChecks.Create(AParent: TForm; const ALogPath: String);
var
  LLines: TStringList;
  I: Integer;
begin
  inherited Create(nil);
  FParent := AParent;
  FLogPath := ALogPath;
  FLog := TStringList.Create;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 20;
  FTimer.OnTimer := TimerTick;
  FPreviousException := Application.OnException;
  Application.OnException := OnException;
  LLines := TStringList.Create;
  try
    for I := 1 to 267 do
      LLines.Add(Format('Operation %3.3d/267: Account %d type 1 -> 2; ', [I, I]) +
        StringOfChar('W', 700) + ' END-' + IntToStr(I));
    FDetails := LLines.Text;
    while (Length(FDetails) > 0) and (FDetails[Length(FDetails)] in [#13, #10]) do
      Delete(FDetails, Length(FDetails), 1);
  finally
    LLines.Free;
  end;
  FPrompt := 'Execute 267 operations?' + sLineBreak +
    'Total fee: 0.0267 PASC' + sLineBreak +
    'These operations will be transmitted to the network.';
end;

destructor TOperationMessageChecks.Destroy;
begin
  FTimer.Enabled := False;
  if Assigned(FObserved) then
    FObserved.RemoveFreeNotification(Self);
  Application.OnException := FPreviousException;
  FLog.Free;
  inherited Destroy;
end;

procedure TOperationMessageChecks.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if (AOperation = opRemove) and (AComponent = FObserved) then
    FObserved := nil;
end;

procedure TOperationMessageChecks.Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise Exception.Create(FCaseName + ': ' + AMessage);
end;

procedure TOperationMessageChecks.Log(const AMessage: String);
begin
  FLog.Add(AMessage);
  FLog.SaveToFile(FLogPath);
end;

procedure TOperationMessageChecks.LogException(E: Exception);
{$IFDEF FPC}
var
  I: Integer;
{$ENDIF}
begin
  Log('EXCEPTION phase=' + FPhase + ' case=' + FCaseName + ' ' + E.ClassName + ': ' + E.Message);
{$IFDEF FPC}
  Log('FRAME ' + BackTraceStrFunc(ExceptAddr));
  for I := 0 to ExceptFrameCount - 1 do
    Log('FRAME ' + BackTraceStrFunc(ExceptFrames[I]));
{$ELSE}
  Log('ADDRESS ' + IntToHex(NativeUInt(ExceptAddr), SizeOf(Pointer) * 2));
{$ENDIF}
end;

procedure TOperationMessageChecks.OnException(Sender: TObject; E: Exception);
var
  I: Integer;
begin
  if FError = '' then
  begin
    FError := E.ClassName + ': ' + E.Message;
    LogException(E);
  end;
  if Assigned(FObserved) and FObserved.HandleAllocated then
    PostMessage(FObserved.Handle, WM_CLOSE, 0, 0)
  else
    for I := 0 to Screen.FormCount - 1 do
      if (Screen.Forms[I] is TFRMMemoText) and Screen.Forms[I].Visible and
        Screen.Forms[I].HandleAllocated then
        PostMessage(Screen.Forms[I].Handle, WM_CLOSE, 0, 0);
end;

procedure TOperationMessageChecks.CheckLayout(AForm: TFRMMemoText; AInitial: Boolean);
var
  LWork, LWindow, LClient, LMemo, LFooter, LButton, LSummary: TRect;
  LYes: TComponent;
  LPrompt: TComponent;
  LStyle: NativeInt;
  LActualText, LSelectedText: String;
  LLineCount: Integer;

  procedure Step(const AName: String);
  begin
    FPhase := 'layout ' + AName;
    Log('CHECK ' + FCaseName + ' ' + AName);
  end;

  procedure CheckContained(const AOuter, AInner: TRect; const AName: String);
  var
    LContained: Boolean;
  begin
    LContained := (AInner.Left >= AOuter.Left) and (AInner.Top >= AOuter.Top) and
      (AInner.Right <= AOuter.Right) and (AInner.Bottom <= AOuter.Bottom);
    if not LContained then
    begin
      Log(Format('CONTAINMENT %s %s outer=%d,%d,%d,%d inner=%d,%d,%d,%d',
        [FCaseName, AName, AOuter.Left, AOuter.Top, AOuter.Right, AOuter.Bottom,
         AInner.Left, AInner.Top, AInner.Right, AInner.Bottom]));
      LogHandles(AForm, 'containment failure ' + AName);
    end;
    Require(LContained, AName + ' extends beyond its container');
  end;

begin
  LLineCount := 0;
  LActualText := '';
  Step('native bounds');
  Require(AForm.HandleAllocated and IsWindowVisible(AForm.Handle) and
    (fsModal in AForm.FormState), 'dialog is not an actual visible modal window');
  GetWindowRect(AForm.Handle, LWindow);
  LWork := AForm.Monitor.WorkareaRect;
  CheckContained(LWork, LWindow, 'dialog/workarea');
  LClient.TopLeft := AForm.ClientToScreen(Types.Point(0, 0));
  LClient.BottomRight := AForm.ClientToScreen(Types.Point(AForm.ClientWidth, AForm.ClientHeight));
  GetWindowRect(AForm.Memo.Handle, LMemo);
  GetWindowRect(AForm.pnlBottom.Handle, LFooter);
  GetWindowRect(AForm.bbCancel.Handle, LButton);
  CheckContained(LClient, LMemo, 'memo');
  CheckContained(LClient, LFooter, 'footer');
  CheckContained(LFooter, LButton, 'No/Close button');
  Require(LMemo.Bottom <= LFooter.Top, 'memo overlaps the button footer');
  Require(AForm.bbCancel.Visible and AForm.bbCancel.Enabled and
    (LButton.Right > LButton.Left) and (LButton.Bottom > LButton.Top),
    'No/Close button is missing or unusable');
  Step('native bounds checked');
  LPrompt := AForm.FindComponent('lblPrompt');
  LYes := AForm.FindComponent('bbConfirmYes');
  if FMode in [mmConfirm, mmResult] then
  begin
    Step('read memo text');
    LActualText := AForm.Memo.Lines.Text;
    Step('memo text returned');
    Require(NormalizeMemoText(LActualText) = NormalizeMemoText(FDetails),
      'long operation text was truncated or modified');
    Step('read memo line count');
    LLineCount := AForm.Memo.Lines.Count;
    Step('memo line count returned');
    Require(LLineCount = 267, 'all 267 operation lines were not retained');
    Require(AForm.Memo.ReadOnly and not AForm.AllowInput, 'operation memo accepts edits');
    Require((AForm.Memo.ScrollBars = ssBoth) and not AForm.Memo.WordWrap,
      'long operations require vertical and horizontal scrolling without wrapping');
    LStyle := GetWindowLongPtr(AForm.Memo.Handle, GWL_STYLE);
    Require((LStyle and WS_VSCROLL <> 0) and (LStyle and WS_HSCROLL <> 0),
      'native memo scrollbars are missing');
    Step('summary and buttons');
    Require((LPrompt is TLabel) and TLabel(LPrompt).Visible and
      (TLabel(LPrompt).Caption = FPrompt), 'fixed summary is missing or changed');
    LSummary.TopLeft := TLabel(LPrompt).ClientToScreen(Types.Point(0, 0));
    LSummary.BottomRight := TLabel(LPrompt).ClientToScreen(
      Types.Point(TLabel(LPrompt).Width, TLabel(LPrompt).Height));
    CheckContained(LClient, LSummary, 'summary');
    Require(LSummary.Bottom <= LMemo.Top, 'fixed summary overlaps the operation list');
    Require(AForm.bbCancel.Default and AForm.bbCancel.Cancel,
      'No/Close must be the default and Escape button');
    if FMode = mmConfirm then
    begin
      Require((LYes is TBitBtn) and TBitBtn(LYes).Visible and TBitBtn(LYes).Enabled,
        'confirmation has no affirmative button');
      Require(not TBitBtn(LYes).Default and (TBitBtn(LYes).ModalResult = mrYes),
        'Yes is default or has an incorrect modal result');
      Require(AForm.bbCancel.ModalResult = mrNo, 'confirmation default is not No');
      GetWindowRect(TBitBtn(LYes).Handle, LButton);
      CheckContained(LFooter, LButton, 'Yes button');
    end
    else
      Require(not Assigned(LYes) or not TControl(LYes).Visible,
        'result dialog exposes a confirmation Yes button');
    // Verify selection/copy access through the actual readonly native memo.
    // Reading SelText avoids changing the user's clipboard.
    Step('select all memo');
    AForm.Memo.SelectAll;
    Step('read selected memo text');
    LSelectedText := AForm.Memo.SelText;
    Step('selected memo text returned');
    Require(NormalizeMemoText(LSelectedText) = NormalizeMemoText(FDetails),
      'readonly operation list is not fully selectable/copyable');
    Step('clear memo selection');
    AForm.Memo.SelStart := 0;
    AForm.Memo.SelLength := 0;
    Step('memo selection cleared');
  end
  else
  begin
    Require(not AForm.pnlPrompt.Showing,
      'new confirmation summary leaked into a legacy memo mode');
    if FMode = mmInput then
      Require(not AForm.Memo.ReadOnly and AForm.AllowInput, 'legacy input mode lost editability')
    else
      Require(AForm.Memo.ReadOnly and not AForm.AllowInput, 'legacy readonly mode changed');
  end;
  Step('complete');
  if AInitial then
    Log('INITIAL ' + FCaseName + ' bounded to actual monitor workarea')
  else
    Log(Format('LAYOUT %s client=%dx%d lines=%d text=%d',
      [FCaseName, AForm.ClientWidth, AForm.ClientHeight,
       LLineCount, Length(LActualText)]));
end;

procedure TOperationMessageChecks.Capture(AForm: TFRMMemoText);
var
  LBitmap: Graphics.TBitmap;
  LOrigin: TPoint;
  LLeft, LTop, LRight, LBottom, X, Y, LLight, LDark: Integer;
  LColor: LongInt;
{$IFDEF FPC}
  LCanvasDC: HDC;

  procedure PaintControl(AControl: TWinControl);
  var
    LSaved, I: Integer;
    LOrigin: TPoint;
    LWindow: TRect;
  begin
    if not AControl.HandleAllocated or not IsWindowVisible(AControl.Handle) then
      Exit;
    LSaved := SaveDC(LCanvasDC);
    try
      GetWindowRect(AControl.Handle, LWindow);
      LOrigin := AForm.ScreenToClient(LWindow.TopLeft);
      SetViewportOrgEx(LCanvasDC, LOrigin.X, LOrigin.Y, nil);
      SendMessage(AControl.Handle, WM_PRINT, WPARAM(LCanvasDC), PRF_NONCLIENT);
      LOrigin := AForm.ScreenToClient(AControl.ClientToScreen(Types.Point(0, 0)));
      SetViewportOrgEx(LCanvasDC, LOrigin.X, LOrigin.Y, nil);
      IntersectClipRect(LCanvasDC, 0, 0, AControl.ClientWidth, AControl.ClientHeight);
      AControl.Perform(LM_ERASEBKGND, WPARAM(LCanvasDC), 0);
      AControl.Perform(LM_PAINT, WPARAM(LCanvasDC), 0);
      if not (AControl is TCustomControl) and not (AControl is TCustomForm) then
        SendMessage(AControl.Handle, WM_PRINTCLIENT, WPARAM(LCanvasDC), PRF_CLIENT);
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
      LBitmap.Canvas.Brush.Color := AForm.GetRGBColorResolvingParent;
      LBitmap.Canvas.FillRect(AForm.ClientRect);
      LCanvasDC := LBitmap.Canvas.Handle;
      PaintControl(AForm);
    finally
      LBitmap.Canvas.Unlock;
    end;
{$ENDIF}
    // Reject blank captures; native memo text must appear in the saved image.
    LOrigin := AForm.ScreenToClient(AForm.Memo.ClientToScreen(Types.Point(0, 0)));
    LLeft := Max(0, LOrigin.X + 2);
    LTop := Max(0, LOrigin.Y + 2);
    LRight := Min(LBitmap.Width - 1, LLeft + 240);
    LBottom := Min(LBitmap.Height - 1, LTop + 120);
    LLight := 0;
    LDark := 0;
    Y := LTop;
    while Y <= LBottom do
    begin
      X := LLeft;
      while X <= LRight do
      begin
        LColor := ColorToRGB(LBitmap.Canvas.Pixels[X, Y]);
        if (GetRValue(LColor) > 220) and (GetGValue(LColor) > 220) and
          (GetBValue(LColor) > 220) then
          Inc(LLight);
        if (GetRValue(LColor) < 100) and (GetGValue(LColor) < 100) and
          (GetBValue(LColor) < 100) then
          Inc(LDark);
        Inc(X, 2);
      end;
      Inc(Y, 2);
    end;
    Log(Format('PIXELS %s memo_light=%d memo_dark=%d', [FCaseName, LLight, LDark]));
    Require((LLight > 100) and (LDark > 20), 'native memo capture is blank or lacks operation text');
    LBitmap.SaveToFile(ChangeFileExt(FLogPath, '.' + FCaseName + '.bmp'));
  finally
    LBitmap.Free;
  end;
end;

procedure TOperationMessageChecks.SendAction(AForm: TFRMMemoText);
var
  LButton: TBitBtn;
  LTarget: HWND;
  LKey: WPARAM;
begin
  case FAction of
    maNo: SendMessage(AForm.bbCancel.Handle, BM_CLICK, 0, 0);
    maYes:
      begin
        LButton := AForm.FindComponent('bbConfirmYes') as TBitBtn;
        SendMessage(LButton.Handle, BM_CLICK, 0, 0);
      end;
    maClose: PostMessage(AForm.Handle, WM_CLOSE, 0, 0);
    maEnter, maEscape:
      begin
        if FAction = maEnter then
          LKey := VK_RETURN
        else
          LKey := VK_ESCAPE;
        if Assigned(AForm.ActiveControl) then
          LTarget := AForm.ActiveControl.Handle
        else
          LTarget := AForm.Handle;
        PostMessage(LTarget, WM_KEYDOWN, LKey, 0);
        PostMessage(LTarget, WM_KEYUP, LKey, 0);
      end;
  end;
end;

procedure TOperationMessageChecks.LogHandles(AForm: TFRMMemoText; const AStage: String);
var
  LFormHandle, LMemoHandle: HWND;
  LYes: TComponent;

  procedure LogControl(AControl: TWinControl);
  var
    LNative, LClient: TRect;
    LHandle: HWND;
  begin
    LNative := Types.Rect(0, 0, 0, 0);
    LClient := Types.Rect(0, 0, 0, 0);
    LHandle := 0;
    if AControl.HandleAllocated then
    begin
      LHandle := AControl.Handle;
      GetWindowRect(LHandle, LNative);
      GetClientRect(LHandle, LClient);
    end;
    Log(Format('BOUNDS %s %s %s logical=%d,%d,%d,%d client=%dx%d native=%d,%d,%d,%d native_client=%d,%d,%d,%d hwnd=%s anchors=%d%d%d%d',
      [FCaseName, AStage, AControl.Name, AControl.Left, AControl.Top,
       AControl.Width, AControl.Height, AControl.ClientWidth, AControl.ClientHeight,
       LNative.Left, LNative.Top, LNative.Right, LNative.Bottom,
       LClient.Left, LClient.Top, LClient.Right, LClient.Bottom,
       IntToHex(NativeUInt(LHandle), SizeOf(Pointer) * 2),
       Ord(akLeft in AControl.Anchors), Ord(akTop in AControl.Anchors),
       Ord(akRight in AControl.Anchors), Ord(akBottom in AControl.Anchors)]));
  end;

begin
  LFormHandle := 0;
  LMemoHandle := 0;
  if AForm.HandleAllocated then
    LFormHandle := AForm.Handle;
  if AForm.Memo.HandleAllocated then
    LMemoHandle := AForm.Memo.Handle;
  Log(Format('HANDLES %s %s form=%s memo=%s processing=%s',
    [FCaseName, AStage, IntToHex(NativeUInt(LFormHandle), SizeOf(Pointer) * 2),
     IntToHex(NativeUInt(LMemoHandle), SizeOf(Pointer) * 2), BoolToStr(FProcessing, True)]));
  LogControl(AForm);
  LogControl(AForm.pnlBottom);
  LogControl(AForm.bbCancel);
  LYes := AForm.FindComponent('bbConfirmYes');
  if LYes is TWinControl then
    LogControl(TWinControl(LYes));
end;

procedure TOperationMessageChecks.TimerTick(Sender: TObject);
var
  I: Integer;
  LForm: TFRMMemoText;
  LWork: TRect;
  LWidth, LHeight: Integer;
begin
  if FProcessing then
    Exit;
  FProcessing := True;
  try
    try
      if GetTickCount64 >= FDeadline then
        raise Exception.Create(FCaseName + ': timed out observing or closing the actual dialog');
      if FActionSent then
        Exit;
      LForm := nil;
      for I := 0 to Screen.FormCount - 1 do
        if (Screen.Forms[I] is TFRMMemoText) and Screen.Forms[I].Visible and
          (Screen.Forms[I].Caption = FTitle) then
        begin
          LForm := TFRMMemoText(Screen.Forms[I]);
          Break;
        end;
      if not Assigned(LForm) then
        Exit;
      Require(not Assigned(FObserved), 'dialog was observed twice before its lifetime ended');
      FPhase := 'observed native modal';
      FObserved := LForm;
      LForm.FreeNotification(Self);
      Inc(FObservedCount);
      FPhase := 'initial layout';
      CheckLayout(LForm, True);
      FPhase := 'owned form scaling';
      LogHandles(LForm, 'before scaling');
      if FScaled then
        LForm.ScaleBy(3, 2);
      LogHandles(LForm, 'after scaling');
      if FScaled or FSmall then
      begin
        LWork := LForm.Monitor.WorkareaRect;
        LWidth := Min(LForm.Width, LWork.Right - LWork.Left);
        LHeight := Min(LForm.Height, LWork.Bottom - LWork.Top);
        if FSmall then
        begin
          LWidth := Min(LWidth, 900);
          LHeight := Min(LHeight, 650);
        end;
        // Scale only this owned form. The monitor/workarea and desktop stay intact.
        LogHandles(LForm, 'before owned bounds');
        LForm.SetBounds(LWork.Left + (LWork.Right - LWork.Left - LWidth) div 2,
          LWork.Top + (LWork.Bottom - LWork.Top - LHeight) div 2, LWidth, LHeight);
        // LCL posts OnResize through its ordinary delayed-event queue. Let that
        // layout finish while the observer guard prevents nested test actions.
        Application.ProcessMessages;
        CheckSynchronize(0);
      end;
      LogHandles(LForm, 'after owned bounds');
      FPhase := 'final layout';
      CheckLayout(LForm, False);
      FPhase := 'native capture';
      if FCapture then
        Capture(LForm);
      if FMode = mmInput then
      begin
        LForm.Memo.SelStart := Length(LForm.Memo.Text);
        LForm.Memo.SelText := ' local edit';
        Require(Pos('local edit', LForm.Memo.Text) > 0, 'legacy input memo is not editable');
      end;
      FPhase := 'native action';
      FActionSent := True;
      SendAction(LForm);
    except
      on E: Exception do
        OnException(Self, E);
    end;
  finally
    FProcessing := False;
  end;
end;

procedure TOperationMessageChecks.RunCase(const AName: String;
  AMode: TMessageMode; AAction: TMessageAction; AScaled, ASmall, ACapture: Boolean);
var
  LResult: Boolean;
  LInput: String;
  LLegacy: TFRMMemoText;
begin
  FCaseName := AName;
  FPhase := 'before public dialog helper';
  Log('BEGIN ' + AName + ' parent=' + FParent.ClassName +
    ' parent_handle=' + BoolToStr(FParent.HandleAllocated, True) +
    ' chars=' + IntToStr(Length(FDetails)));
  FMode := AMode;
  if AMode = mmResult then
    FPrompt := 'Successfully executed 267 operations.' + sLineBreak + 'Total fee: 0.0267 PASC'
  else
    FPrompt := 'Execute 267 operations?' + sLineBreak +
      'Total fee: 0.0267 PASC' + sLineBreak +
      'These operations will be transmitted to the network.';
  FAction := AAction;
  FScaled := AScaled;
  FSmall := ASmall;
  FCapture := ACapture;
  FTitle := Application.Title + ' - ' + AName;
  FError := '';
  FActionSent := False;
  FObservedCount := 0;
  FDeadline := GetTickCount64 + 10000;
  FTimer.Enabled := True;
  try
    case AMode of
      mmConfirm:
        begin
          if AScaled then
            LResult := ConfirmMemoQuery(nil, FTitle, FPrompt, FDetails)
          else
            LResult := ConfirmMemoQuery(FParent, FTitle, FPrompt, FDetails);
          Require(LResult = (AAction = maYes), 'confirmation returned an incorrect approval result');
        end;
      mmResult:
        ShowResultMemo(FParent, FTitle, FPrompt, FDetails);
      mmInput:
        begin
          LInput := 'Original editable text';
          Require(not InputMemoQuery(FTitle, True, LInput), 'cancelled legacy input was accepted');
          Require(LInput = 'Original editable text', 'cancelled legacy input changed its caller value');
        end;
      mmLegacyReadOnly:
        begin
          LLegacy := TFRMMemoText.Create(FParent);
          try
            LLegacy.InitData(FTitle, 'Legacy readonly information');
            LLegacy.ShowModal;
          finally
            LLegacy.Free;
          end;
        end;
    end;
  finally
    FTimer.Enabled := False;
  end;
  Require(FError = '', FError);
  Require(FObservedCount = 1, 'expected exactly one actual dialog lifetime');
  Require(FActionSent, 'no native dialog action was dispatched');
  Require(not Assigned(FObserved), 'dialog outlived its public helper');
  Require(not TNode.NodeExists, 'a message-only check unexpectedly created a node');
  Log('PASS ' + AName);
end;

procedure TOperationMessageChecks.Run;
begin
  Log('Native operation messages: 267 long lines; no wallet, signing, broadcast or clipboard writes.');
  try
    RunCase('confirm-no-100', mmConfirm, maNo, False, False, True);
    RunCase('confirm-default-enter-100', mmConfirm, maEnter, False, False, False);
    RunCase('confirm-escape-150', mmConfirm, maEscape, True, False, False);
    RunCase('confirm-close-150', mmConfirm, maClose, True, False, False);
    RunCase('confirm-yes-100', mmConfirm, maYes, False, False, False);
    RunCase('confirm-yes-150', mmConfirm, maYes, True, False, True);
    RunCase('confirm-small-owned-150', mmConfirm, maNo, True, True, True);
    RunCase('result-close-100', mmResult, maNo, False, False, True);
    RunCase('result-enter-150', mmResult, maEnter, True, False, True);
    RunCase('legacy-input-cancel', mmInput, maClose, False, False, False);
    RunCase('legacy-readonly-close', mmLegacyReadOnly, maNo, False, False, False);
    Log('PASS all 11 native operation-message cases');
  except
    on E: Exception do
    begin
      LogException(E);
      Log('FAIL ' + E.ClassName + ': ' + E.Message);
      raise;
    end;
  end;
end;

procedure CheckWalletOperationMessages(AParent: TForm; const ALogPath: String);
var
  LBag: TDisposables;
  LChecks: TOperationMessageChecks;
  LHost: TOperationMessageHost;
{$IFDEF FPC}
  LPreviousFrameCount: LongInt;
{$ENDIF}
begin
  if TNode.NodeExists then
    raise Exception.Create('Operation message checks require a node-free host');
  if not Assigned(AParent) then
  begin
    if not Assigned(Application.MainForm) then
    begin
      Application.CreateForm(TOperationMessageHost, LHost);
      AParent := LHost;
    end
    else
      AParent := Application.MainForm;
  end;
  LChecks := LBag.AddObject(TOperationMessageChecks.Create(AParent, ALogPath)) as TOperationMessageChecks;
{$IFDEF FPC}
  // Keep the complete native creation caller chain in this owned QA process.
  LPreviousFrameCount := System.RaiseMaxFrameCount;
  System.RaiseMaxFrameCount := 64;
  try
{$ENDIF}
    LChecks.Run;
{$IFDEF FPC}
  finally
    System.RaiseMaxFrameCount := LPreviousFrameCount;
  end;
{$ENDIF}
end;

end.
