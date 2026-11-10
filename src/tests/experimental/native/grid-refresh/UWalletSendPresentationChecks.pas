unit UWalletSendPresentationChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Exercises actual operation-wizard routes on fresh offline data, captures native
  screens at two font scales, and always cancels before Finish or signing. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses UWalletGridMocks;

procedure CheckSendPascPresentation(AMain: TMockMainForm; const ALogPath: String);
procedure CheckChangeAccountInfoPresentation(AMain: TMockMainForm; const ALogPath: String);
procedure CheckChangeAccountInfoApproval(AMain: TMockMainForm; const ALogPath: String);

implementation

uses Windows, Messages, Classes, SysUtils, Types, Math, Forms, Controls, Graphics,
  StdCtrls, ExtCtrls, Buttons, Menus, {$IFDEF FPC}LMessages,{$ENDIF}
  UMemory, UNode, UBlockChain, UFileStorage, UAccounts, UWallet, UConst,
  UUserInterface, UCoreObjects, UCoreUtils, UWizard, UVisualGrid, UWIZSendPASC,
  UWIZOperationSelected, UWIZSendPASC_Details, UWIZOperationFee_Custom,
  UWIZOperationPayload_Encryption, UWIZOperationPayload_Password,
  UWIZOperationPayload_Content, UWIZOperationConfirmation, UWIZOperationSigner_Select,
  UWIZOperation, UWIZChangeAccountInfo, UWIZChangeAccountInfo_Details,
  UWIZChangeAccountInfo_Skipped, UCommon, UBaseTypes, UPCDataTypes, UCommon.Collections;

type
  TControlFontAccess = class(TControl)
  public
    property Font;
    property Color;
    {$IFDEF FPC}
    property BaseBounds;
    {$ENDIF}
  end;

  TGroupBoxContentAccess = class(TGroupBox);

  TNativePixel = packed record
    Blue, Green, Red: Byte;
  end;
  TNativePixelRow = array[0..32767] of TNativePixel;
  PNativePixelRow = ^TNativePixelRow;

  TSendCase = (scDefault, scCustomFee, scPayloadPlain, scPayloadSender,
    scPayloadRecipient, scPayloadPassword);
  TOperationWizard = TWizard<TWIZOperationsModel>;
  TInfoApprovalCase = (iacMixed, iacAllSkipped, iacNoSkipped, iacSingleName, iacFeeSelf, iacFeeCommon);

  TApprovalSafeBox = class(TPCSafeBox)
  public
    procedure BuildFunded(const AKey: TAccountKey);
  end;

  TApprovalStorage = class(TFileStorage)
  protected
    function GetFirstBlockNumber: Int64; override;
    function GetLastBlockNumber: Int64; override;
    function DoLoadBlockChain(AOperations: TPCOperationsComp; ABlock: Cardinal): Boolean; override;
  end;

  TSendPresentationChecks = class(TComponent)
  private
    FMain: TMockMainForm;
    FTimer: TTimer;
    FLog: TStringList;
    FLogPath, FError: String;
    FCase: TSendCase;
    FDeadline: UInt64;
    FCancelled, FInside: Boolean;
    FChangeAccountInfo: Boolean;
    FInfoApproval: Boolean;
    FInfoCase: TInfoApprovalCase;
    FExpectedAccountCount: Integer;
    FPreviousException: TExceptionEvent;
    procedure Require(ACondition: Boolean; const AMessage: String);
    procedure Log(const AMessage: String);
    procedure Pump(AMilliseconds: Cardinal);
    procedure OnException(Sender: TObject; E: Exception);
    procedure Tick(Sender: TObject);
    procedure LoadGenesis;
    procedure BuildApprovalBank;
    procedure Advance(AWizard: TOperationWizard);
    procedure CheckScreen(AWizard: TOperationWizard; AClass: TComponentClass; const AStep: String);
    procedure CheckLayout(AParent: TWinControl; ACanvas: TCanvas; const ACheckpoint: String);
    procedure CheckGridPainting(AGrid: TVisualGrid; const ACheckpoint: String);
    procedure LogGeometry(AControl: TWinControl; const ACheckpoint: String);
    procedure CheckText(AControl: TControl; ACanvas: TCanvas; const ACheckpoint: String);
    procedure RunFlow(AWizard: TWIZSendPASCWizard);
    procedure RunChangeInfoFlow(AWizard: TWIZChangeAccountInfoWizard);
    procedure RunApprovalFlow(AWizard: TWIZChangeAccountInfoWizard);
    procedure CheckInfoConfirmation(AWizard: TWIZChangeAccountInfoWizard; const AStep: String);
    procedure CloseOwnedWizard;
    function IsExpectedWizard(AOwner: TComponent): Boolean;
    function CaseName: String;
    function PaintNativeForm(AForm: TForm): TBitmap;
  public
    constructor Create(AMain: TMockMainForm; const ALogPath: String); reintroduce;
    destructor Destroy; override;
    procedure Run;
    procedure RunChangeAccountInfo;
    procedure RunChangeInfoApproval;
  end;

procedure TApprovalSafeBox.BuildFunded(const AKey: TAccountKey);
var
  I: Integer;
  LBlock: TOperationBlock;
begin
  // Existing native fixtures build a mature in-memory bank through protected
  // AddNew, without bypassing account eligibility or touching real wallet data.
  for I := 0 to 159 do
  begin
    LBlock := CT_OperationBlock_NUL;
    LBlock.block := I;
    LBlock.account_key := AKey;
    LBlock.protocol_version := CT_PROTOCOL_1;
    LBlock.protocol_available := CT_PROTOCOL_6;
    LBlock.compact_target := CT_MinCompactTarget_v1;
    LBlock.timestamp := I * CT_NewLineSecondsAvg;
    LBlock.reward := TPascalCoinProtocol.GetRewardForNewLine(I);
    AddNew(LBlock);
  end;
  DoUpgradeToProtocol5;
  DoUpgradeToProtocol6;
end;

function TApprovalStorage.GetFirstBlockNumber: Int64;
begin
  Result := 0;
end;

function TApprovalStorage.GetLastBlockNumber: Int64;
begin
  Result := Int64(Bank.BlocksCount) - 1;
end;

function TApprovalStorage.DoLoadBlockChain(AOperations: TPCOperationsComp; ABlock: Cardinal): Boolean;
begin
  Result := ABlock < Bank.BlocksCount;
  if Result then
  begin
    AOperations.Clear(True);
    AOperations.SetOperationBlock(Bank.SafeBox.GetBlockInfo(ABlock));
  end;
end;

function PaintNativeControl(AControl: TWinControl): TBitmap;
var
  LDC: HDC;
begin
  Result := TBitmap.Create;
  try
    Result.PixelFormat := pf24bit;
    Result.SetSize(AControl.ClientWidth, AControl.ClientHeight);
    Result.Canvas.Lock;
    try
      {$IFDEF FPC}
      Result.Canvas.Brush.Color := AControl.GetRGBColorResolvingParent;
      {$ELSE}
      Result.Canvas.Brush.Color := TControlFontAccess(AControl).Color;
      {$ENDIF}
      Result.Canvas.FillRect(AControl.ClientRect);
      LDC := Result.Canvas.Handle;
      {$IFDEF FPC}
      AControl.Perform(LM_ERASEBKGND, WPARAM(LDC), 0);
      AControl.Perform(LM_PAINT, WPARAM(LDC), 0);
      {$ELSE}
      AControl.PaintTo(LDC, 0, 0);
      {$ENDIF}
      if AControl is TCustomComboBox then
        SendMessage(AControl.Handle, WM_PRINTCLIENT, WPARAM(LDC), PRF_CLIENT);
    finally
      Result.Canvas.Unlock;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TSendPresentationChecks.LogGeometry(AControl: TWinControl; const ACheckpoint: String);
var
  LClient, LWindow: TRect;
  LDC: HDC;
  LPPI: Integer;
  {$IFDEF FPC}LBase: TRect;{$ENDIF}
begin
  LClient := AControl.ClientRect;
  LWindow := AControl.BoundsRect;
  LPPI := 0;
  if AControl.HandleAllocated then begin
    GetClientRect(AControl.Handle, LClient);
    GetWindowRect(AControl.Handle, LWindow);
    LDC := GetDC(AControl.Handle);
    if LDC <> 0 then begin
      LPPI := GetDeviceCaps(LDC, LOGPIXELSY);
      ReleaseDC(AControl.Handle, LDC);
    end;
  end;
  Log(Format('GEOMETRY %s %s:%s bounds=%d,%d,%d,%d logical_client=%d,%d native_client=%d,%d,%d,%d native_window=%d,%d,%d,%d align=%d constraints=%d,%d,%d,%d font_height=%d device_ppi=%d',
    [ACheckpoint, AControl.Name, AControl.ClassName, AControl.Left, AControl.Top,
     AControl.Width, AControl.Height, AControl.ClientWidth, AControl.ClientHeight,
     LClient.Left, LClient.Top, LClient.Right, LClient.Bottom,
     LWindow.Left, LWindow.Top, LWindow.Right, LWindow.Bottom,
     Ord(AControl.Align), AControl.Constraints.MinWidth, AControl.Constraints.MinHeight,
     AControl.Constraints.MaxWidth, AControl.Constraints.MaxHeight,
     TControlFontAccess(AControl).Font.Height, LPPI]));
  {$IFDEF FPC}
  LBase := TControlFontAccess(AControl).BaseBounds;
  Log(Format('BASE %s %s=%d,%d,%d,%d', [ACheckpoint, AControl.Name,
    LBase.Left, LBase.Top, LBase.Right, LBase.Bottom]));
  {$ENDIF}
end;

constructor TSendPresentationChecks.Create(AMain: TMockMainForm; const ALogPath: String);
begin
  inherited Create(nil);
  FMain := AMain;
  FExpectedAccountCount := 1;
  FLogPath := ALogPath;
  FLog := TStringList.Create;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 20;
  FTimer.OnTimer := Tick;
  FPreviousException := Application.OnException;
  Application.OnException := OnException;
end;

destructor TSendPresentationChecks.Destroy;
begin
  FTimer.Enabled := False;
  Application.OnException := FPreviousException;
  FLog.Free;
  inherited Destroy;
end;

procedure TSendPresentationChecks.Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise Exception.Create(AMessage);
end;

procedure TSendPresentationChecks.Log(const AMessage: String);
begin
  FLog.Add(FormatDateTime('hh:nn:ss.zzz', Now) + ' ' + AMessage);
  FLog.SaveToFile(FLogPath);
end;

function TSendPresentationChecks.CaseName: String;
begin
  if FInfoApproval then
  begin
    case FInfoCase of
      iacMixed: Result := 'mixed';
      iacAllSkipped: Result := 'all-skipped';
      iacNoSkipped: Result := 'no-skipped';
      iacSingleName: Result := 'single-name';
      iacFeeSelf: Result := 'fee-self';
      iacFeeCommon: Result := 'fee-common';
    end;
    Exit;
  end;
  if FChangeAccountInfo then begin
    if FExpectedAccountCount = 1 then
      Result := 'single'
    else
      Result := 'batch';
    Exit;
  end;
  case FCase of
    scDefault: Result := 'default';
    scCustomFee: Result := 'custom-fee';
    scPayloadPlain: Result := 'payload-plain';
    scPayloadSender: Result := 'payload-sender';
    scPayloadRecipient: Result := 'payload-recipient';
    scPayloadPassword: Result := 'payload-password';
  end;
end;

procedure TSendPresentationChecks.Pump(AMilliseconds: Cardinal);
var
  LUntil: UInt64;
begin
  LUntil := GetTickCount64 + AMilliseconds;
  repeat
    Application.ProcessMessages;
    CheckSynchronize(0);
    Require(FError = '', FError);
    Sleep(1);
  until GetTickCount64 >= LUntil;
end;

procedure TSendPresentationChecks.CloseOwnedWizard;
var
  I: Integer;
  LMessage: String;
begin
  for I := Screen.FormCount - 1 downto 0 do
    if (Screen.Forms[I] is TWizardHostForm) and
      IsExpectedWizard(Screen.Forms[I].Owner) then
      TOperationWizard(Screen.Forms[I].Owner).Cancel(LMessage);
end;

function TSendPresentationChecks.IsExpectedWizard(AOwner: TComponent): Boolean;
begin
  if FChangeAccountInfo then
    Result := AOwner is TWIZChangeAccountInfoWizard
  else
    Result := AOwner is TWIZSendPASCWizard;
end;

procedure TSendPresentationChecks.OnException(Sender: TObject; E: Exception);
begin
  if FError = '' then
    FError := E.ClassName + ': ' + E.Message;
  CloseOwnedWizard;
end;

procedure TSendPresentationChecks.LoadGenesis;
var
  LBag: TDisposables;
  LStream: TFileStream;
  LBlock: TPCOperationsComp;
  LCount: Cardinal;
  LMessage: String;
  LUntil: UInt64;
begin
  Require(TUserInterface.Offline, 'Send presentation requires the private offline startup');
  TNode.Node.BroadcastData := False;
  TNode.Node.UpdateBlockchain := False;
  Require(TNode.Node.Bank.BlocksCount = 0, 'Send presentation requires fresh empty data');
  TNode.Node.Bank.StorageClass := TFileStorage;
  LStream := LBag.AddObject(TFileStream.Create(ExtractFilePath(ParamStr(0)) +
    'mainnet-blocks-0-4.bin', fmOpenRead or fmShareDenyNone)) as TFileStream;
  LBlock := LBag.AddObject(TPCOperationsComp.Create(nil)) as TPCOperationsComp;
  LStream.ReadBuffer(LCount, SizeOf(LCount));
  Require(LCount = 5, 'Unexpected public genesis fixture');
  LBlock.Bank := TNode.Node.Bank;
  Require(LBlock.LoadBlockFromStream(LStream, LMessage), LMessage);
  Require(LBlock.OperationBlock.block = 0, 'Expected public genesis block');
  Require(TNode.Node.AddNewBlockChain(nil, LBlock, LMessage), LMessage);
  LUntil := GetTickCount64 + 8000;
  while not TNode.Node.GetSynchronizationStatus.PendingOperationsAvailable and
    (GetTickCount64 < LUntil) do
    Pump(10);
  Require(TNode.Node.GetSynchronizationStatus.PendingOperationsAvailable,
    'Public genesis pending state did not settle');
  Require(TWallet.Keys.Count = 0, 'Send presentation must never use an existing wallet key');
  TWallet.GenerateNewKey('Disposable native Send UI QA key', CT_Default_EC_OpenSSL_NID);
  Require(TNode.Node.GetAccount(0).balance > 10001, 'Genesis sender lacks display-test funds');
  Log('GENESIS public block 0 validated; fresh generated wallet key; offline, no broadcast');
end;

procedure TSendPresentationChecks.BuildApprovalBank;
begin
  Require(TUserInterface.Offline, 'Approval checks require private offline startup');
  Require((TNode.Node.Bank.BlocksCount = 0) and (TWallet.Keys.Count = 0), 'Approval bank must start empty');
  TNode.Node.BroadcastData := False;
  TNode.Node.UpdateBlockchain := False;
  TWallet.GenerateNewKey('Disposable batch approval QA key', CT_Default_EC_OpenSSL_NID);
  TNode.Node.Bank.StorageClass := TApprovalStorage;
  TApprovalSafeBox(TNode.Node.Bank.SafeBox).BuildFunded(TWallet.Keys[0].AccountKey);
  TNode.Node.Bank.UpdateValuesFromSafebox;
  Pump(100);
  Require(TNode.Node.Bank.BlocksCount = 160, 'Approval bank did not reach account maturity');
  Require(not TAccountComp.IsAccountBlockedByProtocol(0, TNode.Node.Bank.BlocksCount),
    'Approval fixture must use eligible accounts, not bypass signer validation');
  Log('BANK mature generated local bank; fresh owned key; no network, signing or broadcast');
end;

function TSendPresentationChecks.PaintNativeForm(AForm: TForm): TBitmap;
{$IFDEF FPC}
var
  LCanvasDC: HDC;

  procedure PaintControl(AControl: TWinControl);
  var
    LSaved, I: Integer;
    LOrigin, LWindowOrigin: TPoint;
    LWindow: TRect;
  begin
    if not AControl.HandleAllocated or not IsWindowVisible(AControl.Handle) then Exit;
    LSaved := SaveDC(LCanvasDC);
    try
      GetWindowRect(AControl.Handle, LWindow);
      LOrigin := AForm.ScreenToClient(LWindow.TopLeft);
      LWindowOrigin := LOrigin;
      SetViewportOrgEx(LCanvasDC, LOrigin.X, LOrigin.Y, nil);
      SendMessage(AControl.Handle, WM_PRINT, WPARAM(LCanvasDC), PRF_NONCLIENT);
      LOrigin := AForm.ScreenToClient(AControl.ClientToScreen(Types.Point(0, 0)));
      SetViewportOrgEx(LCanvasDC, LOrigin.X, LOrigin.Y, nil);
      if AControl is TCustomGroupBox then
        // LCL's logical client excludes the caption/frame, but its real paint
        // handler also draws that native frame outside the logical client.
        IntersectClipRect(LCanvasDC, LWindowOrigin.X - LOrigin.X, LWindowOrigin.Y - LOrigin.Y,
          LWindowOrigin.X - LOrigin.X + LWindow.Right - LWindow.Left,
          LWindowOrigin.Y - LOrigin.Y + LWindow.Bottom - LWindow.Top)
      else
        IntersectClipRect(LCanvasDC, 0, 0, AControl.ClientWidth, AControl.ClientHeight);
      AControl.Perform(LM_ERASEBKGND, WPARAM(LCanvasDC), 0);
      AControl.Perform(LM_PAINT, WPARAM(LCanvasDC), 0);
      // GroupBox's LM_PAINT already maps its inset LCL origin to Win32's full
      // window; printing it again here would shift its frame and graphic labels.
      if not (AControl is TCustomControl) and not (AControl is TCustomForm) and
        not (AControl is TCustomGroupBox) then
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
  {$IFNDEF FPC}
  // GetFormImage locks its canvas throughout recursive VCL painting, keeping
  // temporary child canvases from recycling the destination native DC.
  Result := AForm.GetFormImage;
  Result.PixelFormat := pf24bit;
  {$ELSE}
  Result := TBitmap.Create;
  try
    Result.PixelFormat := pf24bit;
    Result.SetSize(AForm.ClientWidth, AForm.ClientHeight);
    // Same native widget/graphic-child path verified by FrameworkDemoPagingCapture.
    Result.Canvas.Brush.Color := AForm.GetRGBColorResolvingParent;
    Result.Canvas.FillRect(AForm.ClientRect);
    LCanvasDC := Result.Canvas.Handle;
    PaintControl(AForm);
  except
    Result.Free;
    raise;
  end;
  {$ENDIF}
end;

procedure TSendPresentationChecks.CheckGridPainting(AGrid: TVisualGrid; const ACheckpoint: String);
var
  LBag: TDisposables;
  LBitmap, LModeBitmap: TBitmap;
  LRect: TRect;
  LBackground: TNativePixel;
  LRow: PNativePixelRow;
  LMode: TBitBtn;
  LMenu: TPopupMenu;
  X, Y, I, LInk: Integer;
begin
  Require(AGrid.InternalDrawGrid.HandleAllocated and
    (AGrid.InternalDrawGrid.ColCount >= 2) and (AGrid.InternalDrawGrid.RowCount >= 2),
    ACheckpoint + ': native grid geometry has no header/account row');
  LBitmap := LBag.AddObject(PaintNativeControl(AGrid.InternalDrawGrid)) as TBitmap;
  LBitmap.SaveToFile(ChangeFileExt(FLogPath, '.' + ACheckpoint + '-native-grid.bmp'));
  Require(LBitmap.Width <= High(TNativePixelRow) + 1, 'Native grid exceeds pixel-check bounds');
  for I := 0 to FExpectedAccountCount do begin
    LRect := AGrid.InternalDrawGrid.CellRect(0, I);
    Require((LRect.Left >= 0) and (LRect.Top >= 0) and
      (LRect.Right <= LBitmap.Width) and (LRect.Bottom <= LBitmap.Height) and
      (LRect.Right - LRect.Left > 8) and (LRect.Bottom - LRect.Top > 4),
      ACheckpoint + ': native header/account cell is clipped');
    InflateRect(LRect, -3, -2);
    LRow := LBitmap.ScanLine[LRect.Bottom - 1];
    LBackground := LRow^[LRect.Right - 1];
    LInk := 0;
    for Y := LRect.Top to LRect.Bottom - 1 do begin
      LRow := LBitmap.ScanLine[Y];
      for X := LRect.Left to LRect.Right - 1 do
        if (Abs(Integer(LRow^[X].Red) - LBackground.Red) +
          Abs(Integer(LRow^[X].Green) - LBackground.Green) +
          Abs(Integer(LRow^[X].Blue) - LBackground.Blue)) > 90 then
          Inc(LInk);
    end;
    Log(Format('GRID PIXELS %s row=%d ink=%d rect=%d,%d,%d,%d',
      [ACheckpoint, I, LInk, LRect.Left, LRect.Top, LRect.Right, LRect.Bottom]));
    Require(LInk >= 3, ACheckpoint + ': native header/account cell painted no text');
  end;
  Require(AGrid.FindComponent('PageModeButton') is TBitBtn, 'Native grid mode button is missing');
  Require(AGrid.FindComponent('PageModeMenu') is TPopupMenu, 'Native grid mode menu is missing');
  LMode := TBitBtn(AGrid.FindComponent('PageModeButton'));
  LMenu := TPopupMenu(AGrid.FindComponent('PageModeMenu'));
  Require((LMode.Width = LMode.Height) and (LMode.Caption = '') and not LMode.Glyph.Empty,
    ACheckpoint + ': paging selector must remain a square icon button');
  Require(LMenu.Items.Count = 3, ACheckpoint + ': paging menu lost a mode');
  for I := 0 to 2 do begin
    Require(LMenu.Items[I].Tag = I, ACheckpoint + ': paging menu has the wrong mode tag');
    Require(LMenu.Items[I].Checked = (I = Ord(AGrid.PageMode)),
      ACheckpoint + ': paging menu check does not reflect the grid mode');
    case I of
      0: Require(LMenu.Items[I].Caption = 'Single', 'Missing Single paging item');
      1: Require(LMenu.Items[I].Caption = 'Auto', 'Missing Auto paging item');
      2: Require(LMenu.Items[I].Caption = 'Manual', 'Missing Manual paging item');
    end;
  end;
  Require(Pos(LMenu.Items[Ord(AGrid.PageMode)].Caption, LMode.Hint) > 0,
    ACheckpoint + ': paging button hint lost its current mode');
  LModeBitmap := LBag.AddObject(PaintNativeControl(LMode)) as TBitmap;
  LModeBitmap.SaveToFile(ChangeFileExt(FLogPath, '.' + ACheckpoint + '-native-mode.bmp'));
  LRect := Types.Rect(3, 3, LModeBitmap.Width - 3, LModeBitmap.Height - 3);
  Require((LRect.Right > LRect.Left) and (LRect.Bottom > LRect.Top),
    ACheckpoint + ': paging button has no native icon interior');
  LRow := LModeBitmap.ScanLine[LRect.Top];
  LBackground := LRow^[LRect.Left];
  LInk := 0;
  for Y := LRect.Top to LRect.Bottom - 1 do begin
    LRow := LModeBitmap.ScanLine[Y];
    for X := LRect.Left to LRect.Right - 1 do
      if (Abs(Integer(LRow^[X].Red) - LBackground.Red) +
        Abs(Integer(LRow^[X].Green) - LBackground.Green) +
        Abs(Integer(LRow^[X].Blue) - LBackground.Blue)) > 90 then
        Inc(LInk);
  end;
  Require(LInk >= 3, ACheckpoint + ': native paging button painted no icon');
  Log(Format('GRID MODE PIXELS %s icon_ink=%d', [ACheckpoint, LInk]));
  Log(Format('GRID MODE %s entries=%d checked=%d button=%dx%d hint=%s',
    [ACheckpoint, LMenu.Items.Count, Ord(AGrid.PageMode), LMode.Width, LMode.Height, LMode.Hint]));
end;

procedure TSendPresentationChecks.CheckText(AControl: TControl; ACanvas: TCanvas; const ACheckpoint: String);
var
  LText: String;
  LRect: TRect;
  LFlags: Cardinal;
  LExtra, LCanvasHeight, LCharY: Integer;
  LDC: HDC;
  LFont, LSavedFont: HGDIOBJ;
  LMetrics: TTextMetric;
  LClient, LFormat: TRect;
  LPosition: LRESULT;
begin
  LExtra := 0;
  LFlags := DT_CALCRECT or DT_NOPREFIX;
  if AControl is TLabel then begin
    LText := TLabel(AControl).Caption;
    if TLabel(AControl).WordWrap then
      LFlags := LFlags or DT_WORDBREAK;
  end else if AControl is TCheckBox then begin
    LText := TCheckBox(AControl).Caption;
    LExtra := GetSystemMetrics(SM_CXMENUCHECK) + 6;
  end else if AControl is TRadioButton then begin
    LText := TRadioButton(AControl).Caption;
    LExtra := GetSystemMetrics(SM_CXMENUCHECK) + 6;
  end else if AControl is TEdit then begin
    ACanvas.Font.Assign(TControlFontAccess(AControl).Font);
    LCanvasHeight := ACanvas.TextHeight('Mg');
    LDC := GetDC(TEdit(AControl).Handle);
    Require(LDC <> 0, 'Cannot inspect the native edit text metrics');
    try
      LFont := HGDIOBJ(SendMessage(TEdit(AControl).Handle, WM_GETFONT, 0, 0));
      LSavedFont := 0;
      if LFont <> 0 then
        LSavedFont := SelectObject(LDC, LFont);
      try
        Require(GetTextMetrics(LDC, LMetrics), 'Cannot inspect the native edit font');
      finally
        if LSavedFont <> 0 then
          SelectObject(LDC, LSavedFont);
      end;
      GetClientRect(TEdit(AControl).Handle, LClient);
      LFormat := LClient;
      SendMessage(TEdit(AControl).Handle, EM_GETRECT, 0, LPARAM(@LFormat));
      LPosition := SendMessage(TEdit(AControl).Handle, EM_POSFROMCHAR, 0, 0);
      LCharY := (LPosition shr 16) and $FFFF;
      if LCharY > High(SmallInt) then
        Dec(LCharY, 65536);
      Log(Format('EDIT %s %s text=%s font_height=%d font_size=%d canvas_height=%d native_height=%d ascent=%d descent=%d leading=%d client=%d,%d,%d,%d format=%d,%d,%d,%d character_y=%d outer_height=%d',
        [ACheckpoint, AControl.Name, TEdit(AControl).Text,
         TEdit(AControl).Font.Height, TEdit(AControl).Font.Size, LCanvasHeight,
         LMetrics.tmHeight, LMetrics.tmAscent, LMetrics.tmDescent, LMetrics.tmInternalLeading,
         LClient.Left, LClient.Top, LClient.Right, LClient.Bottom,
         LFormat.Left, LFormat.Top, LFormat.Right, LFormat.Bottom, LCharY, AControl.Height]));
    finally
      ReleaseDC(TEdit(AControl).Handle, LDC);
    end;
    Require(AControl.Height >= LCanvasHeight + 4,
      ACheckpoint + ': edit font is clipped in ' + AControl.Name);
    Exit;
  end else
    Exit;
  if LText = '' then Exit;
  ACanvas.Font.Assign(TControlFontAccess(AControl).Font);
  LRect := Types.Rect(0, 0, Max(1, AControl.Width - LExtra), 0);
  DrawText(ACanvas.Handle, PChar(LText), Length(LText), LRect, LFlags);
  Require((LRect.Right + LExtra <= AControl.Width + 2) and
    (LRect.Bottom <= AControl.Height + 2),
    Format('%s: caption clipped in %s, text %dx%d control %dx%d',
      [ACheckpoint, AControl.Name, LRect.Right + LExtra, LRect.Bottom, AControl.Width, AControl.Height]));
end;

procedure TSendPresentationChecks.CheckLayout(AParent: TWinControl; ACanvas: TCanvas; const ACheckpoint: String);
var
  LContent, LOverlap: TRect;
  A, B: TControl;
  I, J, LCaptionWidth: Integer;
  LHeaderSummary: Boolean;
begin
  LContent := AParent.ClientRect;
  LCaptionWidth := 0;
  if AParent is TGroupBox then begin
    TGroupBoxContentAccess(AParent).AdjustClientRect(LContent);
    ACanvas.Font.Assign(TGroupBox(AParent).Font);
    LCaptionWidth := ACanvas.TextWidth(TGroupBox(AParent).Caption) + 20;
  end;
  for I := 0 to AParent.ControlCount - 1 do begin
    A := AParent.Controls[I];
    if not A.Visible then Continue;
    // Existing selected-account summaries deliberately share the caption band
    // to its right; the actual group caption and summary must not intersect.
    LHeaderSummary := (AParent is TGroupBox) and (A is TLabel) and
      (A.Top < LContent.Top) and (A.Left >= LCaptionWidth);
    Require((A.Left >= LContent.Left) and (A.Top >= LContent.Top - Ord(LHeaderSummary) * LContent.Top) and
      (A.Left + A.Width <= LContent.Right + 1) and (A.Top + A.Height <= LContent.Bottom + 1),
      Format('%s: %s exceeds %s native content (%d,%d,%d,%d in %d,%d,%d,%d)',
        [ACheckpoint, A.Name, AParent.Name, A.Left, A.Top, A.Width, A.Height,
         LContent.Left, LContent.Top, LContent.Right, LContent.Bottom]));
    CheckText(A, ACanvas, ACheckpoint);
    for J := I + 1 to AParent.ControlCount - 1 do begin
      B := AParent.Controls[J];
      if not B.Visible then Continue;
      if IntersectRect(LOverlap, A.BoundsRect, B.BoundsRect) then
        Require((LOverlap.Right - LOverlap.Left <= 1) or (LOverlap.Bottom - LOverlap.Top <= 1),
          ACheckpoint + ': overlapping controls ' + A.Name + ' and ' + B.Name);
      if (A is TWinControl) and (B is TWinControl) and TWinControl(A).TabStop and TWinControl(B).TabStop then
        Require(TWinControl(A).TabOrder <> TWinControl(B).TabOrder,
          ACheckpoint + ': duplicate keyboard tab order in ' + AParent.Name);
    end;
    if A is TVisualGrid then begin
      Require((TVisualGrid(A).RowCount = FExpectedAccountCount) and (TVisualGrid(A).ColCount >= 2) and
        TVisualGrid(A).InternalDrawGrid.Showing and IsWindowVisible(TVisualGrid(A).InternalDrawGrid.Handle),
        ACheckpoint + ': actual selected/confirmation grid is not visible and populated');
      CheckGridPainting(TVisualGrid(A), ACheckpoint);
    end else if A is TWinControl then
      CheckLayout(TWinControl(A), ACanvas, ACheckpoint);
  end;
end;

procedure TSendPresentationChecks.CheckScreen(AWizard: TOperationWizard; AClass: TComponentClass; const AStep: String);
var
  LBag: TDisposables;
  LBitmap: TBitmap;
  LHost: TCustomForm;
  LAncestor: TWinControl;
  LBounds: TRect;
  LUntil: UInt64;
  LTag: String;
  I, LScale: Integer;
  {$IFDEF FPC}LBasePPI: Integer;{$ENDIF}
  LPath: TWizardScreenPath;
begin
  Require(AWizard.CurrentScreen.InheritsFrom(AClass), CaseName + ': wrong actual route step for ' + AStep);
  LPath := AWizard.Screens;
  if not FChangeAccountInfo then
    for I := 0 to High(LPath) do
      Require(not LPath[I].InheritsFrom(TWIZOperationSigner_Select), 'Send PASC unexpectedly injected a signer step');
  LHost := GetParentForm(AWizard.CurrentScreen);
  // Hosted screens are forms themselves; walk to the real modal host.
  if LHost = AWizard.CurrentScreen then
    LHost := GetParentForm(AWizard.CurrentScreen.Parent);
  Require(LHost is TWizardHostForm, 'Send screen has no actual wizard host');
  LUntil := GetTickCount64 + 1500;
  while not AWizard.CurrentScreen.Showing and (GetTickCount64 < LUntil) do
    Pump(10);
  Require(AWizard.CurrentScreen.Showing and IsWindowVisible(AWizard.CurrentScreen.Handle), 'Send screen did not become native-visible');
  LBounds := LHost.BoundsRect;
  {$IFDEF FPC}LBasePPI := LHost.PixelsPerInch;{$ENDIF}
  LAncestor := AWizard.CurrentScreen;
  while Assigned(LAncestor) do begin
    LogGeometry(LAncestor, CaseName + '-' + AStep + '-before-scaling');
    LAncestor := LAncestor.Parent;
  end;
  for LScale := 0 to 1 do begin
    if LScale = 1 then begin
      {$IFDEF FPC}
      // Native DPI adjustment uses the docked panel's current bounds. LCL
      // ScaleBy instead scales its original 360x64 design base and collapses it.
      LHost.AutoAdjustLayout(lapAutoAdjustForDPI, LBasePPI, MulDiv(LBasePPI, 3, 2), 0, 0);
      Require(LHost.PixelsPerInch = MulDiv(LBasePPI, 3, 2), 'LCL did not adopt the requested DPI');
      {$ELSE}
      LHost.ScaleBy(3, 2);
      {$ENDIF}
    end;
    try
      Pump(50);
      LTag := CaseName + '-' + AStep + '-' + IntToStr(100 + LScale * 50);
      LAncestor := AWizard.CurrentScreen;
      while Assigned(LAncestor) do begin
        LogGeometry(LAncestor, LTag);
        LAncestor := LAncestor.Parent;
      end;
      LBitmap := LBag.AddObject(PaintNativeForm(AWizard.CurrentScreen)) as TBitmap;
      LBitmap.SaveToFile(ChangeFileExt(FLogPath, '.' + LTag + '.bmp'));
      CheckLayout(AWizard.CurrentScreen, LBitmap.Canvas, LTag);
      Log('SCREEN ' + LTag + ': visible native content, captions, bounds, no overlaps and keyboard order passed');
    finally
      if LScale = 1 then begin
        {$IFDEF FPC}
        LHost.AutoAdjustLayout(lapAutoAdjustForDPI, LHost.PixelsPerInch, LBasePPI, 0, 0);
        Require(LHost.PixelsPerInch = LBasePPI, 'LCL did not restore the original DPI');
        {$ELSE}
        LHost.ScaleBy(2, 3);
        {$ENDIF}
        LHost.BoundsRect := LBounds;
        Pump(50);
      end;
    end;
  end;
end;

procedure TSendPresentationChecks.Advance(AWizard: TOperationWizard);
var
  LMessage: String;
begin
  Require(not (AWizard.CurrentScreen is TWIZOperationConfirmation), 'QA must never invoke Finish');
  Require(AWizard.TryNext(LMessage), CaseName + ': actual Continue rejected: ' + LMessage);
end;

procedure TSendPresentationChecks.RunFlow(AWizard: TWIZSendPASCWizard);
var
  LDetails: TWIZSendPASC_Details;
  LEncryption: TWIZOperationPayload_Encryption;
  LCustom, LPayload: Boolean;
  LMessage: String;
begin
  CheckScreen(AWizard, TWIZOperationSelected, 'selected');
  Advance(AWizard);
  Require(AWizard.CurrentScreen is TWIZSendPASC_Details, 'Send route did not present Details');
  LDetails := TWIZSendPASC_Details(AWizard.CurrentScreen);
  LDetails.txtRecipient.Text := TAccountComp.AccountNumberToAccountTxtNumber(2);
  LDetails.txtAmount.Text := '1.0000';
  LCustom := FCase in [scCustomFee, scPayloadSender, scPayloadPassword];
  LPayload := FCase in [scPayloadPlain, scPayloadSender, scPayloadRecipient, scPayloadPassword];
  LDetails.chkCustomFee.Checked := LCustom;
  LDetails.chkPayload.Checked := LPayload;
  CheckScreen(AWizard, TWIZSendPASC_Details, 'details');
  Advance(AWizard);
  if LCustom then begin
    Require(AWizard.CurrentScreen is TWIZOperationFee_Custom, 'Custom fee step was not injected');
    TWIZOperationFee_Custom(AWizard.CurrentScreen).Fee := 1;
    TWIZOperationFee_Custom(AWizard.CurrentScreen).cbOverrideDefaultFee.Checked := False;
    CheckScreen(AWizard, TWIZOperationFee_Custom, 'fee');
    Advance(AWizard);
  end;
  if LPayload then begin
    Require(AWizard.CurrentScreen is TWIZOperationPayload_Encryption, 'Payload encryption step was not injected');
    LEncryption := TWIZOperationPayload_Encryption(AWizard.CurrentScreen);
    case FCase of
      scPayloadPlain: LEncryption.rbNotEncrypted.Checked := True;
      scPayloadSender: LEncryption.rbEncryptedWithSender.Checked := True;
      scPayloadRecipient: LEncryption.rbEncryptedWithRecipient.Checked := True;
      scPayloadPassword: LEncryption.rbEncryptedWithPassword.Checked := True;
    end;
    CheckScreen(AWizard, TWIZOperationPayload_Encryption, 'encryption');
    Advance(AWizard);
    if FCase = scPayloadPassword then begin
      Require(AWizard.CurrentScreen is TWIZOperationPayload_Password, 'Password step was not injected');
      TWIZOperationPayload_Password(AWizard.CurrentScreen).edtPassword.Text := 'Disposable native UI password';
      CheckScreen(AWizard, TWIZOperationPayload_Password, 'password');
      Advance(AWizard);
    end;
    Require(AWizard.CurrentScreen is TWIZOperationPayload_Content, 'Payload content step was not injected');
    TWIZOperationPayload_Content(AWizard.CurrentScreen).mmoPayload.Lines.Text := 'Native Send PASC presentation regression.';
    CheckScreen(AWizard, TWIZOperationPayload_Content, 'content');
    Advance(AWizard);
  end;
  CheckScreen(AWizard, TWIZOperationConfirmation, 'confirmation');
  Require(AWizard.Model.SendPASC.DestinationAccount.account = 2, 'Recipient changed during cosmetic navigation');
  Require(AWizard.Model.SendPASC.SingleAmountToSend = 10000, 'Amount changed during cosmetic navigation');
  Require(AWizard.Cancel(LMessage), 'Send wizard did not accept Cancel: ' + LMessage);
  Require(AWizard.State = wsCancelled, 'Send wizard did not enter cancelled state');
  FCancelled := True;
  Log('ROUTE ' + CaseName + ': actual Continue/validation/injection and Cancel passed; signer correctly unreachable');
end;

procedure TSendPresentationChecks.RunChangeInfoFlow(AWizard: TWIZChangeAccountInfoWizard);
var
  LDetails: TWIZChangeAccountInfo_Details;
  LMessage: String;
begin
  Require(AWizard.Model.Account.Count = FExpectedAccountCount, 'Unexpected real Change Account Info selection');
  CheckScreen(AWizard, TWIZOperationSelected, 'selected');
  Advance(AWizard);
  Require(AWizard.CurrentScreen is TWIZChangeAccountInfo_Details, 'Change Account Info did not present Details');
  LDetails := TWIZChangeAccountInfo_Details(AWizard.CurrentScreen);
  if FExpectedAccountCount = 1 then begin
    Require(LDetails.gbName.Enabled, 'Single-account Name group is disabled');
    LDetails.txtName.Text := 'native-info-qa';
  end else
    Require(not LDetails.gbName.Enabled, 'Batch Name group must retain its existing disabled state');
  LDetails.txtType.Text := '12345';
  LDetails.chkCustomFee.Checked := True;
  LDetails.chkPayload.Checked := True;
  CheckScreen(AWizard, TWIZChangeAccountInfo_Details, 'details');
  Require(LDetails.txtType.Text = '12345', 'Visual scaling changed the typed account type');
  Require(LDetails.chkCustomFee.Checked and LDetails.chkPayload.Checked, 'Visual scaling changed the selected options');
  if FExpectedAccountCount = 1 then
    Require(LDetails.txtName.Text = 'native-info-qa', 'Visual scaling changed the typed account name');
  // This cosmetic regression stops on Details, before validation/preparation
  // can inject signer steps or attempt any operation execution.
  Require(AWizard.Cancel(LMessage), 'Change Account Info rejected Cancel: ' + LMessage);
  Require(AWizard.State = wsCancelled, 'Change Account Info did not enter cancelled state');
  FCancelled := True;
  Log('ROUTE ' + CaseName + ': actual Selected/Details and Cancel passed; no Finish, signing or broadcast');
end;

procedure TSendPresentationChecks.CheckInfoConfirmation(AWizard: TWIZChangeAccountInfoWizard; const AStep: String);
var
  I: Integer;
  LGrid: TVisualGrid;
  LExpectedPayer: Cardinal;
  LFees: Int64;
begin
  FExpectedAccountCount := Length(AWizard.Model.ChangeAccountInfo.TargetAccounts);
  CheckScreen(AWizard, TWIZOperationConfirmation, AStep);
  LGrid := nil;
  for I := 0 to AWizard.CurrentScreen.ComponentCount - 1 do
    if AWizard.CurrentScreen.Components[I] is TVisualGrid then
      LGrid := TVisualGrid(AWizard.CurrentScreen.Components[I]);
  Require(Assigned(LGrid), 'Confirmation has no actual visual grid');
  LFees := 0;
  for I := 0 to FExpectedAccountCount - 1 do
  begin
    Require(Cardinal(LGrid.Rows[I].AccountNumber) = AWizard.Model.ChangeAccountInfo.TargetAccounts[I].account,
      'Confirmation contains a skipped account or changed target order');
    LExpectedPayer := AWizard.Model.ChangeAccountInfo.TargetAccounts[I].account;
    if AWizard.Model.ChangeAccountInfo.UseSingleFeePayer then
      LExpectedPayer := AWizard.Model.ChangeAccountInfo.FeePayerAccount.account;
    Require(String(LGrid.Rows[I].Signer) = TAccountComp.AccountNumberToAccountTxtNumber(LExpectedPayer),
      'Confirmation displays the wrong fee payer');
    Require(Int64(LGrid.Rows[I].Fee) = -AWizard.Model.Fee.SingleOperationFee,
      'Confirmation fee differs from the selected fee');
    Inc(LFees, -Int64(LGrid.Rows[I].Fee));
  end;
  Require(LFees = FExpectedAccountCount * AWizard.Model.Fee.SingleOperationFee,
    'Skipped changes contributed to confirmation fees');
end;

procedure TSendPresentationChecks.RunApprovalFlow(AWizard: TWIZChangeAccountInfoWizard);
var
  LDetails: TWIZChangeAccountInfo_Details;
  LReview: TWIZChangeAccountInfo_Skipped;
  LFee: TWIZOperationFee_Custom;
  LOriginal: TArray<TAccount>;
  LPayerA, LPayerB: TAccount;
  LPath: TWizardScreenPath;
  LMessage: String;
  I, LOriginalCount: Integer;

  procedure CheckReview(const AStep: String);
  var
    LExpectedSkipped: Integer;
    LPrefix: String;
  begin
    Require(AWizard.CurrentScreen is TWIZChangeAccountInfo_Skipped, 'Skipped accounts were not explained before continuing');
    LReview := TWIZChangeAccountInfo_Skipped(AWizard.CurrentScreen);
    LExpectedSkipped := 1;
    if FInfoCase = iacAllSkipped then LExpectedSkipped := 2;
    Require(AWizard.Model.ChangeAccountInfo.SkippedCount = LExpectedSkipped,
      'Review has the wrong number of skipped accounts');
    Require(AWizard.Model.ChangeAccountInfo.SkippedCount =
      Length(AWizard.Model.Account.SelectedAccounts) - Length(AWizard.Model.ChangeAccountInfo.TargetAccounts),
      'Review count differs from the original and filtered selections');
    if LExpectedSkipped = 1 then
      LPrefix := '1 selected account already'
    else
      LPrefix := IntToStr(LExpectedSkipped) + ' selected accounts already';
    Require(Pos(LPrefix, LReview.lblSummary.Caption) = 1, 'Review displays the wrong skipped-account count');
    Require(Pos(IntToStr(AWizard.Model.ChangeAccountInfo.NewType), LReview.lblSummary.Caption) > 0,
      'Review omits the requested account type');
    Require(Pos('No operation fee is charged for skipped changes', LReview.lblFees.Caption) > 0,
      'Review omits the skipped-fee explanation');
    Require(not LReview.chkApprove.Checked and not AWizard.Model.ChangeAccountInfo.SkippedAccountsApproved,
      'A previous approval leaked into the current review');
    CheckScreen(AWizard, TWIZChangeAccountInfo_Skipped, AStep);
    Require(not AWizard.TryNext(LMessage), 'Unchecked skip approval allowed continuation');
    Require(AWizard.CurrentScreen = LReview, 'Rejected approval left the review screen');
  end;

  procedure ApproveReview;
  begin
    LReview.chkApprove.Checked := True;
    Advance(AWizard);
    Require(AWizard.Model.ChangeAccountInfo.SkippedAccountsApproved, 'Explicit skip approval was not retained');
  end;

  procedure BackToDetails;
  var
    LSteps: Integer;
  begin
    LSteps := 0;
    while not (AWizard.CurrentScreen is TWIZChangeAccountInfo_Details) do
    begin
      Require(LSteps < 6, 'Back did not return to Details');
      Require(AWizard.TryPrevious(LMessage), 'Actual Back failed: ' + LMessage);
      Inc(LSteps);
    end;
    LDetails := TWIZChangeAccountInfo_Details(AWizard.CurrentScreen);
  end;

begin
  LOriginalCount := AWizard.Model.Account.Count;
  AWizard.Model.Account.SelectedAccounts[0].account_type := 0;
  if LOriginalCount > 1 then
    AWizard.Model.Account.SelectedAccounts[1].account_type := 1;
  LOriginal := TArrayTool<TAccount>.Copy(AWizard.Model.Account.SelectedAccounts);
  CheckScreen(AWizard, TWIZOperationSelected, 'selected');
  Advance(AWizard);
  LDetails := TWIZChangeAccountInfo_Details(AWizard.CurrentScreen);
  LDetails.chkCustomFee.Checked := FInfoCase in [iacFeeSelf, iacFeeCommon];
  LDetails.chkPayload.Checked := False;
  if FInfoCase = iacSingleName then
  begin
    LDetails.txtName.Text := 'single-info-qa';
    LDetails.txtType.Text := '0';
  end
  else if FInfoCase = iacAllSkipped then
  begin
    AWizard.Model.Account.SelectedAccounts[1].account_type := 0;
    LOriginal[1].account_type := 0;
    LDetails.txtType.Text := '0';
  end
  else if FInfoCase in [iacNoSkipped, iacFeeSelf] then
    LDetails.txtType.Text := '2'
  else
    LDetails.txtType.Text := '1';
  CheckScreen(AWizard, TWIZChangeAccountInfo_Details, 'details');
  Advance(AWizard);
  if FInfoCase in [iacMixed, iacAllSkipped, iacFeeCommon] then
  begin
    CheckReview('skip-review');
    if FInfoCase = iacAllSkipped then
    begin
      Require(Length(AWizard.Model.ChangeAccountInfo.TargetAccounts) = 0, 'All-skipped batch has an update target');
      Require(not LReview.chkApprove.Enabled, 'All-skipped review enables approval');
      LReview.chkApprove.Checked := True;
      Require(not AWizard.TryNext(LMessage), 'All-skipped batch reached a fee or confirmation step');
    end
    else
      ApproveReview;
  end;
  if FInfoCase in [iacFeeSelf, iacFeeCommon] then
  begin
    Require(AWizard.CurrentScreen is TWIZOperationFee_Custom, 'Custom fee did not use the existing fee screen');
    LFee := TWIZOperationFee_Custom(AWizard.CurrentScreen);
    LFee.cbOverrideDefaultFee.Checked := False;
    LFee.Fee := 1;
    Require(LFee.chkSingleFeePayer.Visible and not LFee.chkSingleFeePayer.Checked,
      'Change Info fee page must start with optional common payer unchecked');
    if FInfoCase = iacFeeCommon then
    begin
      LPayerA := TNode.Node.GetAccount(5);
      LPayerB := TNode.Node.GetAccount(10);
      LPayerA.balance := 3;
      LPayerB.balance := 4;
      AWizard.Model.Signer.SignerCandidates := TArray<TAccount>.Create(LPayerA, LPayerB);
      LFee.SingleFeePayerChange(nil);
      LFee.chkSingleFeePayer.Checked := True;
      LFee.SingleFeePayerChange(nil);
      Require(LFee.cbFeePayer.ItemIndex = 0, 'Multiple eligible payers were chosen without user selection');
      Require(not AWizard.TryNext(LMessage), 'Checked common-payer placeholder passed validation');
      LFee.cbFeePayer.ItemIndex := 1;
      LFee.FeePayerChange(nil);
      LFee.Fee := 2;
      Require(AWizard.Model.ChangeAccountInfo.FeePayerSelected and
        (AWizard.Model.ChangeAccountInfo.FeePayerAccount.account = 5), 'Changing fee lost a still-eligible explicit payer');
      LFee.Fee := 4;
      Require((LFee.cbFeePayer.Items.Count = 2) and (LFee.cbFeePayer.ItemIndex = 0) and
        not AWizard.Model.ChangeAccountInfo.FeePayerSelected,
        'Fee increase silently replaced the user-selected payer with another account');
      LFee.Fee := 5;
      Require((LFee.cbFeePayer.Items.Count = 1) and not AWizard.Model.ChangeAccountInfo.FeePayerSelected,
        'An ineligible payer survived a fee increase');
      Require(not AWizard.TryNext(LMessage), 'Checked common payer with no eligible account passed validation');
      LFee.chkSingleFeePayer.Checked := False;
      LFee.SingleFeePayerChange(nil);
      Require(LFee.Validate(LMessage), 'Unchecked self-pay path depends on a common-payer selection');
      LFee.Fee := 1;
      LFee.chkSingleFeePayer.Checked := True;
      LFee.SingleFeePayerChange(nil);
      Require(LFee.cbFeePayer.ItemIndex = 0, 'Invalidated payer was silently replaced');
      LFee.cbFeePayer.ItemIndex := 1;
      LFee.FeePayerChange(nil);
    end;
    CheckScreen(AWizard, TWIZOperationFee_Custom, 'fee-and-payer');
    Advance(AWizard);
  end;
  if FInfoCase <> iacAllSkipped then
  begin
    LPath := AWizard.Screens;
    for I := 0 to High(LPath) do
      Require(not LPath[I].InheritsFrom(TWIZOperationSigner_Select), 'Change Info injected a separate payer screen');
    CheckInfoConfirmation(AWizard, 'confirmation');
    if FInfoCase = iacMixed then
    begin
      BackToDetails;
      LDetails.txtType.Text := '2';
      Advance(AWizard);
      Require(AWizard.Model.ChangeAccountInfo.SkippedCount = 0, 'New type retained old skipped accounts');
      Require(not AWizard.Model.ChangeAccountInfo.SkippedAccountsApproved, 'New type retained old approval');
      LPath := AWizard.Screens;
      for I := 0 to High(LPath) do
        Require(not LPath[I].InheritsFrom(TWIZChangeAccountInfo_Skipped), 'No-skip route retained the obsolete review step');
      CheckInfoConfirmation(AWizard, 'changed-type-confirmation');
      BackToDetails;
      LDetails.txtType.Text := '0';
      Advance(AWizard);
      CheckReview('changed-type-review');
      ApproveReview;
      Require(AWizard.Model.ChangeAccountInfo.TargetAccounts[0].account = 5, 'Changed type retained the old approved target');
      CheckInfoConfirmation(AWizard, 'reapproved-confirmation');
    end;
  end;
  Require(AWizard.Model.Account.Count = LOriginalCount, 'Filtering replaced the original selection count');
  for I := 0 to High(LOriginal) do
    Require(TAccountComp.EqualAccounts(LOriginal[I], AWizard.Model.Account.SelectedAccounts[I]),
      'Filtering mutated an original selected account');
  Require(AWizard.Cancel(LMessage), 'Approval case rejected Cancel: ' + LMessage);
  Require(AWizard.State = wsCancelled, 'Approval case did not cancel');
  FCancelled := True;
  Log('APPROVAL ' + CaseName + ': target filtering, explicit consent, fee rows and Cancel passed');
end;

procedure TSendPresentationChecks.Tick(Sender: TObject);
var
  I: Integer;
  LHost: TForm;
begin
  if FInside then Exit;
  try
    Require(GetTickCount64 < FDeadline, 'Timed out waiting for the actual operation wizard');
    LHost := nil;
    for I := 0 to Screen.FormCount - 1 do
      if (Screen.Forms[I] is TWizardHostForm) and Screen.Forms[I].Visible and
        IsExpectedWizard(Screen.Forms[I].Owner) then
        LHost := Screen.Forms[I];
    if not Assigned(LHost) then Exit;
    FInside := True;
    FTimer.Enabled := False;
    if FInfoApproval then
      RunApprovalFlow(TWIZChangeAccountInfoWizard(LHost.Owner))
    else if FChangeAccountInfo then
      RunChangeInfoFlow(TWIZChangeAccountInfoWizard(LHost.Owner))
    else
      RunFlow(TWIZSendPASCWizard(LHost.Owner));
  except
    on E: Exception do begin
      if FError = '' then FError := E.ClassName + ': ' + E.Message;
      CloseOwnedWizard;
    end;
  end;
end;

procedure TSendPresentationChecks.RunChangeInfoApproval;
var
  LCase: TInfoApprovalCase;
begin
  FChangeAccountInfo := True;
  FInfoApproval := True;
  BuildApprovalBank;
  for LCase := Low(TInfoApprovalCase) to High(TInfoApprovalCase) do
  begin
    FInfoCase := LCase;
    FExpectedAccountCount := 2;
    if LCase = iacSingleName then FExpectedAccountCount := 1;
    FInside := False;
    FCancelled := False;
    FDeadline := GetTickCount64 + 10000;
    FTimer.Enabled := True;
    try
      if FExpectedAccountCount = 1 then
        TUserInterface.ShowChangeAccountInfoDialog([0])
      else
        TUserInterface.ShowChangeAccountInfoDialog([0, 5]);
    finally
      FTimer.Enabled := False;
    end;
    Require(FError = '', FError);
    Require(FCancelled, 'Approval case did not cancel');
    Require((TNode.Node.Bank.BlocksCount = 160) and (TNode.Node.MempoolOperationsCount = 0),
      'Approval UI submitted an operation or changed the bank');
  end;
  Log('PASS native Change Info approval: six cases, skip consent, filtered fees, optional payer, Back/type changes, 100/150 percent; no Finish, signing or broadcast');
end;

procedure TSendPresentationChecks.RunChangeAccountInfo;
var
  LCount: Integer;
begin
  FChangeAccountInfo := True;
  LoadGenesis;
  for LCount := 1 to 2 do begin
    FExpectedAccountCount := LCount;
    FInside := False;
    FCancelled := False;
    FDeadline := GetTickCount64 + 6000;
    FTimer.Enabled := True;
    try
      if FExpectedAccountCount = 1 then
        TUserInterface.ShowChangeAccountInfoDialog([0])
      else
        TUserInterface.ShowChangeAccountInfoDialog([0, 1]);
    finally
      FTimer.Enabled := False;
    end;
    Require(FError = '', FError);
    Require(FCancelled, 'Change Account Info did not cancel its Details screen');
    Require((TNode.Node.Bank.BlocksCount = 1) and (TNode.Node.MempoolOperationsCount = 0),
      'Change Account Info cosmetic checks changed core state');
  end;
  Log('PASS native Change Account Info presentation: single/batch Selected and Details at 100/150 percent; no Finish, signing or broadcast');
end;

procedure TSendPresentationChecks.Run;
var
  LCase: TSendCase;
begin
  LoadGenesis;
  for LCase := Low(TSendCase) to High(TSendCase) do begin
    FCase := LCase;
    FInside := False;
    FCancelled := False;
    FDeadline := GetTickCount64 + 6000;
    FTimer.Enabled := True;
    try
      TUserInterface.ShowSendDialog([0]);
    finally
      FTimer.Enabled := False;
    end;
    Require(FError = '', FError);
    Require(FCancelled, 'Send route did not reach and cancel its confirmation');
    Require((TNode.Node.Bank.BlocksCount = 1) and (TNode.Node.MempoolOperationsCount = 0),
      'Send cosmetic checks changed core state');
  end;
  Log('PASS native Send PASC presentation: six routes, all reachable screens at 100/150 percent; no Finish, signing or broadcast');
end;

procedure CheckSendPascPresentation(AMain: TMockMainForm; const ALogPath: String);
var
  LBag: TDisposables;
  LChecks: TSendPresentationChecks;
begin
  LChecks := LBag.AddObject(TSendPresentationChecks.Create(AMain, ALogPath)) as TSendPresentationChecks;
  LChecks.Run;
end;

procedure CheckChangeAccountInfoPresentation(AMain: TMockMainForm; const ALogPath: String);
var
  LBag: TDisposables;
  LChecks: TSendPresentationChecks;
begin
  LChecks := LBag.AddObject(TSendPresentationChecks.Create(AMain, ALogPath)) as TSendPresentationChecks;
  LChecks.RunChangeAccountInfo;
end;

procedure CheckChangeAccountInfoApproval(AMain: TMockMainForm; const ALogPath: String);
var
  LBag: TDisposables;
  LChecks: TSendPresentationChecks;
begin
  LChecks := LBag.AddObject(TSendPresentationChecks.Create(AMain, ALogPath)) as TSendPresentationChecks;
  LChecks.RunChangeInfoApproval;
end;

end.
