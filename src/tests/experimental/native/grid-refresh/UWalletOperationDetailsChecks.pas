unit UWalletOperationDetailsChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Exercises the real wallet grid, input handlers and explicit Details action.
  Only the modal boundary is recorded; synthetic history is never looked up in
  the real node, signed or submitted. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

procedure CheckWalletOperationDetails(const ALogPath: String);

implementation

uses
  Windows, Messages, Classes, SysUtils, Types, Math, Variants, Generics.Collections,
  Forms, Controls, StdCtrls, ExtCtrls, Graphics, Buttons,
  {$IFDEF FPC}Menus{$ELSE}Vcl.Menus{$ENDIF}, ActnList, Grids,
  {$IFDEF FPC}LMessages,{$ENDIF} UMemory, UVisualGrid,
  UCTRLWallet, UWalletGridMocks, UDataSources, UBlockChain, UPCDataTypes, UCrypto,
  UVisualGrid.Paging.Checks;

type
  TDetailsGridAccess = class(TVisualGrid)
  public
    function FooterDescription: String;
  end;

  TDetailsOperationsSource = class(TMockAccountsOperationsDataSource)
  public
    procedure FetchAll(const AContainer: TList<TOperationResume>); override;
  end;

  TDetailsWallet = class(TCTRLWallet)
  private
    FModel: TMockWalletData;
    FDetailsCount: Integer;
    FLastDetailsHash: String;
  protected
    function CreateAccountsDataSource: TMyAccountsDataSource; override;
    function CreateOperationsDataSource: TAccountsOperationsDataSource; override;
    procedure ShowOperationDetails(const AOperationHash: String); override;
  public
    constructor CreateForModel(AOwner: TComponent; AModel: TMockWalletData);
    property AccountsGrid;
    property OperationsGrid;
    property DetailsCount: Integer read FDetailsCount;
    property LastDetailsHash: String read FLastDetailsHash;
  end;

  TOperationDetailsChecks = class(TComponent)
  private
    FHost: TForm;
    FWallet: TDetailsWallet;
    FModel: TMockWalletData;
    FLog: TStringList;
    FLogPath, FError: String;
    FPreviousException: TExceptionEvent;
    FPopupTimer: TTimer;
    FPopupWindow: HWND;
    FPopupDeadline: UInt64;
    FPopupDispatched: Boolean;
    procedure Require(AValue: Boolean; const AMessage: String);
    procedure Log(const AMessage: String);
    procedure OnException(Sender: TObject; E: Exception);
    procedure Pump(AMilliseconds: Cardinal);
    procedure AwaitRows;
    procedure ClickRow(ARow: Integer; ARightButton: Boolean = False);
    procedure CheckPassiveInput(const AScale: String);
    procedure CheckExplicitDetails(const AScale: String);
    procedure CheckUnavailable(const AScale: String);
    procedure CheckToolbarBounds(const AScale: String);
    procedure PopupTick(Sender: TObject);
    function SelectedHash: String;
    function DetailsButton: TBitBtn;
    function DetailsAction: TAction;
    procedure Capture(const ASuffix: String);
  public
    constructor Create(const ALogPath: String); reintroduce;
    destructor Destroy; override;
    procedure Run;
  end;

function TDetailsGridAccess.FooterDescription: String;
var
  LClient: Windows.TRect;
begin
  Windows.GetClientRect(FBottomPanel.Handle, LClient);
  Result := Format('footer logical=%d,%d,%d,%d nativeClient=%d,%d navigation=%d,%d,%d,%d mode=%d,%d,%d,%d fontHeight=%d fontDpi=%d',
    [FBottomPanel.Left, FBottomPanel.Top, FBottomPanel.Width, FBottomPanel.Height,
     LClient.Right - LClient.Left, LClient.Bottom - LClient.Top,
     FBottomRightPanel.Left, FBottomRightPanel.Top, FBottomRightPanel.Width, FBottomRightPanel.Height,
     FPageModeButton.Left, FPageModeButton.Top, FPageModeButton.Width, FPageModeButton.Height,
     FBottomPanel.Font.Height, FBottomPanel.Font.PixelsPerInch]);
end;

procedure TDetailsOperationsSource.FetchAll(const AContainer: TList<TOperationResume>);
var
  LOperation: TOperationResume;
  LAccount, LCounter: Cardinal;
  I: Integer;
begin
  inherited;
  // Valid, distinct hash syntax passes through the real typed source adapter.
  // It identifies fixture rows only; the modal seam prevents real-node lookup.
  for I := 0 to AContainer.Count - 1 do begin
    LOperation := AContainer[I];
    LAccount := LOperation.AffectedAccount;
    LCounter := 100 + I;
    LOperation.n_operation := LCounter;
    SetLength(LOperation.OperationHash, 32);
    FillChar(LOperation.OperationHash[0], 32, I + 1);
    Move(LOperation.Block, LOperation.OperationHash[0], 4);
    Move(LAccount, LOperation.OperationHash[4], 4);
    Move(LCounter, LOperation.OperationHash[8], 4);
    if I = AContainer.Count - 1 then
      SetLength(LOperation.OperationHash, 0);
    AContainer[I] := LOperation;
  end;
end;

constructor TDetailsWallet.CreateForModel(AOwner: TComponent; AModel: TMockWalletData);
begin
  FModel := AModel;
  inherited Create(AOwner);
end;

function TDetailsWallet.CreateAccountsDataSource: TMyAccountsDataSource;
begin
  Result := TMockMyAccountsDataSource.CreateForModel(Self, FModel);
end;

function TDetailsWallet.CreateOperationsDataSource: TAccountsOperationsDataSource;
begin
  Result := TDetailsOperationsSource.CreateForModel(Self, FModel);
  TDetailsOperationsSource(Result).OperationsPerAccount := 8;
end;

procedure TDetailsWallet.ShowOperationDetails(const AOperationHash: String);
begin
  Inc(FDetailsCount);
  FLastDetailsHash := AOperationHash;
end;

function FindOwnedDetailsPopup(AWindow: HWND; AData: LPARAM): BOOL; stdcall;
const
  CT_MN_GETHMENU = $01E1;
var
  LChecks: TOperationDetailsChecks;
  LClassName: array[0..31] of Char;
begin
  Result := True;
  LChecks := TOperationDetailsChecks(Pointer(AData));
  if (GetClassName(AWindow, @LClassName[0], Length(LClassName)) <> 0) and
    (StrPas(PChar(@LClassName[0])) = '#32768') and
    (NativeUInt(SendMessage(AWindow, CT_MN_GETHMENU, 0, 0)) =
      NativeUInt(LChecks.FWallet.mnuOperationsPopup.Handle)) then begin
    LChecks.FPopupWindow := AWindow;
    Result := False;
  end;
end;

constructor TOperationDetailsChecks.Create(const ALogPath: String);
begin
  inherited Create(nil);
  FLogPath := ALogPath;
  FLog := TStringList.Create;
  FPreviousException := Application.OnException;
  Application.OnException := OnException;
  FPopupTimer := TTimer.Create(Self);
  FPopupTimer.Enabled := False;
  FPopupTimer.Interval := 20;
  FPopupTimer.OnTimer := PopupTick;
end;

destructor TOperationDetailsChecks.Destroy;
begin
  FPopupTimer.Enabled := False;
  Application.OnException := FPreviousException;
  FHost.Free;
  FLog.Free;
  inherited Destroy;
end;

procedure TOperationDetailsChecks.Require(AValue: Boolean; const AMessage: String);
begin
  if not AValue then raise Exception.Create(AMessage);
end;

procedure TOperationDetailsChecks.Log(const AMessage: String);
begin
  FLog.Add(AMessage);
  FLog.SaveToFile(ChangeFileExt(FLogPath, '.wallet-operation-details.log'));
end;

procedure TOperationDetailsChecks.OnException(Sender: TObject; E: Exception);
begin
  if FError = '' then FError := E.ClassName + ': ' + E.Message;
  EndMenu;
end;

procedure TOperationDetailsChecks.Pump(AMilliseconds: Cardinal);
var
  LUntil: UInt64;
begin
  LUntil := GetTickCount64 + AMilliseconds;
  repeat
    Application.ProcessMessages;
    CheckSynchronize(0);
    if FError <> '' then raise Exception.Create(FError);
    Sleep(1);
  until GetTickCount64 >= LUntil;
end;

procedure TOperationDetailsChecks.AwaitRows;
var
  LUntil: UInt64;
begin
  LUntil := GetTickCount64 + 8000;
  repeat
    Pump(5);
  until (not FWallet.OperationsGrid.IsUpdating and
    (FWallet.OperationsGrid.RowCount = 40)) or (GetTickCount64 >= LUntil);
  Require(not FWallet.OperationsGrid.IsUpdating and
    (FWallet.OperationsGrid.RowCount = 40), 'Details fixture did not publish all forty provider rows');
end;

function TOperationDetailsChecks.DetailsButton: TBitBtn;
begin
  Require(FWallet.FindComponent('OperationDetailsButton') is TBitBtn,
    'The wallet has no explicit operation Details button');
  Result := TBitBtn(FWallet.FindComponent('OperationDetailsButton'));
end;

function TOperationDetailsChecks.DetailsAction: TAction;
begin
  Require(FWallet.FindComponent('OperationDetailsAction') is TAction,
    'The wallet has no shared operation Details action');
  Result := TAction(FWallet.FindComponent('OperationDetailsAction'));
end;

function TOperationDetailsChecks.SelectedHash: String;
var
  LRows: TArray<Variant>;
begin
  LRows := FWallet.OperationsGrid.SelectedRows;
  Require(Length(LRows) = 1, 'The native input did not leave exactly one operation selected');
  Result := VarToStr(LRows[0].__KEY);
  Require(TPCOperation.IsValidOperationHash(Result), 'Provider did not publish a valid operation hash');
end;

procedure TOperationDetailsChecks.ClickRow(ARow: Integer; ARightButton: Boolean);
var
  LGrid: TDrawGrid;
  LCell: TRect;
  LPoint: LPARAM;
begin
  LGrid := FWallet.OperationsGrid.InternalDrawGrid;
  LGrid.TopRow := ARow + 1;
  LCell := LGrid.CellRect(0, ARow + 1);
  Require((LCell.Top >= 0) and (LCell.Bottom <= LGrid.ClientHeight),
    'Requested native operation cell is not visible');
  LPoint := LPARAM(Cardinal(((LCell.Left + LCell.Right) div 2) and $FFFF) or
    (Cardinal(((LCell.Top + LCell.Bottom) div 2) and $FFFF) shl 16));
  if ARightButton then begin
    SendMessage(LGrid.Handle, WM_RBUTTONDOWN, MK_RBUTTON, LPoint);
    SendMessage(LGrid.Handle, WM_RBUTTONUP, 0, LPoint);
  end else begin
    SendMessage(LGrid.Handle, WM_LBUTTONDOWN, MK_LBUTTON, LPoint);
    SendMessage(LGrid.Handle, WM_LBUTTONUP, 0, LPoint);
  end;
  Pump(25);
end;

procedure TOperationDetailsChecks.CheckPassiveInput(const AScale: String);
var
  LBefore, LBeforeRow, LBeforeTop: Integer;
  LGrid: TDrawGrid;
  LPoint: TPoint;
  LPointParam: LPARAM;
  LHash: String;
begin
  LBefore := FWallet.DetailsCount;
  FWallet.OperationsGrid.ClearSelection(True);
  ClickRow(0);
  LHash := SelectedHash;
  Require(DetailsButton.Enabled, 'Selecting a valid operation did not enable Details');
  Require(FWallet.DetailsCount = LBefore, 'An ordinary operation click opened Details');
  ClickRow(1);
  Require(SelectedHash <> LHash, 'Second native click did not select a distinct operation');
  Require(FWallet.DetailsCount = LBefore, 'Selecting a second operation opened Details');
  LGrid := FWallet.OperationsGrid.InternalDrawGrid;
  LGrid.SetFocus;
  LBeforeRow := LGrid.Row;
  SendMessage(LGrid.Handle, WM_KEYDOWN, VK_DOWN, 1);
  SendMessage(LGrid.Handle, WM_KEYUP, VK_DOWN, LPARAM($C0000001));
  Pump(25);
  Require(LGrid.Row = LBeforeRow + 1, 'Native Down key did not change operation selection');
  Require(FWallet.DetailsCount = LBefore, 'Keyboard operation navigation opened Details');
  LBeforeTop := LGrid.TopRow;
  LBeforeRow := LGrid.Row;
  LPoint := LGrid.ClientToScreen(Types.Point(LGrid.ClientWidth div 2, LGrid.ClientHeight div 2));
  LPointParam := LPARAM(Cardinal(LPoint.X and $FFFF) or (Cardinal(LPoint.Y and $FFFF) shl 16));
  SendMessage(LGrid.Handle, WM_MOUSEWHEEL, WPARAM(Cardinal($FF880000)), LPointParam);
  Pump(25);
  Require((LGrid.TopRow > LBeforeTop) or (LGrid.Row > LBeforeRow),
    'Native wheel did not scroll or advance operation selection');
  Require(FWallet.DetailsCount = LBefore, 'Mouse-wheel operation navigation opened Details');
  Log('PASS ' + AScale + ': native row clicks, keyboard and wheel make no Details requests');
end;

procedure TOperationDetailsChecks.PopupTick(Sender: TObject);
var
  LMenu: HMENU;
  LCommand, LState: Cardinal;
  LOwner: HWND;
  LProcess: DWORD;
  LCaption: array[0..127] of Char;
  LIndex: Integer;
begin
  if GetTickCount64 >= FPopupDeadline then begin
    FError := 'Timed out observing the actual operation Details popup';
    FPopupTimer.Enabled := False;
    EndMenu;
    Exit;
  end;
  if FPopupDispatched then Exit;
  EnumThreadWindows(GetCurrentThreadId, @FindOwnedDetailsPopup, LPARAM(Self));
  if FPopupWindow = 0 then Exit;
  try
    LMenu := FWallet.mnuOperationsPopup.Handle;
    LIndex := FWallet.miOperationInfo.MenuIndex;
    LCommand := GetMenuItemID(LMenu, LIndex);
    Require(LCommand = FWallet.miOperationInfo.Command, 'Details native command does not match its menu item');
    LState := GetMenuState(LMenu, LIndex, MF_BYPOSITION);
    Require((LState <> High(Cardinal)) and ((LState and (MF_DISABLED or MF_GRAYED)) = 0),
      'The native Details command is disabled for a valid selection');
    Require(GetMenuString(LMenu, LIndex, @LCaption[0], Length(LCaption), MF_BYPOSITION) > 0,
      'The native Details command has no caption');
    // VCL may assign a native accelerator ("&Details"). Compare the visible
    // caption through the toolkit's own mnemonic parser, retaining exact text.
    Require(StripHotkey(StrPas(PChar(@LCaption[0]))) = 'Details',
      'The context-menu action is not named Details: ' + StrPas(PChar(@LCaption[0])));
    LOwner := {$IFDEF FPC}Application.Handle{$ELSE}Vcl.Menus.PopupList.Window{$ENDIF};
    Require((LOwner <> 0) and
      (GetWindowThreadProcessId(LOwner, @LProcess) = GetCurrentThreadId) and
      (LProcess = GetCurrentProcessId), 'Details menu command owner is outside the test');
    FPopupDispatched := True;
    FPopupTimer.Enabled := False;
    // Dispatch through the toolkit's actual native menu-command window. No
    // process-wide input or switch to the user's desktop is needed.
    EndMenu;
    SendMessage(LOwner, WM_COMMAND, WPARAM(LCommand), 0);
  except
    on E: Exception do OnException(Self, E);
  end;
end;

procedure TOperationDetailsChecks.CheckExplicitDetails(const AScale: String);
var
  LBefore: Integer;
  LHash: String;
begin
  FWallet.OperationsGrid.ClearSelection(True);
  ClickRow(0);
  LHash := SelectedHash;
  LBefore := FWallet.DetailsCount;
  SendMessage(DetailsButton.Handle, BM_CLICK, 0, 0);
  Pump(25);
  Require((FWallet.DetailsCount = LBefore + 1) and (FWallet.LastDetailsHash = LHash),
    'The explicit Details button did not request the selected operation exactly once');
  Require(SelectedHash = LHash, 'Details button cleared or changed the selection');
  FPopupWindow := 0;
  FPopupDispatched := False;
  FPopupDeadline := GetTickCount64 + 3000;
  FPopupTimer.Enabled := True;
  try
    ClickRow(0, True);
  finally
    FPopupTimer.Enabled := False;
    EndMenu;
  end;
  Require(FPopupDispatched, 'Right-click did not show and dispatch the native Details menu');
  Require((FWallet.DetailsCount = LBefore + 2) and (FWallet.LastDetailsHash = LHash),
    'The native Details menu did not request the selected operation exactly once');
  Require(SelectedHash = LHash, 'The Details context menu cleared or changed the selection');
  Log('PASS ' + AScale + ': native Details button and right-click command each request the correct hash once; selection retained');
end;

procedure TOperationDetailsChecks.CheckUnavailable(const AScale: String);
var
  LBefore: Integer;
  LAdmissionDeadline: UInt64;
begin
  LBefore := FWallet.DetailsCount;
  FWallet.OperationsGrid.ClearSelection(True);
  Require(not DetailsButton.Enabled and not DetailsAction.Enabled,
    'Details remains enabled with no operation selection');
  FWallet.miOperationInfoClick(Self);
  Require(FWallet.DetailsCount = LBefore, 'Details opened without an operation selection');
  ClickRow(39);
  Require(not DetailsButton.Enabled and not FWallet.miOperationInfo.Enabled,
    'A row without a valid operation hash enabled Details');
  FWallet.miOperationInfoClick(Self);
  Require(FWallet.DetailsCount = LBefore, 'Details opened for an invalid operation hash');
  FWallet.OperationsGrid.ClearSelection(True);
  ClickRow(0);
  FWallet.OperationsGrid.ShowLoading('Please Wait', 'Synchronizing');
  try
    // Native action updates run during the application idle cycle; invoke the
    // same update contract while the controlled test pumps messages explicitly.
    DetailsAction.Update;
    Require(not DetailsButton.Enabled and not FWallet.miOperationInfo.Enabled,
      'Loading did not disable both Details entry points');
    FWallet.miOperationInfoClick(Self);
    Require(FWallet.DetailsCount = LBefore, 'Details opened while the grid was loading');
  finally
    FWallet.OperationsGrid.HideLoading;
  end;
  DetailsAction.Update;
  Require(DetailsButton.Enabled, 'Details did not become available after loading ended');
  FModel.GateFetches := True;
  try
    FWallet.OperationsGrid.RefreshGrid;
    DetailsAction.Update;
    Require(not DetailsButton.Enabled and not FWallet.miOperationInfo.Enabled,
      'Quiet-period admission left stale operation Details enabled');
    // The configured quiet-period throttle owns admission; pump its timer
    // before asserting the held worker state, without bypassing production.
    LAdmissionDeadline := GetTickCount64+2000;
    while not FWallet.OperationsGrid.IsUpdating and (GetTickCount64<LAdmissionDeadline) do Pump(5);
    Require(FWallet.OperationsGrid.IsUpdating, 'Controlled operation page did not start a worker after quiet-period admission');
    DetailsAction.Update;
    Require(not DetailsButton.Enabled and not FWallet.miOperationInfo.Enabled,
      'A running page fetch did not disable both Details entry points');
    FWallet.miOperationInfoClick(Self);
    Require(FWallet.DetailsCount = LBefore, 'Details opened while its page was fetching');
  finally
    FModel.GateFetches := False;
    FModel.ReleaseFetches;
  end;
  AwaitRows;
  DetailsAction.Update;
  Require(DetailsButton.Enabled, 'Details did not become available when its fetch completed');
  Log('PASS ' + AScale + ': no selection, invalid hash, manual loading and active fetching disable Details and prevent dispatch');
end;

procedure TOperationDetailsChecks.CheckToolbarBounds(const AScale: String);
var
  LButton: TBitBtn;
  LHistory: TComboBox;
  LButtonRect, LHistoryRect: Types.TRect;
begin
  LButton := DetailsButton;
  Require(FWallet.OperationsGrid.FindComponent('OperationsHistory') is TComboBox,
    'The original history picker is missing from the operations toolbar');
  LHistory := TComboBox(FWallet.OperationsGrid.FindComponent('OperationsHistory'));
  Require((LButton.Parent = LHistory.Parent) and
    (FWallet.OperationsGrid.WidgetControl = LButton.Parent),
    'History and Details are not in the same supported widget region');
  Require((LButton.Left >= 0) and (LButton.Top >= 0) and
    (LButton.Left + LButton.Width <= LButton.Parent.ClientWidth) and
    (LButton.Top + LButton.Height <= LButton.Parent.ClientHeight),
    'The Details button is clipped by its native parent');
  Require(LHistory.Left + LHistory.Width <= LButton.Left,
    'The Details button overlaps the history picker');
  Require((LButton.Height=LHistory.Height) and (LButton.Width=LButton.Height),
    'Details must be square and match the native history picker height');
  Require(GetWindowRect(LButton.Handle,LButtonRect) and GetWindowRect(LHistory.Handle,LHistoryRect),
    'Cannot inspect native history/Details rectangles');
  Require((LButtonRect.Top=LHistoryRect.Top) and (LButtonRect.Bottom=LHistoryRect.Bottom),
    'History and Details native top/bottom edges are not aligned');
  Require(not LButton.Glyph.Empty, 'The Details button has no visible information glyph');
  Log('PASS ' + AScale + ': Details icon and history picker fit without overlap');
end;

procedure TOperationDetailsChecks.Capture(const ASuffix: String);
var
  LBag: TDisposables;
  LBitmap: TBitmap;
  LBackground: TColor;
  LInk, LIconInk, X, Y: Integer;
  LIconOrigin: TPoint;
  {$IFDEF FPC}
  LCanvasDC: HDC;

  procedure PaintNativeControl(AControl: TWinControl);
  var
    LSaved, I: Integer;
    LOrigin, LWindowOrigin: TPoint;
    LWindow: TRect;
  begin
    if not AControl.HandleAllocated or not IsWindowVisible(AControl.Handle) then Exit;
    LSaved := SaveDC(LCanvasDC);
    try
      GetWindowRect(AControl.Handle, LWindow);
      LWindowOrigin := FHost.ScreenToClient(LWindow.TopLeft);
      SetViewportOrgEx(LCanvasDC, LWindowOrigin.X, LWindowOrigin.Y, nil);
      SendMessage(AControl.Handle, WM_PRINT, WPARAM(LCanvasDC), PRF_NONCLIENT);
      LOrigin := FHost.ScreenToClient(AControl.ClientToScreen(Types.Point(0, 0)));
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
      for I := 0 to AControl.ControlCount - 1 do
        if AControl.Controls[I] is TWinControl then
          PaintNativeControl(TWinControl(AControl.Controls[I]));
    finally
      RestoreDC(LCanvasDC, LSaved);
    end;
  end;
  {$ENDIF}
begin
  {$IFDEF FPC}
  LBitmap := LBag.AddObject(TBitmap.Create) as TBitmap;
  LBitmap.PixelFormat := pf24bit;
  LBitmap.SetSize(FHost.ClientWidth, FHost.ClientHeight);
  LBitmap.Canvas.Brush.Color := FHost.GetRGBColorResolvingParent;
  LBitmap.Canvas.FillRect(FHost.ClientRect);
  LCanvasDC := LBitmap.Canvas.Handle;
  // Direct LCL form PaintTo can return black on a non-input desktop. Paint
  // each actual native child into the initialized client bitmap instead.
  PaintNativeControl(FHost);
  {$ELSE}
  LBitmap := LBag.AddObject(FHost.GetFormImage) as TBitmap;
  {$ENDIF}
  LBackground := ColorToRGB(LBitmap.Canvas.Pixels[0, 0]);
  LInk := 0;
  for Y := 0 to (LBitmap.Height - 1) div 4 do
    for X := 0 to (LBitmap.Width - 1) div 4 do
      if ColorToRGB(LBitmap.Canvas.Pixels[X * 4, Y * 4]) <> LBackground then Inc(LInk);
  Require(LInk > 100, 'Details capture is blank');
  LIconOrigin := FHost.ScreenToClient(DetailsButton.ClientToScreen(Types.Point(0, 0)));
  LIconInk := 0;
  for Y := LIconOrigin.Y to LIconOrigin.Y + DetailsButton.ClientHeight - 1 do
    for X := LIconOrigin.X to LIconOrigin.X + DetailsButton.ClientWidth - 1 do begin
      Require(ColorToRGB(LBitmap.Canvas.Pixels[X, Y]) <> ColorToRGB(clFuchsia),
        'Details icon exposes a magenta transparency mask');
      if ColorToRGB(LBitmap.Canvas.Pixels[X, Y]) = ColorToRGB(clNavy) then Inc(LIconInk);
    end;
  Require(LIconInk >= 8, 'The native Details button did not paint its information icon');
  LBitmap.SaveToFile(ChangeFileExt(FLogPath, '.wallet-operation-details-' + ASuffix + '.bmp'));
end;

procedure TOperationDetailsChecks.Run;
var
  LOriginalBounds: TRect;

  procedure CheckScale(const AScale: String);
  var
    LScalePpi: Integer;
  begin
    AwaitRows;
    LScalePpi := StrToInt(AScale) * 96 div 100;
    CheckVisualGridFooter(FWallet.AccountsGrid, LScalePpi, FLog);
    CheckVisualGridFooter(FWallet.OperationsGrid, LScalePpi, FLog);
    Log('GEOMETRY ' + AScale + ': ' + TDetailsGridAccess(FWallet.OperationsGrid).FooterDescription);
    CheckPassiveInput(AScale);
    CheckExplicitDetails(AScale);
    CheckUnavailable(AScale);
    CheckToolbarBounds(AScale);
    Capture(AScale);
  end;

begin
  FHost := TForm.CreateNew(nil);
  FHost.Caption := 'Wallet operation Details verification';
  FHost.Font.Name := 'Tahoma';
  FHost.Font.Height := -11;
  FHost.SetBounds(30, 30, 1120, 610);
  FModel := TMockWalletData.Create(FHost);
  FModel.SetData(7501, 5);
  FWallet := TDetailsWallet.CreateForModel(FHost, FModel);
  FWallet.Font.Assign(FHost.Font);
  FWallet.Parent := FHost;
  FWallet.Align := alClient;
  FWallet.OperationsGrid.PageMode := vpmSingle;
  FHost.Show;
  FWallet.Show;
  CheckScale('100');
  LOriginalBounds := FHost.BoundsRect;
  FHost.ScaleBy(5, 4);
  try
    CheckScale('125');
  finally
    FHost.ScaleBy(4, 5);
    FHost.BoundsRect := LOriginalBounds;
  end;
  FHost.ScaleBy(3, 2);
  try
    CheckScale('150');
  finally
    FHost.ScaleBy(2, 3);
    FHost.BoundsRect := LOriginalBounds;
  end;
  Log('PASS main-wallet Details: selection is passive; only explicit actions open details');
end;

procedure CheckWalletOperationDetails(const ALogPath: String);
var
  LBag: TDisposables;
  LChecks: TOperationDetailsChecks;
begin
  LChecks := LBag.AddObject(TOperationDetailsChecks.Create(ALogPath)) as TOperationDetailsChecks;
  LChecks.Run;
end;

end.
