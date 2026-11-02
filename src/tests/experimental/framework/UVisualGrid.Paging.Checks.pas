unit UVisualGrid.Paging.Checks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>.
  Shared native paging checks for Delphi/VCL and Lazarus/LCL. The source and
  controls are real; only the in-memory records and a worker gate are supplied. }

{$IFDEF FPC}{$mode delphi}{$H+}{$ENDIF}

interface

uses Classes, UVisualGrid;

type
  TGridPagingScenario = (psSingleFromUnknownTotal, psModeTransitions,
    psCompleteSearchAndSort, psEmptyAndOnePage, psSuspendedLatestMode,
    psDiscardOldWorker, psDisabledPaging, psModeChangeScrollOrigin,
    psSuspendedModeScrollOrigin, psHeldModeScrollOrigin);

procedure CheckVisualGridPaging(AScenario: TGridPagingScenario; ALog: TStrings = nil);
procedure CheckVisualGridFooter(AGrid: TVisualGrid; APpi: Integer; ALog: TStrings = nil);
procedure ChooseVisualGridPageMode(AGrid: TVisualGrid; AIndex: Integer; AKeyboard: Boolean = False);

implementation

uses SysUtils, Types, Math, Variants, Forms, Controls, StdCtrls, Grids, Graphics, SyncObjs,
  Buttons, {$IFDEF FPC}Menus{$ELSE}Vcl.Menus{$ENDIF}, ExtCtrls,
  Generics.Collections, UCommon, UCommon.UI, UCommon.Data
  {$IFDEF MSWINDOWS}, Windows, Messages{$ENDIF};

type
  TPagingSource = class(TCustomDataSource<Integer>)
  protected
    function GetColumns: TDataColumns; override;
    function GetItemField({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AItem: Integer; const ABindingName: String): Variant; override;
    procedure FetchAll(const AContainer: TList<Integer>); override;
    function FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams; var ADataTable: TDataTable): TPageFetchResult; override;
  public
    Count, Calls: Integer;
    LastParams: TPageFetchParams;
    HoldNext: Boolean;
    Entered, ReleaseRead: TEvent;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TPagingGrid = class(TVisualGrid)
  public
    function ModeControl: TBitBtn;
    function ModeMenu: TPopupMenu;
    function ManualEditor: TEdit;
    function Navigator: TEdit;
    function Footer: TWinControl;
    function FirstButton: TButton;
    function LastButton: TButton;
    function NavigationPanel: TWinControl;
    function PageCountLabel: TLabel;
    function FooterControls: TArray<TControl>;
    function Total: Integer;
    function Pages: Integer;
    function Capacity: Integer;
    procedure ChooseMode(AIndex: Integer);
    procedure Search(const AText: String);
  end;

  TPagingObserver = class(TComponent)
  public
    Grid: TPagingGrid;
    WatchSingle, InvalidSinglePublication: Boolean;
    SinglePublications, ExpectedRows: Integer;
    procedure Completed(Sender: TObject);
  end;

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

{$IFDEF MSWINDOWS}
type
  TNativePagingHost = class(TForm)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TNativePageModePicker = class(TComponent)
  private
    FButton: TBitBtn;
    FMenu: TPopupMenu;
    FTimer: TTimer;
    FIndex, FCommands: Integer;
    FWindow, FCommandOwner: HWND;
    FNativeMenu: NativeUInt;
    FNativeCommand: Cardinal;
    FDispatched, FTimedOut: Boolean;
    FCallbackError: String;
    FDeadline: UInt64;
    FPrevious: TNotifyEvent;
    procedure Tick(Sender: TObject);
    procedure Selected(Sender: TObject);
  public
    constructor Create(AButton: TBitBtn; AMenu: TPopupMenu; AIndex: Integer); reintroduce;
    destructor Destroy; override;
    procedure Run(AKeyboard: Boolean);
  end;

constructor TNativePagingHost.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
end;

procedure EnsureNativePagingHost;
var LHost: TNativePagingHost;
begin
  if Assigned(Application.MainForm) then Exit;
  // LCL's native popup callback requires a real MainForm in a console runner.
  Application.CreateForm(TNativePagingHost,LHost);
end;

function FindOwnedModePopup(AWindow: HWND; AData: LPARAM): BOOL; stdcall;
const CT_MN_GETHMENU = $01E1; // winuser.h; used by VCL native menu styles.
var LPicker: TNativePageModePicker; LClass: array[0..31] of Char;
begin
  Result := True;
  LPicker := TNativePageModePicker(Pointer(AData));
  try
    if GetClassName(AWindow,@LClass[0],Length(LClass)) = 0 then Exit;
    if StrPas(PChar(@LClass[0])) = '#32768' then begin
      LPicker.FNativeMenu := NativeUInt(SendMessage(AWindow,CT_MN_GETHMENU,0,0));
      if LPicker.FNativeMenu = NativeUInt(LPicker.FMenu.Handle) then begin
        LPicker.FWindow := AWindow;
        Result := False;
      end;
    end;
  except
    on E: Exception do begin
      LPicker.FCallbackError := E.ClassName+': '+E.Message;
      Result := False;
    end;
  end;
end;

constructor TNativePageModePicker.Create(AButton: TBitBtn; AMenu: TPopupMenu; AIndex: Integer);
begin
  inherited Create(nil);
  FButton := AButton;
  FMenu := AMenu;
  FIndex := AIndex;
  FPrevious := FMenu.Items[FIndex].OnClick;
  FMenu.Items[FIndex].OnClick := Selected;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 20;
  FTimer.OnTimer := Tick;
end;

destructor TNativePageModePicker.Destroy;
begin
  FTimer.Enabled := False;
  FMenu.Items[FIndex].OnClick := FPrevious;
  inherited Destroy;
end;

procedure TNativePageModePicker.Selected(Sender: TObject);
begin
  Inc(FCommands);
  if Assigned(FPrevious) then FPrevious(Sender);
end;

procedure TNativePageModePicker.Tick(Sender: TObject);
var I: Integer; LState: Cardinal; LProcess: DWORD; LCaption: array[0..127] of Char;
begin
  if GetTickCount64 >= FDeadline then begin
    FTimedOut := True;
    FTimer.Enabled := False;
    EndMenu;
    Exit;
  end;
  if FDispatched then Exit;
  EnumThreadWindows(GetCurrentThreadId,@FindOwnedModePopup,LPARAM(Self));
  if FCallbackError <> '' then begin
    FTimer.Enabled := False;
    EndMenu;
    Exit;
  end;
  if FWindow = 0 then Exit;
  try
    Require(GetMenuItemCount(FMenu.Handle) = 3,'Native popup must contain exactly three modes');
    for I := 0 to FMenu.Items.Count-1 do begin
      LState := GetMenuState(FMenu.Handle,I,MF_BYPOSITION);
      Require(LState <> High(Cardinal),'Native popup item state is unavailable');
      Require(((LState and MF_CHECKED) <> 0) = FMenu.Items[I].Checked,
        'Native menu check mark does not match its current mode');
    end;
    FNativeCommand := GetMenuItemID(FMenu.Handle,FIndex);
    Require(FNativeCommand = FMenu.Items[FIndex].Command,'Native mode command ID does not match its item');
    LState := GetMenuState(FMenu.Handle,FIndex,MF_BYPOSITION);
    Require((LState and (MF_DISABLED or MF_GRAYED)) = 0,'Requested native mode item is disabled');
    Require(GetMenuString(FMenu.Handle,FIndex,@LCaption[0],Length(LCaption),MF_BYPOSITION) > 0,
      'Native mode item has no caption');
    Require(StrPas(PChar(@LCaption[0])) = FMenu.Items[FIndex].Caption,
      'Native mode item caption does not match its requested item');
    // Use the registered native command owner from each toolkit's popup code.
    FCommandOwner := {$IFDEF FPC}Application.Handle{$ELSE}Vcl.Menus.PopupList.Window{$ENDIF};
    Require((FCommandOwner <> 0) and
      (GetWindowThreadProcessId(FCommandOwner,@LProcess) = GetCurrentThreadId) and
      (LProcess = GetCurrentProcessId),'Native popup command owner is not this test thread/process');
    FDispatched := True;
    FTimer.Enabled := False;
    // This is native command dispatch, not synthetic hardware selection. It
    // exercises the real HWND menu routing without touching the input desktop.
    EndMenu;
    SendMessage(FCommandOwner,WM_COMMAND,WPARAM(FNativeCommand),0);
  except
    on E: Exception do begin
      FCallbackError := E.ClassName+': '+E.Message;
      FTimer.Enabled := False;
      EndMenu;
    end;
  end;
end;

procedure TNativePageModePicker.Run(AKeyboard: Boolean);
begin
  FButton.SetFocus;
  FDeadline := GetTickCount64+2000;
  FTimer.Enabled := True;
  try
    if AKeyboard then begin
      SendMessage(FButton.Handle,WM_KEYDOWN,VK_SPACE,1 or (MapVirtualKey(VK_SPACE,0) shl 16));
      SendMessage(FButton.Handle,WM_KEYUP,VK_SPACE,1 or (MapVirtualKey(VK_SPACE,0) shl 16) or $C0000000);
    end else FButton.Click;
  finally
    FTimer.Enabled := False;
  end;
  Require(FCallbackError = '', 'Native mode command failed: '+FCallbackError);
  Require(FDispatched and not FTimedOut and (FCommands = 1),
    Format('Native mode command: dispatched=%s timeout=%s callbacks=%d wanted=%d popup=%s menu=%s owner=%s command=%d',
      [BoolToStr(FDispatched,True),BoolToStr(FTimedOut,True),FCommands,FIndex,
       IntToHex(NativeUInt(FWindow),SizeOf(NativeUInt)*2),
       IntToHex(FNativeMenu,SizeOf(NativeUInt)*2),
       IntToHex(NativeUInt(FCommandOwner),SizeOf(NativeUInt)*2),FNativeCommand]));
end;
{$ENDIF}

procedure ChooseVisualGridPageMode(AGrid: TVisualGrid; AIndex: Integer; AKeyboard: Boolean);
var LButton: TBitBtn; LMenu: TPopupMenu;
  {$IFDEF MSWINDOWS}LPicker: TNativePageModePicker;{$ENDIF}
begin
  LButton := AGrid.FindComponent('PageModeButton') as TBitBtn;
  LMenu := AGrid.FindComponent('PageModeMenu') as TPopupMenu;
  Require(Assigned(LButton) and Assigned(LMenu) and Assigned(LButton.OnClick),
    'Actual paging icon button and popup are not wired');
  Require((AIndex >= 0) and (AIndex < LMenu.Items.Count),'Mode menu index is invalid');
  {$IFDEF MSWINDOWS}
  EnsureNativePagingHost;
  LPicker := TNativePageModePicker.Create(LButton,LMenu,AIndex);
  try
    LPicker.Run(AKeyboard);
  finally
    LPicker.Free;
  end;
  {$ELSE}
  raise ENotSupportedException.Create('Native popup command checks require Windows');
  {$ENDIF}
  Require(Ord(AGrid.PageMode) = AIndex,'Native menu command selected the wrong page mode');
end;

procedure PumpFor(AMilliseconds: Cardinal);
var LUntil: UInt64;
begin
  LUntil := GetTickCount64 + AMilliseconds;
  repeat
    CheckSynchronize(0);
    Application.ProcessMessages;
    Sleep(1);
  until GetTickCount64 >= LUntil;
end;

procedure Settle(AGrid: TVisualGrid);
var LUntil: UInt64;
begin
  LUntil := GetTickCount64 + 5000;
  repeat
    PumpFor(25);
    if not AGrid.IsUpdating and not AGrid.RefreshThrottle.Waiting then begin
      PumpFor(100);
      if not AGrid.IsUpdating and not AGrid.RefreshThrottle.Waiting then Exit;
    end;
  until GetTickCount64 >= LUntil;
  raise Exception.Create('Native paging did not settle before its bounded deadline');
end;

constructor TPagingSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Count := 240;
  Entered := TEvent.Create(nil, True, False, '');
  ReleaseRead := TEvent.Create(nil, True, True, '');
end;

destructor TPagingSource.Destroy;
begin
  ReleaseRead.Free;
  Entered.Free;
  inherited Destroy;
end;

function TPagingSource.GetColumns: TDataColumns;
begin
  Result := TDataColumns.Create(TDataColumn.From('Id', True), TDataColumn.From('Name'));
end;

function TPagingSource.GetItemField({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AItem: Integer; const ABindingName: String): Variant;
begin
  if ABindingName = 'Id' then Result := AItem
  else Result := Format('record %.4d', [AItem]);
end;

procedure TPagingSource.FetchAll(const AContainer: TList<Integer>);
var I: Integer;
begin
  for I := 0 to Count - 1 do AContainer.Add(I);
end;

function TPagingSource.FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams; var ADataTable: TDataTable): TPageFetchResult;
begin
  Inc(Calls);
  LastParams := AParams;
  if HoldNext then begin
    HoldNext := False;
    Entered.SetEvent;
    Require(ReleaseRead.WaitFor(5000) = wrSignaled, 'Held native paging provider was not released');
  end;
  Result := inherited FetchPage(AParams, ADataTable);
end;

function TPagingGrid.ModeControl: TBitBtn;
begin
  Result := FPageModeButton;
end;

function TPagingGrid.ModeMenu: TPopupMenu;
begin
  Result := FPageModeMenu;
end;

function TPagingGrid.ManualEditor: TEdit;
begin
  Result := FPageSizeEdit;
end;

function TPagingGrid.Navigator: TEdit;
begin
  Result := FPageIndexEdit;
end;

function TPagingGrid.Footer: TWinControl;
begin
  Result := FBottomPanel;
end;

function TPagingGrid.FirstButton: TButton;
begin
  Result := FButtonFirst;
end;

function TPagingGrid.LastButton: TButton;
begin
  Result := FButtonLast;
end;

function TPagingGrid.NavigationPanel: TWinControl;
begin
  Result := FBottomRightPanel;
end;

function TPagingGrid.PageCountLabel: TLabel;
begin
  Result := FPageCountLabel;
end;

function TPagingGrid.FooterControls: TArray<TControl>;
begin
  Result := TArray<TControl>.Create(FAllRecordsCountLabel, FPageSizeLabel,
    FPageSizeEdit, FPageModeButton, FButtonFirst, FButtonPrevious,
    FPageIndexEdit, FPageCountLabel, FButtonNext, FButtonLast);
end;

function TPagingGrid.Total: Integer;
begin
  Result := FTotalDataCount;
end;

function TPagingGrid.Pages: Integer;
begin
  Result := FPageCount;
end;

function TPagingGrid.Capacity: Integer;
begin
  Result := ClientRowCount;
end;

procedure TPagingGrid.ChooseMode(AIndex: Integer);
begin
  ChooseVisualGridPageMode(Self,AIndex);
end;

procedure TPagingGrid.Search(const AText: String);
begin
  FSearchEdit.Text := AText;
  SearchButtonClick(Self);
end;

procedure TPagingObserver.Completed(Sender: TObject);
begin
  if WatchSingle and (Grid.PageMode = vpmSingle) then begin
    Inc(SinglePublications);
    if Grid.RowCount <> ExpectedRows then InvalidSinglePublication := True;
  end;
end;

function RowId(AGrid: TVisualGrid; AIndex: Integer): Integer;
begin
  Result := Integer(TDataRowData(AGrid.Rows[AIndex])['Id']);
end;

function EffectivelyEnabled(AControl: TControl): Boolean;
begin
  Result := True;
  while Assigned(AControl) do begin
    if not AControl.Enabled then Exit(False);
    AControl := AControl.Parent;
  end;
end;

function NativeBounds(AControl: TControl): Types.TRect;
var
  LPoint: Types.TPoint;
begin
  {$IFDEF MSWINDOWS}
  if (AControl is TWinControl) and TWinControl(AControl).HandleAllocated then begin
    Require(Windows.GetWindowRect(TWinControl(AControl).Handle, Result),
      'Cannot read an actual footer control window rectangle');
    Exit;
  end;
  {$ENDIF}
  LPoint := AControl.Parent.ClientToScreen(AControl.BoundsRect.TopLeft);
  Result := Types.Rect(LPoint.X, LPoint.Y, LPoint.X + AControl.Width,
    LPoint.Y + AControl.Height);
end;

function RectText(const ARect: Types.TRect): String;
begin
  Result := Format('%d,%d,%d,%d',[ARect.Left,ARect.Top,ARect.Right,ARect.Bottom]);
end;
function NativeClientBounds(AControl: TWinControl): Types.TRect;
var LOrigin: Types.TPoint;
begin
  LOrigin := Types.Point(0,0);
  {$IFDEF MSWINDOWS}
  if AControl.HandleAllocated then begin
    Require(Windows.GetClientRect(AControl.Handle,Result),'Cannot read native footer ancestor client bounds');
    Require(Windows.ClientToScreen(AControl.Handle,LOrigin),'Cannot locate native footer ancestor client bounds');
    Windows.OffsetRect(Result,LOrigin.X,LOrigin.Y);
    Exit;
  end;
  {$ENDIF}
  LOrigin := AControl.ClientToScreen(LOrigin);
  Result := Types.Rect(LOrigin.X,LOrigin.Y,LOrigin.X+AControl.ClientWidth,LOrigin.Y+AControl.ClientHeight);
end;
procedure CheckFooterGeometry(AGrid: TPagingGrid; APpi, ALogicalWidth: Integer; ALog: TStrings);
var
  LMode, LFirst, LRect, LOther, LFooter: Types.TRect;
  LOrigin: Types.TPoint;
  LControls: TArray<TControl>;
    LBitmap: Graphics.TBitmap;
  LGap, I, J: Integer;
  LAncestor: TWinControl;
  LAncestorClient: Types.TRect;
  LCheckpoint: String;
begin
  LCheckpoint := Format('footer PPI=%d logicalWidth=%d mode=%d',
    [APpi, ALogicalWidth, Ord(AGrid.PageMode)]);
  Require(AGrid.Footer.Visible and AGrid.ModeControl.Visible,
    LCheckpoint + ': selector must remain visible');
  LMode := NativeBounds(AGrid.ModeControl);
  Require(AGrid.ModeControl.Width = AGrid.ModeControl.Height, LCheckpoint+': mode button must stay square');
  Require(AGrid.ModeControl.Caption = '', LCheckpoint+': mode button must remain icon-only');
  Require(not AGrid.ModeControl.Glyph.Empty, LCheckpoint+': mode button glyph must be present');
  LOrigin := AGrid.Footer.ClientToScreen(Types.Point(0, 0));
  LFooter := Types.Rect(LOrigin.X, LOrigin.Y, LOrigin.X + AGrid.Footer.ClientWidth,
    LOrigin.Y + AGrid.Footer.ClientHeight);
  if AGrid.FirstButton.Visible then begin
    LFirst := NativeBounds(AGrid.FirstButton);
    LGap := LFirst.Left - LMode.Right;
    if Assigned(ALog) then
      ALog.Add(Format('%s button=%d,%d,%d,%d first=%d,%d,%d,%d gap=%d',
        [LCheckpoint, LMode.Left, LMode.Top, LMode.Right, LMode.Bottom,
         LFirst.Left, LFirst.Top, LFirst.Right, LFirst.Bottom, LGap]));
    Require((LGap > 0) and (LGap <= (8 * APpi + 48) div 96),
      LCheckpoint + ': icon button must sit immediately left of the first-page button');
    Require(Abs((LMode.Top + LMode.Bottom) - (LFirst.Top + LFirst.Bottom)) <= 2,
      LCheckpoint + ': icon button and first-page button must share a vertical center');
    Require(AGrid.ModeControl.Parent = AGrid.FirstButton.Parent,
      LCheckpoint + ': icon button and navigation must move in one native parent');
    Require(AGrid.LastButton.Visible and AGrid.Navigator.Visible,
      LCheckpoint + ': compact paging must keep its last-page and page-entry controls');
  end;
  Require(AGrid.ModeControl.Parent = AGrid.NavigationPanel,
    LCheckpoint + ': selector must retain its navigation parent, including Single');
  if Assigned(ALog) then
    ALog.Add(Format('%s footerLogical=%s footerNativeClient=%s navLogical=%s navNativeClient=%s fontHeight=%d fontPPI=%d',
      [LCheckpoint,RectText(AGrid.Footer.BoundsRect),RectText(NativeClientBounds(AGrid.Footer)),
       RectText(AGrid.NavigationPanel.BoundsRect),RectText(NativeClientBounds(AGrid.NavigationPanel)),
       AGrid.Font.Height,AGrid.Font.PixelsPerInch]));
  LControls := AGrid.FooterControls;
  for I := 0 to High(LControls) do begin
    if not LControls[I].Visible then Continue;
    LRect := NativeBounds(LControls[I]);
    Require((LRect.Right > LRect.Left) and (LRect.Bottom > LRect.Top) and
      (LRect.Left >= LFooter.Left) and (LRect.Right <= LFooter.Right) and
      (LRect.Top >= LFooter.Top) and (LRect.Bottom <= LFooter.Bottom),
      LCheckpoint + ': visible footer control is clipped: ' + LControls[I].ClassName);
    LAncestor := LControls[I].Parent;
    while Assigned(LAncestor) and (LAncestor <> AGrid.Parent) do begin
      LAncestorClient := NativeClientBounds(LAncestor);
      Require((LRect.Left >= LAncestorClient.Left) and (LRect.Top >= LAncestorClient.Top) and
        (LRect.Right <= LAncestorClient.Right) and (LRect.Bottom <= LAncestorClient.Bottom),
        LCheckpoint + Format(': footer control #%d %s [%d,%d,%d,%d] is clipped by native ancestor %s [%d,%d,%d,%d]',
          [I,LControls[I].ClassName,LRect.Left,LRect.Top,LRect.Right,LRect.Bottom,LAncestor.ClassName,
           LAncestorClient.Left,LAncestorClient.Top,LAncestorClient.Right,LAncestorClient.Bottom]));
      LAncestor := LAncestor.Parent;
    end;
    for J := I + 1 to High(LControls) do begin
      if not LControls[J].Visible then Continue;
      LOther := NativeBounds(LControls[J]);
      Require((LRect.Right <= LOther.Left) or (LOther.Right <= LRect.Left) or
        (LRect.Bottom <= LOther.Top) or (LOther.Bottom <= LRect.Top),
        LCheckpoint + Format(': footer controls overlap: #%d %s [%d,%d,%d,%d] / #%d %s [%d,%d,%d,%d]',
          [I, LControls[I].ClassName, LRect.Left, LRect.Top, LRect.Right, LRect.Bottom,
           J, LControls[J].ClassName, LOther.Left, LOther.Top, LOther.Right, LOther.Bottom]));
    end;
  end;
  if AGrid.PageCountLabel.Visible then begin
    LBitmap := Graphics.TBitmap.Create;
    try
      LBitmap.Canvas.Font.Assign(AGrid.PageCountLabel.Font);
      Require(AGrid.PageCountLabel.Width >=
        LBitmap.Canvas.TextWidth(AGrid.PageCountLabel.Caption),
        LCheckpoint + ': native page-count text is truncated');
      LBitmap.Canvas.Font.Assign(AGrid.Navigator.Font);
      Require(AGrid.Navigator.ClientWidth >=
        LBitmap.Canvas.TextWidth(AGrid.Navigator.Text) + 2,
        LCheckpoint + ': native page-entry text is truncated');
    finally
      LBitmap.Free;
    end;
  end;
end;

procedure CheckVisualGridFooter(AGrid: TVisualGrid; APpi: Integer; ALog: TStrings);
begin
  CheckFooterGeometry(TPagingGrid(AGrid),APpi,(AGrid.Width*96+APpi div 2) div APpi,ALog);
end;
procedure CheckFooterHostedFirstShow(ALog: TStrings);
const
  InitialPpis: array[0..2] of Integer = (96,120,144);
  ChangedPpis: array[0..4] of Integer = (144,120,96,120,144);
var
  LForm: TForm;
  LGroup: TGroupBox;
  LPanel: TPanel;
  LGrid: TPagingGrid;
  LSource: TPagingSource;
  LPpi, LCase, LMode, LScalePass, LNextPpi, LBeforeCalls, LWidthCase: Integer;
begin
  for LCase := Low(InitialPpis) to High(InitialPpis) do begin
    LPpi := InitialPpis[LCase];
    LForm := TForm.CreateNew(nil);
    try
      LForm.Font.Name := 'Tahoma';
      LForm.Font.Height := -11;
      LForm.ClientWidth := 340;
      LForm.ClientHeight := 330;
      LGroup := TGroupBox.Create(LForm);
      LGroup.Parent := LForm;
      LGroup.Align := alClient;
      LGroup.Caption := 'My Accounts';
      LPanel := TPanel.Create(LForm);
      LPanel.Parent := LGroup;
      LPanel.Align := alClient;
      LPanel.BevelOuter := bvNone;
      LSource := TPagingSource.Create(LForm);
      LSource.Count := 277;
      LSource.SnapshotCacheEnabled := True;
      LGrid := TPagingGrid.Create(LForm);
      LGrid.PageMode := vpmAuto;
      LGrid.FetchDataInThread := True;
      LGrid.RefreshThrottle.Mode := temNone;
      LGrid.Options := [vgoAutoHidePaging,vgoAutoHideSearchPanel];
      LGrid.AddColumn('Id').Binding := 'Id';
      LGrid.AddColumn('Name').Binding := 'Name';
      LGrid.Align := alClient;
      LSource.HoldNext := True;
      LSource.ReleaseRead.ResetEvent;
      LGrid.DataSource := LSource;
      LPanel.AddControlDockCenter(LGrid);
      if LPpi <> 96 then begin
        {$IFDEF FPC}LForm.ScaleBy(LPpi,96);{$ELSE}LForm.ScaleForPPI(LPpi);{$ENDIF}
      end;
      LForm.Show;
      try
        PumpFor(200);
        CheckFooterGeometry(LGrid,LPpi,340,ALog);
        Require(LSource.Entered.WaitFor(1000) = wrSignaled,'First-show provider was not actually held');
        LBeforeCalls := LSource.Calls;
        for LWidthCase := 0 to 1 do begin
          if LWidthCase = 0 then LForm.ClientWidth := (240*LPpi+48) div 96
          else LForm.ClientWidth := (340*LPpi+48) div 96;
          PumpFor(150);
          Require(LGrid.IsUpdating,'Busy footer fixture released its provider too early');
          Require(LSource.Calls = LBeforeCalls,'Visual footer reflow queried a busy provider');
          CheckFooterGeometry(LGrid,LPpi,LForm.ClientWidth*96 div LPpi,ALog);
          if Assigned(ALog) then ALog.Add(Format('PASS held footer resize PPI=%d width=%d sourceCalls=%d',
            [LPpi,LForm.ClientWidth,LSource.Calls]));
        end;
      finally
        LSource.ReleaseRead.SetEvent;
      end;
      Settle(LGrid);
      CheckFooterGeometry(LGrid,LPpi,340,ALog);
      for LMode := Ord(vpmSingle) to Ord(vpmManual) do begin
        LGrid.PageMode := TVisualGridPageMode(LMode);
        Settle(LGrid);
        CheckFooterGeometry(LGrid,LPpi,340,ALog);
      end;
      // Grow and shrink the same native hierarchy; a fresh form per DPI alone
      // misses stale minimum sizes and first-fetch rounding in hosted grids.
      for LScalePass := Low(ChangedPpis) to High(ChangedPpis) do begin
        LNextPpi := ChangedPpis[LScalePass];
        if LNextPpi <> LPpi then begin
          {$IFDEF FPC}LForm.ScaleBy(LNextPpi,LPpi);{$ELSE}LForm.ScaleForPPI(LNextPpi);{$ENDIF}
          LPpi := LNextPpi;
        end;
        for LMode := Ord(vpmSingle) to Ord(vpmManual) do begin
          LGrid.PageMode := TVisualGridPageMode(LMode);
          Settle(LGrid);
          CheckFooterGeometry(LGrid,LPpi,340,ALog);
        end;
      end;
    finally
      LForm.Free;
    end;
  end;
end;

procedure CheckFooterAnchoringMatrix(ALog: TStrings);
const
  Widths: array[0..6] of Integer = (340, 240, 900, 300, 240, 400, 900);
  Modes: array[0..2] of TVisualGridPageMode = (vpmAuto, vpmManual, vpmSingle);
var
  LForm: TForm;
  LGrid: TPagingGrid;
  LSource: TPagingSource;
  LPpi, LPass, LWidth, LMode: Integer;
begin
  for LPass := 0 to 2 do begin
    LPpi := 96 + 48 * LPass;
    LForm := TForm.CreateNew(nil);
    try
      LForm.ClientWidth := 900;
      LForm.ClientHeight := 560;
      LSource := TPagingSource.Create(LForm);
      // A real five-digit page count, without repeatedly rebuilding its raw rows.
      LSource.Count := 10001;
      LSource.SnapshotCacheEnabled := True;
      LGrid := TPagingGrid.Create(LForm);
      LGrid.Parent := LForm;
      LGrid.Align := alClient;
      LGrid.FetchDataInThread := False;
      LGrid.CanSearch := False;
      LGrid.ManualPageSize := 1;
      LGrid.Options := [vgoAutoHidePaging];
      LGrid.AddColumn('Id').Binding := 'Id';
      LGrid.AddColumn('Name').Binding := 'Name';
      LForm.Show;
      if LPpi <> 96 then begin
        {$IFDEF FPC}LForm.ScaleBy(LPpi, 96);{$ELSE}LForm.ScaleForPPI(LPpi);{$ENDIF}
      end;
      PumpFor(50);
      LGrid.DataSource := LSource;
      Settle(LGrid);
      for LWidth := Low(Widths) to High(Widths) do begin
        LForm.ClientWidth := (Widths[LWidth] * LPpi + 48) div 96;
        for LMode := Low(Modes) to High(Modes) do begin
          LGrid.PageMode := Modes[LMode];
          Settle(LGrid);
          CheckFooterGeometry(LGrid, LPpi, Widths[LWidth], ALog);
          if Modes[LMode] = vpmManual then begin
            LGrid.LastButton.Click;
            Settle(LGrid);
            Require((LGrid.PageIndex = 10000) and (LGrid.Navigator.Text = '10001'),
              'The compact last-page button must retain its real navigation action');
            CheckFooterGeometry(LGrid, LPpi, Widths[LWidth], ALog);
          end;
        end;
      end;
    finally
      LForm.Free;
    end;
  end;
end;

procedure CheckVisualGridPaging(AScenario: TGridPagingScenario; ALog: TStrings);
var LForm: TForm; LGrid: TPagingGrid; LSource: TPagingSource; LObserver: TPagingObserver;
  LBefore, LWidth, LPage, LSize, I: Integer; LRect, LClient: TRect;
  LPoint: TPoint;

  procedure CheckSelector;
  var LModeIndex: Integer;
  begin
    Require((LGrid.ModeMenu.Items.Count = 3) and
      (LGrid.ModeMenu.Items[0].Caption = 'Single') and
      (LGrid.ModeMenu.Items[1].Caption = 'Auto') and
      (LGrid.ModeMenu.Items[2].Caption = 'Manual'), 'Paging mode labels/order must be Single, Auto, Manual');
    for LModeIndex := 0 to 2 do
      Require(LGrid.ModeMenu.Items[LModeIndex].Checked = (LModeIndex = Ord(LGrid.PageMode)),
        'Exactly the current page mode must be checked');
    Require((LGrid.ModeControl.Caption = '') and not LGrid.ModeControl.Glyph.Empty and
      (LGrid.ModeControl.Width = LGrid.ModeControl.Height),
      'Paging selector must be a square icon button, not a text dropdown');
    Require(LGrid.ModeControl.ShowHint and
      (Pos(LGrid.ModeMenu.Items[Ord(LGrid.PageMode)].Caption,LGrid.ModeControl.Hint) > 0),
      'Paging icon tooltip must explain its current mode');
    Require(LGrid.Footer.Visible and LGrid.ModeControl.Visible,
      'The mode selector must remain reachable, including an empty or single-page result');
    LPoint := LGrid.ModeControl.Parent.ClientToScreen(LGrid.ModeControl.BoundsRect.TopLeft);
    LRect := Types.Rect(LPoint.X, LPoint.Y, LPoint.X + LGrid.ModeControl.Width,
      LPoint.Y + LGrid.ModeControl.Height);
    LPoint := LGrid.Footer.ClientToScreen(Types.Point(0, 0));
    LClient := Types.Rect(LPoint.X, LPoint.Y, LPoint.X + LGrid.Footer.ClientWidth,
      LPoint.Y + LGrid.Footer.ClientHeight);
    Require((LRect.Left >= LClient.Left) and (LRect.Right <= LClient.Right) and
      (LRect.Top >= LClient.Top) and (LRect.Bottom <= LClient.Bottom),
      'Paging selector extends outside the actual footer client rectangle');
  end;

  procedure CheckAll(AExpected: Integer);
  begin
    Require((LSource.LastParams.PageSize <= 0) and
      (LSource.LastParams.PageIndex = 0), 'Single must request all matches independently of a cached total');
    Require((LGrid.RowCount = AExpected) and (LGrid.Total = AExpected),
      'Single did not display the complete source result');
    if AExpected > 0 then
      Require((LGrid.PageIndex = 0) and (LGrid.Pages = 1), 'A complete nonempty result must occupy one page');
    Require(not LGrid.ManualEditor.Visible and not LGrid.Navigator.Visible,
      'Single must hide finite page-size and navigation controls');
    CheckSelector;
  end;

  procedure CheckModeScrollTransitions(AKind: Integer);
  var
    LFrom, LTo, LOther: TVisualGridPageMode;
    LBeforeCalls, LTopRow, LLeftCol: Integer;
    LCell: TRect;
    LCheckpoint: String;

    procedure MoveToBottomRight;
    begin
      LGrid.InternalDrawGrid.Row := LGrid.InternalDrawGrid.RowCount - 1;
      LGrid.InternalDrawGrid.TopRow := LGrid.InternalDrawGrid.RowCount - 1;
      LGrid.InternalDrawGrid.LeftCol := LGrid.InternalDrawGrid.ColCount - 1;
    end;

  begin
    LGrid.SelectionType := stRow;
    LGrid.ManualPageSize := 80;
    LGrid.Columns[1].Width := 1500;
    for LFrom := Low(TVisualGridPageMode) to High(TVisualGridPageMode) do
      for LTo := Low(TVisualGridPageMode) to High(TVisualGridPageMode) do
      begin
        if LFrom = LTo then
          Continue;
        LCheckpoint := Format('mode scroll %d>%d kind=%d', [Ord(LFrom), Ord(LTo), AKind]);
        LGrid.FetchDataInThread := False;
        LGrid.PageMode := LFrom;
        Settle(LGrid);
        if LFrom <> vpmSingle then
        begin
          LGrid.PageIndex := LGrid.Pages - 1;
          Settle(LGrid);
        end;
        MoveToBottomRight;
        Settle(LGrid);
        Require(LGrid.InternalDrawGrid.LeftCol > LGrid.InternalDrawGrid.FixedCols,
          LCheckpoint + ': fixture did not actually scroll horizontally');
        if LFrom <> vpmAuto then
          Require(LGrid.InternalDrawGrid.TopRow > LGrid.InternalDrawGrid.FixedRows,
            LCheckpoint + ': fixture did not actually scroll to later data rows');
        LTopRow := LGrid.InternalDrawGrid.TopRow;
        LLeftCol := LGrid.InternalDrawGrid.LeftCol;
        LBeforeCalls := LSource.Calls;
        LGrid.PageMode := LFrom;
        PumpFor(100);
        Require((LSource.Calls = LBeforeCalls) and
          (LGrid.InternalDrawGrid.TopRow = LTopRow) and
          (LGrid.InternalDrawGrid.LeftCol = LLeftCol),
          LCheckpoint + ': assigning the existing mode changed scroll or queried');
        if AKind = 1 then
        begin
          LGrid.RefreshSuspended := True;
          LGrid.PageMode := LTo;
          // Movement while an old page remains visible must not erase the
          // pending new-mode origin when the retained result is finally loaded.
          MoveToBottomRight;
          PumpFor(100);
          Require(LSource.Calls = LBeforeCalls, LCheckpoint + ': suspended mode queried');
          LGrid.RefreshSuspended := False;
        end
        else if AKind = 2 then
        begin
          LGrid.FetchDataInThread := True;
          LSource.Entered.ResetEvent;
          LSource.ReleaseRead.ResetEvent;
          LSource.HoldNext := True;
          LGrid.RefreshGrid;
          Require(LSource.Entered.WaitFor(1000) = wrSignaled,
            LCheckpoint + ': original worker did not enter');
          LOther := TVisualGridPageMode(3 - Ord(LFrom) - Ord(LTo));
          LGrid.PageMode := LOther;
          LGrid.PageMode := LTo;
          MoveToBottomRight;
          LSource.ReleaseRead.SetEvent;
        end
        else
          LGrid.PageMode := LTo;
        Settle(LGrid);
        Require((LGrid.PageMode = LTo) and (LGrid.PageIndex = 0),
          LCheckpoint + ': new mode did not publish page zero');
        Require((LGrid.InternalDrawGrid.TopRow = LGrid.InternalDrawGrid.FixedRows) and
          (LGrid.InternalDrawGrid.LeftCol = LGrid.InternalDrawGrid.FixedCols),
          Format('%s: native scroll origin is row=%d col=%d, expected row=%d col=%d',
            [LCheckpoint, LGrid.InternalDrawGrid.TopRow, LGrid.InternalDrawGrid.LeftCol,
             LGrid.InternalDrawGrid.FixedRows, LGrid.InternalDrawGrid.FixedCols]));
        Require((LGrid.RowCount > 0) and (RowId(LGrid, 0) = 0),
          LCheckpoint + ': new page does not contain the first record');
        LCell := LGrid.InternalDrawGrid.CellRect(LGrid.InternalDrawGrid.FixedCols,
          LGrid.InternalDrawGrid.FixedRows);
        Require((LCell.Left >= 0) and (LCell.Top >= 0) and
          (LCell.Bottom <= LGrid.InternalDrawGrid.ClientHeight) and
          (LCell.Right > LCell.Left) and (LCell.Bottom > LCell.Top),
          LCheckpoint + ': first native data cell is offscreen or empty');
        if ALog <> nil then
          ALog.Add('PASS ' + LCheckpoint);
      end;
  end;

begin
  LForm := TForm.CreateNew(nil);
  LSource := nil;
  try
    LForm.SetBounds(60, 70, 780, 560);
    LSource := TPagingSource.Create(LForm);
    LObserver := TPagingObserver.Create(LForm);
    LGrid := TPagingGrid.Create(LForm);
    LObserver.Grid := LGrid;
    LGrid.OnFinishedUpdating := LObserver.Completed;
    LGrid.FetchDataInThread := False;
    if AScenario = psSingleFromUnknownTotal then LGrid.PageMode := vpmSingle;
    LGrid.Parent := LForm;
    LGrid.Align := alClient;
    LGrid.ManualPageSize := 25;
    LGrid.SortMode := smSingleColumn;
    LGrid.Options := [vgoColSizing, vgoAutoHidePaging, vgoAutoHideSearchPanel,
      vgoSortDirectionAllowNone];
    with LGrid.AddColumn('Id') do begin
      Binding := 'Id';
      Width := 127;
      Filters := SORTABLE_NUMERIC_FILTER;
    end;
    with LGrid.AddColumn('Name') do begin
      Binding := 'Name';
      Width := 273;
      Filters := SORTABLE_TEXT_FILTER;
    end;
    LForm.Show;
    PumpFor(50);
    LGrid.DataSource := LSource;
    Settle(LGrid);
    case AScenario of
      psSingleFromUnknownTotal: begin
        CheckAll(240);
        Require(LGrid.InternalDrawGrid.RowCount = 241, 'Single needs a native header and every matching row');
        {$IFDEF MSWINDOWS}
        Require((GetWindowLongPtr(LGrid.InternalDrawGrid.Handle, GWL_STYLE) and WS_VSCROLL) <> 0,
          'All-row mode must expose the actual native vertical scrollbar');
        {$ENDIF}
        LSource.Count := 370;
        LGrid.RefreshGrid;
        Settle(LGrid);
        CheckAll(370);
        Require(RowId(LGrid, 369) = 369, 'Single truncated a growing result to its previous total');
      end;
      psModeTransitions: begin
        Require(LGrid.PageMode = vpmManual, 'Existing default must remain Manual');
        LGrid.PageIndex := 3;
        Settle(LGrid);
        LWidth := LGrid.Columns[1].Width;
        LGrid.ChooseMode(0);
        Settle(LGrid);
        Require(LGrid.PageMode = vpmSingle, 'Mode menu did not select Single');
        CheckAll(240);
        LBefore := LSource.Calls;
        LGrid.PageMode := vpmSingle;
        PumpFor(200);
        Require(LSource.Calls = LBefore, 'Assigning the same mode fetched again');
        LGrid.ChooseMode(0);
        Settle(LGrid);
        Require(LSource.Calls = LBefore, 'Selecting the current native menu item fetched again');
        ChooseVisualGridPageMode(LGrid,1,True);
        Settle(LGrid);
        Require((LGrid.PageMode = vpmAuto) and (LGrid.PageIndex = 0), 'Auto transition must start at page zero');
        Require((LGrid.PageSize = LGrid.Capacity) and
          not LGrid.ManualEditor.Visible and LGrid.Navigator.Visible,
          'Auto must fit complete native rows and show only navigation');
        Require(LGrid.InternalDrawGrid.ScrollBars = {$IFDEF FPC}ssAutoHorizontal{$ELSE}ssHorizontal{$ENDIF},
          'Auto must allow horizontal scrolling without a vertical partial row');
        LGrid.Columns[1].Width := 1500;
        Settle(LGrid);
        {$IFDEF MSWINDOWS}
        Require((GetWindowLongPtr(LGrid.InternalDrawGrid.Handle, GWL_STYLE) and WS_HSCROLL) <> 0,
          'Wide Auto columns must expose the actual native horizontal scrollbar');
        Require((GetWindowLongPtr(LGrid.InternalDrawGrid.Handle, GWL_STYLE) and WS_VSCROLL) = 0,
          'Automatic paging must not expose a vertical scrollbar');
        {$ENDIF}
        Require(LGrid.PageSize = LGrid.Capacity,
          'Auto did not refit complete rows after the horizontal scrollbar appeared');
        LGrid.Columns[1].Width := LWidth;
        Settle(LGrid);
        {$IFDEF MSWINDOWS}
        Require((GetWindowLongPtr(LGrid.InternalDrawGrid.Handle, GWL_STYLE) and
          (WS_HSCROLL or WS_VSCROLL)) = 0,
          'Fitting Auto columns must hide the horizontal bar and keep vertical scrolling disabled');
        {$ENDIF}
        Require(LGrid.PageSize = LGrid.Capacity,
          'Auto did not refit complete rows after hiding the horizontal scrollbar');
        LGrid.PageIndex := 2;
        Settle(LGrid);
        LPage := LGrid.PageIndex;
        LForm.Height := LForm.Height + 67;
        Settle(LGrid);
        Require((LGrid.PageIndex = LPage) and (LGrid.PageSize = LGrid.Capacity),
          'Auto resize must retain the numerical page and refit complete rows');
        LGrid.ChooseMode(2);
        Settle(LGrid);
        Require((LGrid.PageMode = vpmManual) and (LGrid.PageIndex = 0) and
          (LGrid.PageSize = 25) and (LGrid.ManualPageSize = 25),
          'Returning to Manual lost its independent remembered size');
        Require(LGrid.ManualEditor.Visible and LGrid.Navigator.Visible,
          'Manual controls were not restored');
        Require(LGrid.Columns[1].Width = LWidth, 'Mode transitions changed an established column width');
        LGrid.AutoPageSize := True;
        Settle(LGrid);
        Require(LGrid.PageMode = vpmAuto, 'Legacy AutoPageSize did not map to Auto');
        LGrid.AutoPageSize := False;
        Settle(LGrid);
        Require((LGrid.PageMode = vpmManual) and (LGrid.ManualPageSize = 25),
          'Disabling legacy AutoPageSize did not restore Manual size');
        LGrid.ShowAllData := True;
        Settle(LGrid);
        CheckAll(240);
        LGrid.ShowAllData := False;
        Settle(LGrid);
        Require((LGrid.PageMode = vpmManual) and (LGrid.PageSize = 25),
          'Disabling legacy ShowAllData did not restore Manual size');
        CheckSelector;
        CheckFooterHostedFirstShow(ALog);
        CheckFooterAnchoringMatrix(ALog);
      end;
      psCompleteSearchAndSort: begin
        LGrid.PageMode := vpmSingle;
        Settle(LGrid);
        LGrid.Columns[0].SortDirection := sdDescending;
        Settle(LGrid);
        Require((LGrid.RowCount = 240) and (RowId(LGrid, 0) = 239) and
          (RowId(LGrid, 239) = 0), 'Single sorting did not include the entire source');
        LGrid.Search('235');
        Settle(LGrid);
        Require((LGrid.PageIndex = 0) and (LGrid.RowCount = 1) and
          (RowId(LGrid, 0) = 235), 'Single search did not filter the complete source');
        CheckSelector;
        LGrid.Search('');
        Settle(LGrid);
        CheckAll(240);
      end;
      psEmptyAndOnePage: begin
        for I := 0 to 1 do begin
          LSource.Count := I;
          LGrid.RefreshGrid;
          Settle(LGrid);
          LGrid.PageMode := vpmSingle;
          Settle(LGrid);
          CheckAll(I);
          for LSize := 0 to 2 do begin
            LGrid.ChooseMode(LSize);
            LForm.ClientWidth := 290;
            Settle(LGrid);
            CheckSelector;
            CheckFooterGeometry(LGrid, 96, 290, ALog);
          end;
          LForm.ClientWidth := 780;
        end;
      end;
      psSuspendedLatestMode: begin
        LGrid.RefreshSuspended := True;
        LBefore := LSource.Calls;
        LGrid.ChooseMode(0);
        LGrid.ChooseMode(1);
        LGrid.ChooseMode(2);
        LForm.ClientWidth := 300;
        PumpFor(50);
        CheckFooterGeometry(LGrid, 96, 300, ALog);
        LGrid.ManualPageSize := 17;
        LGrid.ChooseMode(0);
        LForm.Height := LForm.Height + 80;
        PumpFor(300);
        Require(LSource.Calls = LBefore, 'Changing mode/size/layout while suspended queried its source');
        LGrid.RefreshThrottle.Cancel;
        LGrid.RefreshSuspended := False;
        Settle(LGrid);
        Require(LSource.Calls = LBefore + 1, 'Resume did not fetch exactly one latest mode');
        CheckAll(240);
        LGrid.ShowLoading('Please Wait', 'Test hold', nil, True);
        Require(not EffectivelyEnabled(LGrid.ModeControl), 'Disabled loading must gate the mode button');
        LBefore := LSource.Calls;
        LGrid.ModeMenu.Items[2].Click;
        Require((LGrid.PageMode = vpmSingle) and (LSource.Calls = LBefore),
          'A loading-disabled menu command changed mode or queried data');
        LGrid.HideLoading;
        Require(EffectivelyEnabled(LGrid.ModeControl), 'Hiding the manual card did not restore the mode button');
      end;
      psDiscardOldWorker: begin
        LGrid.FetchDataInThread := True;
        LSource.Entered.ResetEvent;
        LSource.ReleaseRead.ResetEvent;
        LSource.HoldNext := True;
        LGrid.RefreshGrid;
        Require(LSource.Entered.WaitFor(1000) = wrSignaled, 'Old finite provider never entered its real worker');
        LObserver.ExpectedRows := 240;
        LObserver.WatchSingle := True;
        // The public API remains available while native fetch input is gated.
        LGrid.PageMode := vpmSingle;
        LSource.ReleaseRead.SetEvent;
        Settle(LGrid);
        CheckAll(240);
        Require((LObserver.SinglePublications > 0) and not LObserver.InvalidSinglePublication,
          'An obsolete finite page was published after switching to Single');
        Require(LGrid.ManualPageSize = 25, 'Late finite result overwrote manual size memory');
      end;
      psModeChangeScrollOrigin: CheckModeScrollTransitions(0);
      psSuspendedModeScrollOrigin: CheckModeScrollTransitions(1);
      psHeldModeScrollOrigin: CheckModeScrollTransitions(2);
      psDisabledPaging: begin
        LGrid.CanPage := False;
        for I := 0 to 2 do begin
          LGrid.PageMode := TVisualGridPageMode(I);
          LGrid.RefreshGrid;
          Settle(LGrid);
          Require((LSource.LastParams.PageSize <= 0) and (LGrid.RowCount = 240),
            'CanPage=False must query all rows independently of the retained mode');
          Require(not LGrid.Navigator.Visible and not LGrid.ManualEditor.Visible and
            not LGrid.ModeControl.Visible,
            'Paging-disabled grid must hide finite controls');
        end;
        LGrid.CanPage := True;
        LGrid.PageMode := vpmManual;
        Settle(LGrid);
        Require((LGrid.ManualPageSize = 25) and (LGrid.RowCount = 25),
          'Re-enabling paging lost the configured manual size');
      end;
    end;
    if ALog <> nil then ALog.Add('PASS native paging scenario=' + IntToStr(Ord(AScenario)));
  finally
    if Assigned(LSource) then LSource.ReleaseRead.SetEvent;
    LForm.Free;
  end;
end;

end.
