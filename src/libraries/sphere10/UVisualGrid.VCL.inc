{ Native VCL adapters used only by UVisualGrid's Delphi branch. Column metadata
  uses the existing VCL header-section collection; drawing and input remain on
  TDrawGrid. Child windows/components keep normal VCL ownership. }
{$IFDEF VISUALGRID_VCL_DECLARATIONS}
type
  TGridColumn = THeaderSection;
  TVisualGridHeaderClick = procedure(Sender:TObject; IsColumn:Boolean; Index:Integer) of object;
  TVisualGridCellSelected = procedure(Sender:TObject; ACol,ARow:Integer) of object;
  TGridZone = (gzInvalid, gzFixedCells, gzNormal);

  TDrawGrid = class(Vcl.Grids.TDrawGrid)
  private
    FHeader: THeaderControl;
    FAutoFillColumns,FAdjusting,FKeepRanges,FHasRanges: Boolean;
    FDataRows:Integer;
    FRanges: TArray<TRect>;
    FHeaderClick: TVisualGridHeaderClick;
    FSelectionChanged: TVisualGridCellSelected;
    function GetColumns:THeaderSections;
    function GetSelectedRangeCount:Integer;
    function GetSelectedRange(AIndex:Integer):TRect;
    function GetGridHeight:Integer;
    procedure SetAutoFillColumns(AValue:Boolean);
    function GetGridOptions:TGridOptions;
    procedure SetGridOptions(AValue:TGridOptions);
  protected
    procedure Resize; override;
    procedure ColWidthsChanged; override;
    procedure FixedCellClick(ACol,ARow:Longint); override;
    procedure SelectionMoved(const OldSel:TGridRect); override;
    procedure MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure DrawCell(ACol,ARow:Longint; ARect:TRect; AState:TGridDrawState); override;
    function SelectCell(ACol,ARow:Longint):Boolean; override;
  public
    constructor Create(AOwner:TComponent); override;
    procedure FitColumns;
    procedure InvalidateCell(ACol,ARow:Longint; ARedraw:Boolean); reintroduce;
    procedure InvalidateRow(ARow:Longint); reintroduce;
    procedure MoveExtend(AExtend:Boolean; ACol,ARow:Integer);
    function IsFixedCellVisible(ACol,ARow:Integer):Boolean;
    function MouseToGridZone(X,Y:Integer):TGridZone;
    property Columns:THeaderSections read GetColumns;
    property AutoFillColumns:Boolean read FAutoFillColumns write SetAutoFillColumns;
    property SelectedRangeCount:Integer read GetSelectedRangeCount;
    property SelectedRange[AIndex:Integer]:TRect read GetSelectedRange;
    property GridHeight:Integer read GetGridHeight;
    property DataRowCount:Integer read FDataRows write FDataRows;
    property OnHeaderClick:TVisualGridHeaderClick read FHeaderClick write FHeaderClick;
    property OnSelection:TVisualGridCellSelected read FSelectionChanged write FSelectionChanged;
    property Options:TGridOptions read GetGridOptions write SetGridOptions;
  end;

  TCheckComboItemChange = procedure(Sender:TObject; AIndex:Integer) of object;
  TCheckComboBox = class(TComboBox)
  private
    FPopup:TForm;
    FChecklist:TCheckListBox;
    FChecks:TArray<Boolean>;
    FOpening:Boolean;
    FItemChange:TCheckComboItemChange;
    FStoredItems:TStringList;
    function ReadStoredItems:TStrings;
    procedure SyncItems(Sender:TObject);
    function GetChecked(AIndex:Integer):Boolean;
    procedure SetChecked(AIndex:Integer; AValue:Boolean);
    function GetObject(AIndex:Integer):TObject;
    procedure SetObject(AIndex:Integer; AValue:TObject);
    procedure CheckClicked(Sender:TObject);
    procedure PopupDeactivated(Sender:TObject);
    procedure PopupKeyDown(Sender:TObject; var Key:Word; Shift:TShiftState);
  protected
    procedure DropDown; override;
    procedure WndProc(var Message:TMessage); override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure AddItem(const AText:string; AState:TCheckBoxState); reintroduce;
    procedure Clear; override;
    property Checked[AIndex:Integer]:Boolean read GetChecked write SetChecked;
    property Objects[AIndex:Integer]:TObject read GetObject write SetObject;
    property OnItemChange:TCheckComboItemChange read FItemChange write FItemChange;
    property Items:TStrings read ReadStoredItems;
  end;
{$ELSE}
constructor TDrawGrid.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FHeader:=THeaderControl.Create(Self);
  FHeader.Visible:=False;
  Options:=(Options-[goDrawFocusSelected])+[goFixedRowClick];
end;

function TDrawGrid.GetColumns:THeaderSections;
begin
  Result:=FHeader.Sections;
end;

procedure TDrawGrid.SetAutoFillColumns(AValue:Boolean);
begin
  FAutoFillColumns:=AValue;
  FitColumns;
end;

function TDrawGrid.GetGridOptions:TGridOptions;
begin
  Result:=Vcl.Grids.TDrawGrid(Self).Options;
end;

procedure TDrawGrid.SetGridOptions(AValue:TGridOptions);
begin
  Vcl.Grids.TDrawGrid(Self).Options:=AValue;
  if not (goRowSelect in AValue) and not (goDrawFocusSelected in AValue) then begin
    FRanges:=nil;
    FHasRanges:=True;
  end;
end;

procedure TDrawGrid.FitColumns;
var i,LUsed,LStretch,LExtra,LWidth:Integer;
begin
  // Buffer column metadata until VCL creates the grid window.
  if FAdjusting or not Assigned(FHeader) or not HandleAllocated then Exit;
  if Columns.Count=0 then Exit;
  FAdjusting:=True;
  try
    ColCount:=Columns.Count;
    LUsed:=0; LStretch:=0;
    for i:=0 to Columns.Count-1 do begin
      LWidth:=Columns[i].Width;
      if LWidth<=0 then LWidth:=DefaultColWidth;
      ColWidths[i]:=LWidth;
      Inc(LUsed,LWidth+GridLineWidth);
      if Columns[i].AutoSize then Inc(LStretch);
    end;
    if FAutoFillColumns then begin
      LExtra:=Max(0,ClientWidth-LUsed);
      if LStretch=0 then ColWidths[ColCount-1]:=ColWidths[ColCount-1]+LExtra
      else for i:=0 to Columns.Count-1 do
        if Columns[i].AutoSize then ColWidths[i]:=ColWidths[i]+LExtra div LStretch;
    end;
  finally FAdjusting:=False; end;
end;

procedure TDrawGrid.Resize;
begin
  inherited;
  FitColumns;
end;

procedure TDrawGrid.ColWidthsChanged;
var i:Integer;
begin
  inherited;
  if FAdjusting or not Assigned(FHeader) then Exit;
  for i:=0 to Min(ColCount,Columns.Count)-1 do Columns[i].Width:=ColWidths[i];
end;

procedure TDrawGrid.FixedCellClick(ACol,ARow:Longint);
begin
  inherited;
  if (ARow=0) and Assigned(FHeaderClick) then FHeaderClick(Self,True,ACol);
end;

procedure TDrawGrid.SelectionMoved(const OldSel:TGridRect);
begin
  inherited;
  if not FKeepRanges then begin
    FHasRanges:=True;
    SetLength(FRanges,1);
    FRanges[0]:=Rect(Selection.Left,Selection.Top,Selection.Right,Selection.Bottom);
    if Assigned(FSelectionChanged) then FSelectionChanged(Self,Col,Row);
  end;
end;

procedure TDrawGrid.MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:Integer);
var LCol,LRow,i,j:Integer; LToggle:Boolean;
begin
  LToggle:=(Button=mbLeft) and (ssCtrl in Shift) and
    (goRowSelect in Options) and (goRangeSelect in Options) and
    (MouseToGridZone(X,Y)=gzNormal);
  FKeepRanges:=LToggle;
  try inherited; finally FKeepRanges:=False; end;
  if not LToggle then begin
    if (Button=mbLeft) and (MouseToGridZone(X,Y)=gzNormal) and
      ((goRowSelect in Options) or (goDrawFocusSelected in Options)) then begin
      FHasRanges:=True;
      SetLength(FRanges,1);
      FRanges[0]:=Rect(Selection.Left,Selection.Top,Selection.Right,Selection.Bottom);
      if Assigned(FSelectionChanged) then FSelectionChanged(Self,Col,Row);
    end;
    Exit;
  end;
  FHasRanges:=True;
  MouseToCell(X,Y,LCol,LRow);
  for i:=0 to High(FRanges) do
    if (LRow>=FRanges[i].Top) and (LRow<=FRanges[i].Bottom) then begin
      // Toggle one row while retaining the rest of a selected range.
      if FRanges[i].Top< LRow then begin
        SetLength(FRanges,Length(FRanges)+1);
        FRanges[High(FRanges)]:=Rect(0,FRanges[i].Top,ColCount-1,LRow-1);
      end;
      if FRanges[i].Bottom>LRow then begin
        SetLength(FRanges,Length(FRanges)+1);
        FRanges[High(FRanges)]:=Rect(0,LRow+1,ColCount-1,FRanges[i].Bottom);
      end;
      for j:=i to High(FRanges)-1 do FRanges[j]:=FRanges[j+1];
      SetLength(FRanges,Length(FRanges)-1);
      Invalidate;
      if Assigned(FSelectionChanged) then FSelectionChanged(Self,LCol,LRow);
      Exit;
    end;
  SetLength(FRanges,Length(FRanges)+1);
  FRanges[High(FRanges)]:=Rect(0,LRow,ColCount-1,LRow);
  Invalidate;
  if Assigned(FSelectionChanged) then FSelectionChanged(Self,LCol,LRow);
end;

procedure TDrawGrid.DrawCell(ACol,ARow:Longint; ARect:TRect; AState:TGridDrawState);
var i:Integer;
begin
  if FHasRanges and (Length(FRanges)=0) and (ARow>=FixedRows) then begin
    Exclude(AState,gdSelected);
    Canvas.Brush.Color:=Color;
    Canvas.Font.Color:=Font.Color;
    Canvas.FillRect(ARect);
  end;
  if (goRowSelect in Options) and FHasRanges and (ARow>=FixedRows) then begin
    Exclude(AState,gdSelected);
    Canvas.Brush.Color:=Color;
    Canvas.Font.Color:=Font.Color;
    Canvas.FillRect(ARect);
    for i:=0 to High(FRanges) do
      if (ARow>=FRanges[i].Top) and (ARow<=FRanges[i].Bottom) then begin
        Include(AState,gdSelected);
        Canvas.Brush.Color:=clHighlight;
        Canvas.Font.Color:=clHighlightText;
        Canvas.FillRect(ARect);
        Break;
      end;
  end;
  inherited DrawCell(ACol,ARow,ARect,AState);
end;

function TDrawGrid.SelectCell(ACol,ARow:Longint):Boolean;
begin
  Result:=Assigned(FHeader) and (Columns.Count>0) and (ARow>0) and (ARow<=FDataRows);
  if Result then Result:=inherited SelectCell(ACol,ARow);
end;

function TDrawGrid.GetSelectedRangeCount:Integer;
begin
  if FHasRanges then Result:=Length(FRanges) else Result:=1;
end;

function TDrawGrid.GetSelectedRange(AIndex:Integer):TRect;
begin
  if Length(FRanges)>0 then Result:=FRanges[AIndex]
  else Result:=Rect(Selection.Left,Selection.Top,Selection.Right,Selection.Bottom);
end;

function TDrawGrid.GetGridHeight:Integer;
var i:Integer;
begin
  Result:=0;
  for i:=0 to RowCount-1 do Inc(Result,RowHeights[i]+GridLineWidth);
end;

procedure TDrawGrid.InvalidateCell(ACol,ARow:Longint; ARedraw:Boolean);
begin
  if HandleAllocated then inherited InvalidateCell(ACol,ARow);
end;

procedure TDrawGrid.InvalidateRow(ARow:Longint);
begin
  if HandleAllocated then inherited InvalidateRow(ARow);
end;

procedure TDrawGrid.MoveExtend(AExtend:Boolean; ACol,ARow:Integer);
begin
  MoveColRow(ACol,ARow,not AExtend,True);
end;

function TDrawGrid.IsFixedCellVisible(ACol,ARow:Integer):Boolean;
var R:TRect;
begin
  R:=CellRect(ACol,ARow);
  Result:=(R.Right>0) and (R.Left<ClientWidth) and (R.Bottom>0) and (R.Top<ClientHeight);
end;

function TDrawGrid.MouseToGridZone(X,Y:Integer):TGridZone;
var C,R:Integer;
begin
  Result:=gzInvalid;
  MouseToCell(X,Y,C,R);
  if (C<0) or (R<0) or (C>=ColCount) or (R>=RowCount) or (R>FDataRows) then Exit;
  if (C<FixedCols) or (R<FixedRows) then Result:=gzFixedCells else Result:=gzNormal;
end;

constructor TCheckComboBox.Create(AOwner:TComponent);
begin
  inherited;
  Style:=csDropDownList;
  FStoredItems:=TStringList.Create;
  FStoredItems.OnChange:=SyncItems;
end;

destructor TCheckComboBox.Destroy;
begin
  FStoredItems.Free;
  inherited;
end;

function TCheckComboBox.ReadStoredItems:TStrings;
begin
  Result:=FStoredItems;
end;

procedure TCheckComboBox.SyncItems(Sender:TObject);
begin
  // Native TComboBox.Items.Add requests an HWND. Buffer the control's model
  // while a composite grid is still being constructed without a parent.
  if not HandleAllocated or not Assigned(FStoredItems) then Exit;
  Vcl.StdCtrls.TComboBox(Self).Items.Assign(FStoredItems);
  if FStoredItems.Count>0 then ItemIndex:=0;
end;

procedure TCheckComboBox.CreateWnd;
begin
  inherited;
  SyncItems(Self);
end;

procedure TCheckComboBox.AddItem(const AText:string; AState:TCheckBoxState);
begin
  Items.Add(AText);
  SetLength(FChecks,Items.Count);
  FChecks[Items.Count-1]:=AState=cbChecked;
end;

procedure TCheckComboBox.Clear;
begin
  if Assigned(FPopup) then FPopup.Hide;
  if Assigned(FChecklist) then FChecklist.Clear;
  FStoredItems.Clear;
  FChecks:=nil;
end;

function TCheckComboBox.GetChecked(AIndex:Integer):Boolean;
begin
  Result:=(AIndex>=0) and (AIndex<Length(FChecks)) and FChecks[AIndex];
end;

procedure TCheckComboBox.SetChecked(AIndex:Integer; AValue:Boolean);
begin
  if GetChecked(AIndex)=AValue then Exit;
  if Length(FChecks)<Items.Count then SetLength(FChecks,Items.Count);
  FChecks[AIndex]:=AValue;
  if Assigned(FChecklist) and (AIndex<FChecklist.Items.Count) then FChecklist.Checked[AIndex]:=AValue;
  if Assigned(FItemChange) then FItemChange(Self,AIndex);
end;

function TCheckComboBox.GetObject(AIndex:Integer):TObject;
begin
  Result:=Items.Objects[AIndex];
end;

procedure TCheckComboBox.SetObject(AIndex:Integer; AValue:TObject);
begin
  Items.Objects[AIndex]:=AValue;
end;

procedure TCheckComboBox.PopupDeactivated(Sender:TObject);
begin
  FPopup.Hide;
end;

procedure TCheckComboBox.PopupKeyDown(Sender:TObject; var Key:Word; Shift:TShiftState);
begin
  if Key in [VK_ESCAPE,VK_RETURN] then begin FPopup.Hide; Key:=0; end;
end;

procedure TCheckComboBox.WndProc(var Message:TMessage);
begin
  // Retain native combo chrome while directing opening input to the checked
  // list, rather than creating a second single-selection Windows drop list.
  if ((Message.Msg=CB_SHOWDROPDOWN) and (Message.WParam<>0)) or
    (Message.Msg=WM_LBUTTONDOWN) or
    ((Message.Msg=WM_KEYDOWN) and (Message.WParam=VK_F4)) or
    ((Message.Msg=WM_SYSKEYDOWN) and (Message.WParam=VK_DOWN)) then begin
    if Enabled then DropDown;
    Message.Result:=0;
    Exit;
  end;
  inherited;
  if (Message.Msg=CM_ENABLEDCHANGED) and not Enabled and Assigned(FPopup) then FPopup.Hide;
end;

procedure TCheckComboBox.CheckClicked(Sender:TObject);
begin
  if FChecklist.ItemIndex>=0 then
    SetChecked(FChecklist.ItemIndex,FChecklist.Checked[FChecklist.ItemIndex]);
end;

procedure TCheckComboBox.DropDown;
var P:TPoint; i:Integer;
begin
  if FOpening then Exit;
  FOpening:=True;
  try
    DroppedDown:=False;
    if not Assigned(FPopup) then begin
      FPopup:=TForm.CreateNew(Self);
      FPopup.BorderStyle:=bsNone;
      FPopup.Position:=poDesigned;
      FPopup.PopupMode:=pmExplicit;
      FPopup.PopupParent:=GetParentForm(Self);
      FPopup.OnDeactivate:=PopupDeactivated;
      FChecklist:=TCheckListBox.Create(FPopup);
      FChecklist.Parent:=FPopup;
      FChecklist.Align:=alClient;
      FChecklist.OnClickCheck:=CheckClicked;
      FChecklist.OnKeyDown:=PopupKeyDown;
    end;
    FChecklist.Items.Assign(Items);
    for i:=0 to Items.Count-1 do FChecklist.Checked[i]:=GetChecked(i);
    P:=ClientToScreen(Point(0,Height));
    FPopup.SetBounds(P.X,P.Y,Max(Width,160),Min(240,Max(32,Items.Count*(Font.Height*-1+8)+4)));
    FPopup.Show;
    FChecklist.SetFocus;
  finally FOpening:=False; end;
end;
{$ENDIF}
