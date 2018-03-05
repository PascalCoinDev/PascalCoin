unit UGridUtils;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Classes, Grids, UNode, UAccounts, UBlockChain, UAppParams,
  UWallet, UCrypto, UPoolMining, URPC;

Type
  // TAccountsGrid implements a visual integration of TDrawGrid
  // to show accounts information
  TAccountColumnType = (act_account_number,act_account_key,act_balance,act_updated,act_n_operation,act_updated_state,act_name,act_type,act_saleprice);
  TAccountColumn = Record
    ColumnType : TAccountColumnType;
    width : Integer;
  End;

  TAccountsGrid = Class(TComponent)
  private
    FAccountsBalance : Int64;
    FAccountsList : TOrderedCardinalList;
    FColumns : Array of TAccountColumn;
    FDrawGrid : TDrawGrid;
    FNodeNotifyEvents : TNodeNotifyEvents;
    FShowAllAccounts: Boolean;
    FOnUpdated: TNotifyEvent;
    FAccountsCount: Integer;
    FAllowMultiSelect: Boolean;
    procedure SetDrawGrid(const Value: TDrawGrid);
    Procedure InitGrid;
    Procedure OnNodeNewOperation(Sender : TObject);
    procedure OnGridDrawCell(Sender: TObject; ACol, ARow: Longint; Rect: TRect; State: TGridDrawState);
    procedure SetNode(const Value: TNode);
    function GetNode: TNode;
    procedure SetShowAllAccounts(const Value: Boolean);
    procedure SetAllowMultiSelect(const Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); Override;
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Property DrawGrid : TDrawGrid read FDrawGrid write SetDrawGrid;
    Function LockAccountsList : TOrderedCardinalList;
    Procedure UnlockAccountsList;
    Property Node : TNode read GetNode write SetNode;
    Function AccountNumber(GridRow : Integer) : Int64;
    Procedure SaveToStream(Stream : TStream);
    Procedure LoadFromStream(Stream : TStream);
    Property ShowAllAccounts : Boolean read FShowAllAccounts write SetShowAllAccounts;
    Property AccountsBalance : Int64 read FAccountsBalance;
    Property AccountsCount : Integer read FAccountsCount;
    Function MoveRowToAccount(nAccount : Cardinal) : Boolean;
    Property OnUpdated : TNotifyEvent read FOnUpdated write FOnUpdated;
    Property AllowMultiSelect : Boolean read FAllowMultiSelect write SetAllowMultiSelect;
    Function SelectedAccounts(accounts : TOrderedCardinalList) : Integer;
  End;

  TOperationsGrid = Class(TComponent)
  private
    FDrawGrid: TDrawGrid;
    FAccountNumber: Int64;
    FOperationsResume : TOperationsResumeList;
    FNodeNotifyEvents : TNodeNotifyEvents;
    FPendingOperations: Boolean;
    FBlockStart: Int64;
    FBlockEnd: Int64;
    FMustShowAlwaysAnAccount: Boolean;
    Procedure OnNodeNewOperation(Sender : TObject);
    Procedure OnNodeNewAccount(Sender : TObject);
    Procedure InitGrid;
    procedure OnGridDrawCell(Sender: TObject; ACol, ARow: Longint; Rect: TRect; State: TGridDrawState);
    procedure SetDrawGrid(const Value: TDrawGrid);
    procedure SetAccountNumber(const Value: Int64);
    procedure SetNode(const Value: TNode);
    function GetNode: TNode;
    procedure SetPendingOperations(const Value: Boolean);

    procedure SetBlockEnd(const Value: Int64);
    procedure SetBlockStart(const Value: Int64);
    procedure SetMustShowAlwaysAnAccount(const Value: Boolean);
    function GetSelectedOperation : TOperationResume;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); Override;
  public
    property SelectedOperation : TOperationResume read GetSelectedOperation;
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Property DrawGrid : TDrawGrid read FDrawGrid write SetDrawGrid;
    Property PendingOperations : Boolean read FPendingOperations write SetPendingOperations;
    Property AccountNumber : Int64 read FAccountNumber write SetAccountNumber;
    Property MustShowAlwaysAnAccount : Boolean read FMustShowAlwaysAnAccount write SetMustShowAlwaysAnAccount;
    Property Node : TNode read GetNode write SetNode;
    Procedure UpdateAccountOperations; virtual;
    Procedure ShowModalDecoder(WalletKeys: TWalletKeys; AppParams : TAppParams);
    Property BlockStart : Int64 read FBlockStart write SetBlockStart;
    Property BlockEnd : Int64 read FBlockEnd write SetBlockEnd;
    Procedure SetBlocks(bstart,bend : Int64);
    Property OperationsResume : TOperationsResumeList read FOperationsResume;
  End;

  TBlockChainData = Record
    Block : Cardinal;
    Timestamp : Cardinal;
    BlockProtocolVersion,
    BlockProtocolAvailable : Word;
    OperationsCount : Integer;
    Volume : Int64;
    Reward, Fee : Int64;
    Target : Cardinal;
    HashRateKhs : Int64;
    MinerPayload : TRawBytes;
    PoW : TRawBytes;
    SafeBoxHash : TRawBytes;
    AccumulatedWork : UInt64;
    TimeAverage200 : Real;
    TimeAverage150 : Real;
    TimeAverage100 : Real;
    TimeAverage75 : Real;
    TimeAverage50 : Real;
    TimeAverage25 : Real;
    TimeAverage10 : Real;
  End;
  TBlockChainDataArray = Array of TBlockChainData;

  { TBlockChainGrid }

  TBlockChainGrid = Class(TComponent)
  private
    FBlockChainDataArray : TBlockChainDataArray;
    FBlockStart: Int64;
    FMaxBlocks: Integer;
    FBlockEnd: Int64;
    FDrawGrid: TDrawGrid;
    FNodeNotifyEvents : TNodeNotifyEvents;
    FHashRateAverageBlocksCount: Integer;
    FShowTimeAverageColumns: Boolean;
    Procedure OnNodeNewAccount(Sender : TObject);
    Procedure InitGrid;
    procedure OnGridDrawCell(Sender: TObject; ACol, ARow: Longint; Rect: TRect; State: TGridDrawState);
    function GetNode: TNode;
    procedure SetBlockEnd(const Value: Int64);
    procedure SetBlockStart(const Value: Int64);
    procedure SetDrawGrid(const Value: TDrawGrid);
    procedure SetMaxBlocks(const Value: Integer);
    procedure SetNode(const Value: TNode);
    procedure SetHashRateAverageBlocksCount(const Value: Integer);
    procedure SetShowTimeAverageColumns(AValue: Boolean);
 public
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); Override;
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Property DrawGrid : TDrawGrid read FDrawGrid write SetDrawGrid;
    Property Node : TNode read GetNode write SetNode;
    Procedure UpdateBlockChainGrid; virtual;
    Property BlockStart : Int64 read FBlockStart write SetBlockStart;
    Property BlockEnd : Int64 read FBlockEnd write SetBlockEnd;
    Procedure SetBlocks(bstart,bend : Int64);
    Property MaxBlocks : Integer read FMaxBlocks write SetMaxBlocks;
    Property HashRateAverageBlocksCount : Integer read FHashRateAverageBlocksCount write SetHashRateAverageBlocksCount;
    Property ShowTimeAverageColumns : Boolean read FShowTimeAverageColumns write SetShowTimeAverageColumns;
  End;

Const
  CT_TBlockChainData_NUL : TBlockChainData = (Block:0;Timestamp:0;BlockProtocolVersion:0;BlockProtocolAvailable:0;OperationsCount:-1;Volume:-1;Reward:0;Fee:0;Target:0;HashRateKhs:0;MinerPayload:'';PoW:'';SafeBoxHash:'';AccumulatedWork:0;TimeAverage200:0;TimeAverage150:0;TimeAverage100:0;TimeAverage75:0;TimeAverage50:0;TimeAverage25:0;TimeAverage10:0);


implementation

uses
  Graphics, SysUtils, UTime, UOpTransaction, UConst,
  UFRMPayloadDecoder, ULog;

{ TAccountsGrid }

Const CT_ColumnHeader : Array[TAccountColumnType] Of String =
  ('Account N.','Key','Balance','Updated','N Op.','S','Name','Type','Price');

function TAccountsGrid.AccountNumber(GridRow: Integer): Int64;
begin
  if GridRow<1 then Result := -1
  else if FShowAllAccounts then begin
    if Assigned(Node) then begin
      Result := GridRow-1;
    end else Result := -1;
  end else if GridRow<=FAccountsList.Count then begin
    Result := (FAccountsList.Get(GridRow-1));
  end else Result := -1;
end;

constructor TAccountsGrid.Create(AOwner: TComponent);
Var i : Integer;
begin
  inherited;
  FAllowMultiSelect := false;
  FOnUpdated := Nil;
  FAccountsBalance := 0;
  FAccountsCount := 0;
  FShowAllAccounts := false;
  FAccountsList := TOrderedCardinalList.Create;
  FDrawGrid := Nil;
  SetLength(FColumns,7);
  FColumns[0].ColumnType := act_account_number;
  FColumns[0].width := 65;
  FColumns[1].ColumnType := act_name;
  FColumns[1].width := 80;
  FColumns[2].ColumnType := act_balance;
  FColumns[2].width := 80;
  FColumns[3].ColumnType := act_n_operation;
  FColumns[3].width := 40;
  FColumns[4].ColumnType := act_type;
  FColumns[4].width := 40;
  FColumns[5].ColumnType := act_saleprice;
  FColumns[5].width := 45;
  FColumns[6].ColumnType := act_updated_state;
  FColumns[6].width := 25;
  FNodeNotifyEvents := TNodeNotifyEvents.Create(Self);
  FNodeNotifyEvents.OnOperationsChanged := OnNodeNewOperation;
end;

destructor TAccountsGrid.Destroy;
begin
  FNodeNotifyEvents.Free;
  FAccountsList.Free;
  inherited;
end;

function TAccountsGrid.GetNode: TNode;
begin
  Result := FNodeNotifyEvents.Node;
end;

procedure TAccountsGrid.InitGrid;
Var i : Integer;
  acc : TAccount;
begin
  FAccountsBalance := 0;
  FAccountsCount := FAccountsList.Count;
  if Not assigned(DrawGrid) then exit;
  if FShowAllAccounts then begin
    if Assigned(Node) then begin
      if Node.Bank.AccountsCount<1 then DrawGrid.RowCount := 2
      else DrawGrid.RowCount := Node.Bank.AccountsCount+1;
      FAccountsBalance := Node.Bank.SafeBox.TotalBalance;
    end else DrawGrid.RowCount := 2;
  end else begin
    if FAccountsList.Count<1 then DrawGrid.RowCount := 2
    else DrawGrid.RowCount := FAccountsList.Count+1;
    if Assigned(Node) then begin
      for i := 0 to FAccountsList.Count - 1 do begin
        acc := Node.Bank.SafeBox.Account( FAccountsList.Get(i) );
        inc(FAccountsBalance, acc.balance);
      end;
    end;
  end;
  DrawGrid.FixedRows := 1;
  if Length(FColumns)=0 then DrawGrid.ColCount := 1
  else DrawGrid.ColCount := Length(FColumns);
  DrawGrid.FixedCols := 0;
  for i := low(FColumns) to high(FColumns) do begin
    DrawGrid.ColWidths[i] := FColumns[i].width;
  end;
  FDrawGrid.DefaultRowHeight := 18;
  DrawGrid.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
    {goRangeSelect, }goDrawFocusSelected, {goRowSizing, }goColSizing, {goRowMoving,}
    {goColMoving, goEditing, }goTabs, goRowSelect, {goAlwaysShowEditor,}
    goThumbTracking{$IFnDEF FPC}, goFixedColClick, goFixedRowClick, goFixedHotTrack{$ENDIF}];
  if FAllowMultiSelect then DrawGrid.Options := DrawGrid.Options + [goRangeSelect];
  FDrawGrid.Invalidate;
  if Assigned(FOnUpdated) then FOnUpdated(Self);
end;

procedure TAccountsGrid.LoadFromStream(Stream: TStream);
Var c,i,j : Integer;
begin
  if Stream.Read(c,sizeof(c))<sizeof(c) then exit;
  if c<=0 then exit;
  SetLength(FColumns,c);
  for i := 0 to c - 1 do begin
    Stream.Read(j,sizeof(j));
    if (j>=Integer(Low(TAccountColumnType))) And (j<=Integer(High(TAccountColumnType))) then begin
      FColumns[i].ColumnType := TAccountColumnType(j);
    end else FColumns[i].ColumnType := act_account_number;
    Stream.Read(FColumns[i].width,sizeof(FColumns[i].width));
  end;
  Stream.Read(j,sizeof(j));
  If Assigned(FDrawGrid) then FDrawGrid.Width := j;
  Stream.Read(j,sizeof(j));
  If Assigned(FDrawGrid) then FDrawGrid.Height := j;
end;

function TAccountsGrid.LockAccountsList: TOrderedCardinalList;
begin
  Result := FAccountsList;
end;

function TAccountsGrid.MoveRowToAccount(nAccount: Cardinal): Boolean;
Var oal : TOrderedCardinalList;
  idx : Integer;
begin
  Result := false;
  if Not Assigned(FDrawGrid) then exit;
  if Not Assigned(Node) then exit;
  if FDrawGrid.RowCount<=1 then exit;
  if FShowAllAccounts then begin
    If (FDrawGrid.RowCount>nAccount+1) And (nAccount>=0) And (nAccount<Node.Bank.AccountsCount) then begin
      FDrawGrid.Row := nAccount+1;
      Result := true;
    end else begin
      FDrawGrid.Row := FDrawGrid.RowCount-1;
    end;
  end else begin
    oal := LockAccountsList;
    try
      If oal.Find(nAccount,idx) then begin
        If FDrawGrid.RowCount>idx+1 then begin
          FDrawGrid.Row := idx+1;
          Result := true;
        end else begin
          FDrawGrid.Row := FDrawGrid.RowCount-1;
        end;
      end else begin
        If FDrawGrid.RowCount>idx+1 then begin
          FDrawGrid.Row := idx+1;
        end else begin
          FDrawGrid.Row := FDrawGrid.RowCount-1;
        end;
      end;
    finally
      UnlockAccountsList;
    end;
  end;
end;

procedure TAccountsGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation=opRemove then begin
    if (AComponent=FDrawGrid) then begin
      SetDrawGrid(Nil);
    end;
  end;
end;

{$IFDEF FPC}
Type
TTextFormats = (tfBottom, tfCalcRect, tfCenter, tfEditControl, tfEndEllipsis,
  tfPathEllipsis, tfExpandTabs, tfExternalLeading, tfLeft, tfModifyString,
  tfNoClip, tfNoPrefix, tfRight, tfRtlReading, tfSingleLine, tfTop,
  tfVerticalCenter, tfWordBreak);
TTextFormat = set of TTextFormats;

Procedure Canvas_TextRect(Canvas : TCanvas; var Rect: TRect; var Text: string; State: TGridDrawState; TextFormat: TTextFormat = []);
Var ts : TTextStyle;
Begin
  if (tfRight in TextFormat) then ts.Alignment:=taRightJustify
  else if (tfCenter in TextFormat) then ts.Alignment:=taCenter
  else ts.Alignment:=taLeftJustify;
  if (tfWordBreak in TextFormat) then ts.Wordbreak:=true
  else ts.Wordbreak:=false;
  if (tfVerticalCenter in TextFormat) then ts.Layout:=tlCenter
  else if (tfBottom in TextFormat) then ts.Layout:=tlBottom
  else ts.Layout:=tlTop;
  ts.Clipping:=Not (tfNoClip in TextFormat);
  ts.SingleLine := (tfSingleLine in TextFormat);
  ts.Wordbreak:= (tfWordBreak in TextFormat);
  ts.EndEllipsis:= (tfEndEllipsis in TextFormat);
  ts.ExpandTabs:=false;
  ts.Opaque:=false;
  ts.ShowPrefix:= not (tfNoPrefix in TextFormat);
  ts.SystemFont:=false;
  Canvas.TextRect(Rect,Rect.Left,Rect.Top,Text,ts);
end;
{$ELSE}
Procedure Canvas_TextRect(Canvas : TCanvas; var Rect: TRect; var Text: string; State: TGridDrawState; TextFormat: TTextFormat = []);
Begin
  Canvas.TextRect(Rect,Text,TextFormat);
end;
{$ENDIF}

procedure TAccountsGrid.OnGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
  Function FromColorToColor(colorstart,colordest : Integer; step,totalsteps : Integer) : Integer;
  var sr,sg,sb,dr,dg,db : Byte;
    i : Integer;
  begin
    i := colorstart;
    sr := GetRValue(i);
    sg := GetGValue(i);
    sb := GetBValue(i);
    i := colordest;
    dr := GetRValue(i);
    dg := GetGValue(i);
    db := GetBValue(i);
    sr := sr + (((dr-sr) DIV totalsteps)*step);
    sg := sg + (((dg-sg) DIV totalsteps)*step);
    sb := sb + (((db-sb) DIV totalsteps)*step);
    Result :=RGB(sr,sg,sb);
  end;
Var C : TAccountColumn;
  s : String;
  n_acc : Int64;
  account : TAccount;
  ndiff : Cardinal;
begin
  if Not Assigned(Node) then exit;

  if (ACol>=0) AND (ACol<length(FColumns)) then begin
    C := FColumns[ACol];
  end else begin
    C.ColumnType := act_account_number;
    C.width := -1;
  end;
  {.$IFDEF FPC}
  DrawGrid.Canvas.Font.Color:=clBlack;
  {.$ENDIF}
  if (ARow=0) then begin
    // Header
    s := CT_ColumnHeader[C.ColumnType];
    Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfCenter,tfVerticalCenter]);
  end else begin
    n_acc := AccountNumber(ARow);
    if (n_acc>=0) then begin
      if (n_acc>=Node.Bank.AccountsCount) then account := CT_Account_NUL
      else account := Node.Operations.SafeBoxTransaction.Account(n_acc);
      ndiff := Node.Bank.BlocksCount - account.updated_block;
      if (gdSelected in State) then
        If (gdFocused in State) then DrawGrid.Canvas.Brush.Color := clGradientActiveCaption
        else DrawGrid.Canvas.Brush.Color := clGradientInactiveCaption
      else DrawGrid.Canvas.Brush.Color := clWindow;
      DrawGrid.Canvas.FillRect(Rect);
      InflateRect(Rect,-2,-1);
      case C.ColumnType of
        act_account_number : Begin
          s := TAccountComp.AccountNumberToAccountTxtNumber(n_acc);
          Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfRight,tfVerticalCenter,tfSingleLine]);
        End;
        act_account_key : Begin
          s := Tcrypto.ToHexaString(TAccountComp.AccountKey2RawString(account.accountInfo.accountKey));
          Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfLeft,tfVerticalCenter,tfSingleLine]);
        End;
        act_balance : Begin
          if ndiff=0 then begin
            // Pending operation... showing final balance
            DrawGrid.Canvas.Font.Color := clBlue;
            s := '('+TAccountComp.FormatMoney(account.balance)+')';
          end else begin
            s := TAccountComp.FormatMoney(account.balance);
            if account.balance>0 then DrawGrid.Canvas.Font.Color := ClGreen
            else if account.balance=0 then DrawGrid.Canvas.Font.Color := clGrayText
            else DrawGrid.Canvas.Font.Color := clRed;
          end;
          Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfRight,tfVerticalCenter,tfSingleLine]);
        End;
        act_updated : Begin
          s := Inttostr(account.updated_block);
          Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfRight,tfVerticalCenter,tfSingleLine]);
        End;
        act_n_operation : Begin
          s := InttoStr(account.n_operation);
          Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfRight,tfVerticalCenter,tfSingleLine]);
        End;
        act_updated_state : Begin
          if TAccountComp.IsAccountBlockedByProtocol(account.account,Node.Bank.BlocksCount) then begin
            DrawGrid.Canvas.Brush.Color := clRed;
          end else if ndiff=0 then begin
            DrawGrid.Canvas.Brush.Color := RGB(255,128,0);
          end else if ndiff<=8 then begin
            DrawGrid.Canvas.Brush.Color := FromColorToColor(RGB(253,250,115),ColorToRGB(clGreen),ndiff-1,8-1);
          end else begin
            DrawGrid.Canvas.Brush.Color := clGreen;
          end;
          DrawGrid.Canvas.Ellipse(Rect.Left+1,Rect.Top+1,Rect.Right-1,Rect.Bottom-1);
        End;
        act_name : Begin
          s := account.name;
          Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfLeft,tfVerticalCenter,tfSingleLine]);
        end;
        act_type : Begin
          s := IntToStr(account.account_type);
          Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfRight,tfVerticalCenter,tfSingleLine]);
        end;
        act_saleprice : Begin
          if TAccountComp.IsAccountForSale(account.accountInfo) then begin
            // Show price for sale
            s := TAccountComp.FormatMoney(account.accountInfo.price);
            if TAccountComp.IsAccountForSaleAcceptingTransactions(account.accountInfo) then begin
              if TAccountComp.IsAccountLocked(account.accountInfo,Node.Bank.BlocksCount) then begin
                DrawGrid.Canvas.Font.Color := clNavy;
              end else begin
                DrawGrid.Canvas.Font.Color := clRed;
              end;
            end else begin
              DrawGrid.Canvas.Font.Color := clGrayText
            end;
          end else s := '';
          Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfRight,tfVerticalCenter,tfSingleLine]);
        end;
      else
        s := '(???)';
        Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfCenter,tfVerticalCenter,tfSingleLine]);
      end;
    end;
  end;
end;

procedure TAccountsGrid.OnNodeNewOperation(Sender: TObject);
begin
  If Assigned(FDrawGrid) then FDrawGrid.Invalidate;
end;

procedure TAccountsGrid.SaveToStream(Stream: TStream);
Var c,i,j : Integer;
begin
  c := Length(FColumns);
  Stream.Write(c,sizeof(c));
  for i := 0 to c - 1 do begin
    j := Integer(FColumns[i].ColumnType);
    Stream.Write(j,sizeof(j));
    if Assigned(FDrawGrid) then begin
      FColumns[i].width := FDrawGrid.ColWidths[i];
    end;
    Stream.Write(FColumns[i].width,sizeof(FColumns[i].width));
  end;
  j := FDrawGrid.Width;
  Stream.Write(j,sizeof(j));
  j := FDrawGrid.Height;
  Stream.Write(j,sizeof(j));
end;

function TAccountsGrid.SelectedAccounts(accounts: TOrderedCardinalList): Integer;
var i64 : Int64;
  i : Integer;
begin
  accounts.Clear;
  Result := 0;
  if not assigned(FDrawGrid) then exit;
  if FAllowMultiSelect then begin
    for i := FDrawGrid.Selection.Top to FDrawGrid.Selection.Bottom do begin
      i64 := AccountNumber(i);
      if i64>=0 then accounts.Add(i64);
    end;
  end;
  If accounts.Count=0 then begin
    i64 := AccountNumber(DrawGrid.Row);
    if i64>=0 then accounts.Add(i64);
  end;
  Result := accounts.Count;
end;

procedure TAccountsGrid.SetAllowMultiSelect(const Value: Boolean);
begin
  FAllowMultiSelect := Value;
  InitGrid;
end;

procedure TAccountsGrid.SetDrawGrid(const Value: TDrawGrid);
begin
  if FDrawGrid=Value then exit;
  FDrawGrid := Value;
  if Assigned(Value) then begin
    Value.FreeNotification(self);
    FDrawGrid.OnDrawCell := OnGridDrawCell;
    InitGrid;
  end;
end;

procedure TAccountsGrid.SetNode(const Value: TNode);
begin
  if GetNode=Value then exit;
  FNodeNotifyEvents.Node := Value;
  InitGrid;
end;

procedure TAccountsGrid.SetShowAllAccounts(const Value: Boolean);
begin
  if FShowAllAccounts=Value then exit;
  FShowAllAccounts := Value;
  InitGrid;
end;

procedure TAccountsGrid.UnlockAccountsList;
begin
  InitGrid;
end;

{ TOperationsGrid }

constructor TOperationsGrid.Create(AOwner: TComponent);
begin
  FAccountNumber := 0;
  FDrawGrid := Nil;
  MustShowAlwaysAnAccount := false;
  FOperationsResume := TOperationsResumeList.Create;
  FNodeNotifyEvents := TNodeNotifyEvents.Create(Self);
  FNodeNotifyEvents.OnBlocksChanged := OnNodeNewAccount;
  FNodeNotifyEvents.OnOperationsChanged := OnNodeNewOperation;
  FBlockStart := -1;
  FBlockEnd := -1;
  FPendingOperations := false;
  inherited;
end;

destructor TOperationsGrid.Destroy;
begin
  FOperationsResume.Free;
  FNodeNotifyEvents.Free;
  inherited;
end;

function TOperationsGrid.GetNode: TNode;
begin
  Result := FNodeNotifyEvents.Node;
end;

procedure TOperationsGrid.InitGrid;
begin
  if Not Assigned(FDrawGrid) then exit;
  if FOperationsResume.Count>0 then FDrawGrid.RowCount := FOperationsResume.Count+1
  else FDrawGrid.RowCount := 2;
  DrawGrid.FixedRows := 1;
  DrawGrid.DefaultDrawing := true;
  DrawGrid.FixedCols := 0;
  DrawGrid.ColCount := 8;
  DrawGrid.ColWidths[0] := 110; // Time
  DrawGrid.ColWidths[1] := 70; // Block/Op
  DrawGrid.ColWidths[2] := 60; // Account
  DrawGrid.ColWidths[3] := 180; // OpType
  DrawGrid.ColWidths[4] := 70; // Amount
  DrawGrid.ColWidths[5] := 60; // Operation Fee
  DrawGrid.ColWidths[6] := 80; // Balance
  DrawGrid.ColWidths[7] := 500; // Payload
  FDrawGrid.DefaultRowHeight := 18;
  FDrawGrid.Invalidate;
  DrawGrid.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
    {goRangeSelect, }goDrawFocusSelected, {goRowSizing, }goColSizing, {goRowMoving,}
    {goColMoving, goEditing, }goTabs, goRowSelect, {goAlwaysShowEditor,}
    goThumbTracking{$IFnDEF FPC}, goFixedColClick, goFixedRowClick, goFixedHotTrack{$ENDIF}];
end;

procedure TOperationsGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation=opRemove then begin
    if (AComponent=FDrawGrid) then begin
      SetDrawGrid(Nil);
    end;
  end;
end;

procedure TOperationsGrid.OnGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
Var s : String;
  opr : TOperationResume;
begin
  {.$IFDEF FPC}
  DrawGrid.Canvas.Font.Color:=clBlack;
  {.$ENDIF}
  opr := CT_TOperationResume_NUL;
  Try
  if (ARow=0) then begin
    // Header
    case ACol of
      0 : s := 'Time';
      1 : s := 'Block/Op';
      2 : s := 'Account';
      3 : s := 'Operation';
      4 : s := 'Amount';
      5 : s := 'Fee';
      6 : s := 'Balance';
      7 : s := 'Payload';
    else s:= '';
    end;
    Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfCenter,tfVerticalCenter]);
  end else begin
    if (gdSelected in State) then
      If (gdFocused in State) then DrawGrid.Canvas.Brush.Color := clGradientActiveCaption
      else DrawGrid.Canvas.Brush.Color := clGradientInactiveCaption
    else DrawGrid.Canvas.Brush.Color := clWindow;
    DrawGrid.Canvas.FillRect(Rect);
    InflateRect(Rect,-2,-1);
    if (ARow<=FOperationsResume.Count) then begin
      opr := FOperationsResume.OperationResume[ARow-1];
      If (opr.AffectedAccount=opr.SignerAccount) then begin
      end else begin
        if (gdSelected in State) or (gdFocused in State) then begin
        end else DrawGrid.Canvas.font.Color := clGrayText;
      end;
      if ACol=0 then begin
        if opr.time=0 then s := '(Pending)'
        else s := DateTimeToStr(UnivDateTime2LocalDateTime(UnixToUnivDateTime(opr.time)));
        Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfleft,tfVerticalCenter,tfSingleLine]);
      end else if ACol=1 then begin
        s := Inttostr(opr.Block);
        if opr.NOpInsideBlock>=0 then s := s + '/'+Inttostr(opr.NOpInsideBlock+1);
        Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfleft,tfVerticalCenter,tfSingleLine]);
      end else if ACol=2 then begin
        s := TAccountComp.AccountNumberToAccountTxtNumber(opr.AffectedAccount);
        Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfleft,tfVerticalCenter,tfSingleLine]);
      end else if ACol=3 then begin
        s := opr.OperationTxt;
        Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfleft,tfVerticalCenter,tfSingleLine]);
      end else if ACol=4 then begin
        s := TAccountComp.FormatMoney(opr.Amount);
        if opr.Amount>0 then DrawGrid.Canvas.Font.Color := ClGreen
        else if opr.Amount=0 then DrawGrid.Canvas.Font.Color := clGrayText
        else DrawGrid.Canvas.Font.Color := clRed;
        Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfRight,tfVerticalCenter,tfSingleLine]);
      end else if ACol=5 then begin
        s := TAccountComp.FormatMoney(opr.Fee);
        if opr.Fee>0 then DrawGrid.Canvas.Font.Color := ClGreen
        else if opr.Fee=0 then DrawGrid.Canvas.Font.Color := clGrayText
        else DrawGrid.Canvas.Font.Color := clRed;
        Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfRight,tfVerticalCenter,tfSingleLine]);
      end else if ACol=6 then begin
        if opr.time=0 then begin
          // Pending operation... showing final balance
          DrawGrid.Canvas.Font.Color := clBlue;
          s := '('+TAccountComp.FormatMoney(opr.Balance)+')';
        end else begin
          s := TAccountComp.FormatMoney(opr.Balance);
          if opr.Balance>0 then DrawGrid.Canvas.Font.Color := ClGreen
          else if opr.Balance=0 then DrawGrid.Canvas.Font.Color := clGrayText
          else DrawGrid.Canvas.Font.Color := clRed;
        end;
        Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfRight,tfVerticalCenter,tfSingleLine]);
      end else if ACol=7 then begin
        s := opr.PrintablePayload;
        Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfLeft,tfVerticalCenter,tfSingleLine]);
      end else begin
        s := '(???)';
        Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfCenter,tfVerticalCenter,tfSingleLine]);
      end;
    end;
  end;
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,Classname,Format('Error at OnGridDrawCell row %d col %d Block %d - %s',[ARow,ACol,opr.Block,E.Message]));
    end;
  End;
end;

procedure TOperationsGrid.OnNodeNewAccount(Sender: TObject);
begin
  If (AccountNumber<0) And (FBlockEnd<0) And (Not FPendingOperations) then UpdateAccountOperations;
end;

procedure TOperationsGrid.OnNodeNewOperation(Sender: TObject);
Var //Op : TPCOperation;
  l : TList;
begin
  Try
    if (AccountNumber<0) then begin
      If (FPendingOperations) then UpdateAccountOperations;
    end else begin
      l := TList.Create;
      Try
        If Node.Operations.OperationsHashTree.GetOperationsAffectingAccount(AccountNumber,l)>0 then begin
          if l.IndexOf(TObject(PtrInt(AccountNumber)))>=0 then UpdateAccountOperations;
        end;
      Finally
        l.Free;
      End;
    end;
  Except
    On E:Exception do begin
      E.message := 'Exception on updating OperationsGrid '+inttostr(AccountNumber)+': '+E.Message;
      Raise;
    end;
  end;
end;

procedure TOperationsGrid.SetAccountNumber(const Value: Int64);
begin
  if FAccountNumber=Value then exit;
  FAccountNumber := Value;
  if FAccountNumber>=0 then FPendingOperations := false;
  UpdateAccountOperations;
end;

procedure TOperationsGrid.SetBlockEnd(const Value: Int64);
begin
  FBlockEnd := Value;
end;

procedure TOperationsGrid.SetBlocks(bstart, bend: Int64);
begin
  if (bstart=FBlockStart) And (bend=FBlockEnd) then exit;
  FBlockStart := bstart;
  FBlockEnd := bend;
  if (FBlockEnd>0) And (FBlockStart>FBlockEnd) then FBlockStart := -1;
  FAccountNumber := -1;
  FPendingOperations := false;
  UpdateAccountOperations;
end;

procedure TOperationsGrid.SetBlockStart(const Value: Int64);
begin
  FBlockStart := Value;
end;

procedure TOperationsGrid.SetDrawGrid(const Value: TDrawGrid);
begin
  if FDrawGrid=Value then exit;
  FDrawGrid := Value;
  if Assigned(Value) then begin
    Value.FreeNotification(self);
    FDrawGrid.OnDrawCell := OnGridDrawCell;
    InitGrid;
  end;
end;

procedure TOperationsGrid.SetMustShowAlwaysAnAccount(const Value: Boolean);
begin
  if FMustShowAlwaysAnAccount=Value then exit;
  FMustShowAlwaysAnAccount := Value;
  UpdateAccountOperations;
end;

procedure TOperationsGrid.SetNode(const Value: TNode);
begin
  if GetNode=Value then exit;
  FNodeNotifyEvents.Node := Value;
  UpdateAccountOperations; // New Build 1.0.3
end;

procedure TOperationsGrid.SetPendingOperations(const Value: Boolean);
begin
  FPendingOperations := Value;
  if FPendingOperations then  FAccountNumber := -1;
  UpdateAccountOperations;
end;

function TOperationsGrid.GetSelectedOperation : TOperationResume;
Var i : Integer;
  opr : TOperationResume;
  FRM : TFRMPayloadDecoder;
begin
  if Not Assigned(FDrawGrid) then exit;
  if (FDrawGrid.Row<=0) Or (FDrawGrid.Row>FOperationsResume.Count) then begin
    Result := CT_TOperationResume_NUL;
    exit;
  end;
  Result := FOperationsResume.OperationResume[FDrawGrid.Row-1];
end;

procedure TOperationsGrid.ShowModalDecoder(WalletKeys: TWalletKeys; AppParams : TAppParams);
Var i : Integer;
  opr : TOperationResume;
  FRM : TFRMPayloadDecoder;
begin
  if Not Assigned(FDrawGrid) then exit;
  if (FDrawGrid.Row<=0) Or (FDrawGrid.Row>FOperationsResume.Count) then exit;
  opr := FOperationsResume.OperationResume[FDrawGrid.Row-1];
  FRM := TFRMPayloadDecoder.Create(FDrawGrid.Owner);
  try
    FRM.Init(opr,WalletKeys,AppParams);
    FRM.ShowModal;
  finally
    FRM.Free;
  end;
end;

procedure TOperationsGrid.UpdateAccountOperations;
Var list : TList;
  i,j : Integer;
  OPR : TOperationResume;
  Op : TPCOperation;
  opc : TPCOperationsComp;
  bstart,bend : int64;
begin
  FOperationsResume.Clear;
  Try
    if Not Assigned(Node) then exit;
    if (MustShowAlwaysAnAccount) And (AccountNumber<0) then exit;

    if FPendingOperations then begin
      for i := Node.Operations.Count - 1 downto 0 do begin
        Op := Node.Operations.OperationsHashTree.GetOperation(i);
        If TPCOperation.OperationToOperationResume(0,Op,Op.SignerAccount,OPR) then begin
          OPR.NOpInsideBlock := i;
          OPR.Block := Node.Bank.BlocksCount;
          OPR.Balance := Node.Operations.SafeBoxTransaction.Account(Op.SignerAccount).balance;
          FOperationsResume.Add(OPR);
        end;
      end;
    end else begin
      if AccountNumber<0 then begin
        opc := TPCOperationsComp.Create(Nil);
        try
          opc.bank := Node.Bank;
          If FBlockEnd<0 then begin
            If Node.Bank.BlocksCount>0 then bend := Node.Bank.BlocksCount-1
            else bend := 0;
          end else bend := FBlockEnd;
          if FBlockStart<0 then begin
            if (bend > 300) then bstart := bend - 300
            else bstart := 0;
          end else bstart:= FBlockStart;
          If bstart<0 then bstart := 0;
          if bend>=Node.Bank.BlocksCount then bend:=Node.Bank.BlocksCount;
          while (bstart<=bend) do begin
            opr := CT_TOperationResume_NUL;
            if (Node.Bank.Storage.LoadBlockChainBlock(opc,bend)) then begin
              // Reward operation
              OPR := CT_TOperationResume_NUL;
              OPR.valid := true;
              OPR.Block := bend;
              OPR.time := opc.OperationBlock.timestamp;
              OPR.AffectedAccount := bend * CT_AccountsPerBlock;
              OPR.Amount := opc.OperationBlock.reward;
              OPR.Fee := opc.OperationBlock.fee;
              OPR.Balance := OPR.Amount+OPR.Fee;
              OPR.OperationTxt := 'Blockchain reward';
              FOperationsResume.Add(OPR);
              // Reverse operations inside a block
              for i := opc.Count - 1 downto 0 do begin
                if TPCOperation.OperationToOperationResume(bend,opc.Operation[i],opc.Operation[i].SignerAccount,opr) then begin
                  opr.NOpInsideBlock := i;
                  opr.Block := bend;
                  opr.time := opc.OperationBlock.timestamp;
                  FOperationsResume.Add(opr);
                end;
              end;
            end else break;
            dec(bend);
          end;
        finally
          opc.Free;
        end;

      end else begin
        list := TList.Create;
        Try
          Node.Operations.OperationsHashTree.GetOperationsAffectingAccount(AccountNumber,list);
          for i := list.Count - 1 downto 0 do begin
            Op := Node.Operations.OperationsHashTree.GetOperation(PtrInt(list[i]));
            If TPCOperation.OperationToOperationResume(0,Op,AccountNumber,OPR) then begin
              OPR.NOpInsideBlock := i;
              OPR.Block := Node.Operations.OperationBlock.block;
              OPR.Balance := Node.Operations.SafeBoxTransaction.Account(AccountNumber).balance;
              FOperationsResume.Add(OPR);
            end;
          end;
        Finally
          list.Free;
        End;
        Node.GetStoredOperationsFromAccount(FOperationsResume,AccountNumber,100,0,5000);
      end;
    end;
  Finally
    InitGrid;
  End;
end;

{ TBlockChainGrid }

constructor TBlockChainGrid.Create(AOwner: TComponent);
begin
  inherited;
  FBlockStart:=-1;
  FBlockEnd:=-1;
  FMaxBlocks := 300;
  FDrawGrid := Nil;
  FNodeNotifyEvents := TNodeNotifyEvents.Create(Self);
  FNodeNotifyEvents.OnBlocksChanged := OnNodeNewAccount;
  FHashRateAverageBlocksCount := 50;
  SetLength(FBlockChainDataArray,0);
  FShowTimeAverageColumns:=False;
end;

destructor TBlockChainGrid.Destroy;
begin
  FNodeNotifyEvents.OnBlocksChanged := Nil;
  FNodeNotifyEvents.Node := Nil;
  FreeAndNil(FNodeNotifyEvents);
  inherited;
end;

function TBlockChainGrid.GetNode: TNode;
begin
  Result := FNodeNotifyEvents.Node;
end;


procedure TBlockChainGrid.InitGrid;
begin
  if Not Assigned(FDrawGrid) then exit;
  FDrawGrid.RowCount := 2;
  DrawGrid.FixedRows := 1;
  DrawGrid.DefaultDrawing := true;
  DrawGrid.FixedCols := 0;
  If ShowTimeAverageColumns then DrawGrid.ColCount:=15
  else DrawGrid.ColCount:=12;
  DrawGrid.ColWidths[0] := 50; // Block
  DrawGrid.ColWidths[1] := 110; // Time
  DrawGrid.ColWidths[2] := 30; // Ops
  DrawGrid.ColWidths[3] := 80; // Volume
  DrawGrid.ColWidths[4] := 50; // Reward
  DrawGrid.ColWidths[5] := 50; // Fee
  DrawGrid.ColWidths[6] := 60; // Target
  DrawGrid.ColWidths[7] := 80; // Hash Rate
  DrawGrid.ColWidths[8] := 190; // Miner Payload
  DrawGrid.ColWidths[9] := 190; // PoW
  DrawGrid.ColWidths[10] := 190; // SafeBox Hash
  DrawGrid.ColWidths[11] := 50; // Protocol
  If ShowTimeAverageColumns then begin
    DrawGrid.ColWidths[12] := 95; // Accumulated work
    DrawGrid.ColWidths[13] := 55; // Deviation
    DrawGrid.ColWidths[14] := 340; // Time average
  end;
  FDrawGrid.DefaultRowHeight := 18;
  FDrawGrid.Invalidate;
  DrawGrid.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
    {goRangeSelect, }goDrawFocusSelected, {goRowSizing, }goColSizing, {goRowMoving,}
    {goColMoving, goEditing, }goTabs, goRowSelect, {goAlwaysShowEditor,}
    goThumbTracking{$IFnDEF FPC}, goFixedColClick, goFixedRowClick, goFixedHotTrack{$ENDIF}];
  UpdateBlockChainGrid;
end;


procedure TBlockChainGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation=opRemove then begin
    if (AComponent=FDrawGrid) then begin
      SetDrawGrid(Nil);
    end;
  end;
end;

procedure TBlockChainGrid.OnGridDrawCell(Sender: TObject; ACol, ARow: Longint;
  Rect: TRect; State: TGridDrawState);
Var s : String;
  bcd : TBlockChainData;
  deviation : Real;
begin
  {.$IFDEF FPC}
  DrawGrid.Canvas.Font.Color:=clBlack;
  {.$ENDIF}
  if (ARow=0) then begin
    // Header
    case ACol of
      0 : s := 'Block';
      1 : s := 'Time';
      2 : s := 'Ops';
      3 : s := 'Volume';
      4 : s := 'Reward';
      5 : s := 'Fee';
      6 : s := 'Target';
      7 : s := 'Mh/s';
      8 : s := 'Miner Payload';
      9 : s := 'Proof of Work';
      10 : s := 'SafeBox Hash';
      11 : s := 'Protocol';
      12 : s := 'Acc.Work';
      13 : s := 'Deviation';
      14 : s := 'Time average';
    else s:= '';
    end;
    Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfCenter,tfVerticalCenter]);
  end else begin
    if (gdSelected in State) then
      If (gdFocused in State) then DrawGrid.Canvas.Brush.Color := clGradientActiveCaption
      else DrawGrid.Canvas.Brush.Color := clGradientInactiveCaption
    else DrawGrid.Canvas.Brush.Color := clWindow;
    DrawGrid.Canvas.FillRect(Rect);
    InflateRect(Rect,-2,-1);
    if ((ARow-1)<=High(FBlockChainDataArray)) then begin
      bcd := FBlockChainDataArray[ARow-1];
      if ACol=0 then begin
        s := IntToStr(bcd.Block);
        Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfRight,tfVerticalCenter]);
      end else if ACol=1 then begin
        s := DateTimeToStr(UnivDateTime2LocalDateTime(UnixToUnivDateTime((bcd.Timestamp))));
        Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfleft,tfVerticalCenter,tfSingleLine]);
      end else if ACol=2 then begin
        if bcd.OperationsCount>=0 then begin
          s := IntToStr(bcd.OperationsCount);
          Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfRight,tfVerticalCenter]);
        end else begin
          DrawGrid.Canvas.Font.Color := clGrayText;
          s := '(no data)';
          Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfCenter,tfVerticalCenter,tfSingleLine]);
        end;
      end else if ACol=3 then begin
        if bcd.Volume>=0 then begin
          s := TAccountComp.FormatMoney(bcd.Volume);
          if FBlockChainDataArray[ARow-1].Volume>0 then DrawGrid.Canvas.Font.Color := ClGreen
          else DrawGrid.Canvas.Font.Color := clGrayText;
          Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfRight,tfVerticalCenter,tfSingleLine]);
        end else begin
          DrawGrid.Canvas.Font.Color := clGrayText;
          s := '(no data)';
          Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfCenter,tfVerticalCenter,tfSingleLine]);
        end;
      end else if ACol=4 then begin
        s := TAccountComp.FormatMoney(bcd.Reward);
        if FBlockChainDataArray[ARow-1].Reward>0 then DrawGrid.Canvas.Font.Color := ClGreen
        else DrawGrid.Canvas.Font.Color := clGrayText;
        Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfRight,tfVerticalCenter,tfSingleLine]);
      end else if ACol=5 then begin
        s := TAccountComp.FormatMoney(bcd.Fee);
        if bcd.Fee>0 then DrawGrid.Canvas.Font.Color := ClGreen
        else DrawGrid.Canvas.Font.Color := clGrayText;
        Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfRight,tfVerticalCenter,tfSingleLine]);
      end else if ACol=6 then begin
        s := IntToHex(bcd.Target,8);
        Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfLeft,tfVerticalCenter]);
      end else if ACol=7 then begin
        s := Format('%.2n',[bcd.HashRateKhs/1024]);
        Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfRight,tfVerticalCenter]);
      end else if ACol=8 then begin
        if TCrypto.IsHumanReadable(bcd.MinerPayload) then
          s := bcd.MinerPayload
        else s := TCrypto.ToHexaString( bcd.MinerPayload );
        Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfLeft,tfVerticalCenter]);
      end else if ACol=9 then begin
        s := TCrypto.ToHexaString(bcd.PoW);
        Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfLeft,tfVerticalCenter]);
      end else if ACol=10 then begin
        s := TCrypto.ToHexaString(bcd.SafeBoxHash);
        Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfLeft,tfVerticalCenter]);
      end else if ACol=11 then begin
        s := Inttostr(bcd.BlockProtocolVersion)+'-'+IntToStr(bcd.BlockProtocolAvailable);
        Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfCenter,tfVerticalCenter,tfSingleLine]);
      end else if ACol=12 then begin
        if bcd.AccumulatedWork>0 then begin
          s := Inttostr(bcd.AccumulatedWork);
          Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfRight,tfVerticalCenter]);
        end else begin
          DrawGrid.Canvas.Font.Color := clGrayText;
          s := '(no data)';
          Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfCenter,tfVerticalCenter,tfSingleLine]);
        end;
      end else if ACol=13 then begin
        deviation := ((CT_NewLineSecondsAvg - bcd.TimeAverage100) / CT_NewLineSecondsAvg)*100;
        s := Format('%.2f',[deviation])+' %';
        Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfRight,tfVerticalCenter,tfSingleLine]);
      end else if ACol=14 then begin
        s := Format('200:%.1f 150:%.1f 100:%.1f 75:%.1f 50:%.1f 25:%.1f 10:%.1f',[bcd.TimeAverage200,
           bcd.TimeAverage150,bcd.TimeAverage100,bcd.TimeAverage75,bcd.TimeAverage50,bcd.TimeAverage25,bcd.TimeAverage10]);
        Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfLeft,tfVerticalCenter,tfSingleLine]);
      end;
    end;
  end;
end;


procedure TBlockChainGrid.OnNodeNewAccount(Sender: TObject);
begin
  if FBlockEnd<0 then UpdateBlockChainGrid;
end;


procedure TBlockChainGrid.SetBlockEnd(const Value: Int64);
begin
  if FBlockEnd=Value then exit;
  FBlockEnd := Value;
  UpdateBlockChainGrid;
end;


procedure TBlockChainGrid.SetBlocks(bstart, bend: Int64);
begin
  if (FBlockStart=bstart) And (FBlockEnd=bend) then exit;
  FBlockStart := bstart;
  FBlockEnd := bend;
  UpdateBlockChainGrid;
end;


procedure TBlockChainGrid.SetBlockStart(const Value: Int64);
begin
  If FBlockStart=Value then exit;
  FBlockStart := Value;
  UpdateBlockChainGrid;
end;


procedure TBlockChainGrid.SetDrawGrid(const Value: TDrawGrid);
begin
  if FDrawGrid=Value then exit;
  FDrawGrid := Value;
  if Assigned(Value) then begin
    Value.FreeNotification(self);
    FDrawGrid.OnDrawCell := OnGridDrawCell;
    InitGrid;
  end;
end;


procedure TBlockChainGrid.SetHashRateAverageBlocksCount(const Value: Integer);
begin
  if FHashRateAverageBlocksCount=Value then exit;
  FHashRateAverageBlocksCount := Value;
  if FHashRateAverageBlocksCount<1 then FHashRateAverageBlocksCount := 1;
  if FHashRateAverageBlocksCount>1000 then FHashRateAverageBlocksCount := 1000;
  UpdateBlockChainGrid;
end;

procedure TBlockChainGrid.SetShowTimeAverageColumns(AValue: Boolean);
begin
  if FShowTimeAverageColumns=AValue then Exit;
  FShowTimeAverageColumns:=AValue;
  InitGrid;
end;

procedure TBlockChainGrid.SetMaxBlocks(const Value: Integer);
begin
  if FMaxBlocks=Value then exit;
  FMaxBlocks := Value;
  if (FMaxBlocks<=0) Or (FMaxBlocks>500) then FMaxBlocks := 300;
  UpdateBlockChainGrid;
end;


procedure TBlockChainGrid.SetNode(const Value: TNode);
begin
  FNodeNotifyEvents.Node := Value;
  UpdateBlockChainGrid;
end;


procedure TBlockChainGrid.UpdateBlockChainGrid;
Var nstart,nend : Cardinal;
  opc : TPCOperationsComp;
  bcd : TBlockChainData;
  i : Integer;
  opb : TOperationBlock;
begin
  if (FBlockStart>FBlockEnd) And (FBlockStart>=0) then FBlockEnd := -1;
  if (FBlockEnd>=0) And (FBlockEnd<FBlockStart) then FBlockStart:=-1;

  if Not Assigned(FNodeNotifyEvents.Node) then exit;

  if FBlockStart>(FNodeNotifyEvents.Node.Bank.BlocksCount-1) then FBlockStart := -1;

  try
    if Node.Bank.BlocksCount<=0 then begin
      SetLength(FBlockChainDataArray,0);
      exit;
    end;
    if (FBlockEnd>=0) And (FBlockEnd<Node.Bank.BlocksCount) then begin
      nend := FBlockEnd
    end else begin
      if (FBlockStart>=0) And (FBlockStart+MaxBlocks<=Node.Bank.BlocksCount) then nend := FBlockStart + MaxBlocks - 1
      else nend := Node.Bank.BlocksCount-1;
    end;

    if (FBlockStart>=0) And (FBlockStart<Node.Bank.BlocksCount) then nstart := FBlockStart
    else begin
      if nend>MaxBlocks then nstart := nend - MaxBlocks + 1
      else nstart := 0;
    end;
    SetLength(FBlockChainDataArray,nend - nstart +1);
    opc := TPCOperationsComp.Create(Nil);
    try
      opc.bank := Node.Bank;
      while (nstart<=nend) do begin
        i := length(FBlockChainDataArray) - (nend-nstart+1);
        bcd := CT_TBlockChainData_NUL;
        opb := Node.Bank.SafeBox.Block(nend).blockchainInfo;
        bcd.Block:=opb.block;
        bcd.Timestamp := opb.timestamp;
        bcd.BlockProtocolVersion := opb.protocol_version;
        bcd.BlockProtocolAvailable := opb.protocol_available;
        bcd.Reward := opb.reward;
        bcd.Fee := opb.fee;
        bcd.Target := opb.compact_target;
        bcd.HashRateKhs := Node.Bank.SafeBox.CalcBlockHashRateInKhs(bcd.Block,HashRateAverageBlocksCount);
        bcd.MinerPayload := opb.block_payload;
        bcd.PoW := opb.proof_of_work;
        bcd.SafeBoxHash := opb.initial_safe_box_hash;
        bcd.AccumulatedWork := Node.Bank.SafeBox.Block(bcd.Block).AccumulatedWork;
        if (Node.Bank.LoadOperations(opc,nend)) then begin
          bcd.OperationsCount := opc.Count;
          bcd.Volume := opc.OperationsHashTree.TotalAmount + opc.OperationsHashTree.TotalFee;
        end;
        bcd.TimeAverage200:=Node.Bank.GetTargetSecondsAverage(bcd.Block,200);
        bcd.TimeAverage150:=Node.Bank.GetTargetSecondsAverage(bcd.Block,150);
        bcd.TimeAverage100:=Node.Bank.GetTargetSecondsAverage(bcd.Block,100);
        bcd.TimeAverage75:=Node.Bank.GetTargetSecondsAverage(bcd.Block,75);
        bcd.TimeAverage50:=Node.Bank.GetTargetSecondsAverage(bcd.Block,50);
        bcd.TimeAverage25:=Node.Bank.GetTargetSecondsAverage(bcd.Block,25);
        bcd.TimeAverage10:=Node.Bank.GetTargetSecondsAverage(bcd.Block,10);
        FBlockChainDataArray[i] := bcd;
        if (nend>0) then dec(nend) else break;
      end;
    finally
      opc.Free;
    end;
  finally
    if Assigned(FDrawGrid) then begin
      if Length(FBlockChainDataArray)>0 then FDrawGrid.RowCount := length(FBlockChainDataArray)+1
      else FDrawGrid.RowCount := 2;
      FDrawGrid.Invalidate;
    end;
  end;
end;

end.
