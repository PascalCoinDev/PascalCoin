unit UGridUtils;

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
  Classes, Grids, Windows, UNode, UAccounts, UBlockChain,
  UWalletKeys;

Type
  // TAccountsGrid implements a visual integration of TDrawGrid
  // to show accounts information
  TAccountColumnType = (act_account_number,act_account_key,act_balance,act_updated,act_n_operation,act_updated_state);
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
    procedure SetDrawGrid(const Value: TDrawGrid);
    Procedure InitGrid;
    Procedure OnNodeNewOperation(Sender : TObject);
    procedure OnGridDrawCell(Sender: TObject; ACol, ARow: Longint; Rect: TRect; State: TGridDrawState);
    procedure SetNode(const Value: TNode);
    function GetNode: TNode;
    procedure SetShowAllAccounts(const Value: Boolean);
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
  End;

implementation

uses
  Graphics, UCrypto, SysUtils, UTime, UOpTransaction, UConst,
  UFRMPayloadDecoder, ULog;

{ TAccountsGrid }

Const CT_ColumnHeader : Array[TAccountColumnType] Of String =
  ('Account N.','Key','Balance','Updated','N Oper.','State');

function TAccountsGrid.AccountNumber(GridRow: Integer): Int64;
begin
  if GridRow<1 then Result := -1
  else if FShowAllAccounts then begin
    if Assigned(Node) then begin
      Result := GridRow-1;
    end else Result := -1;
  end else if GridRow<=FAccountsList.Count then begin
    Result := Integer(FAccountsList.Get(GridRow-1));
  end else Result := -1;
end;

constructor TAccountsGrid.Create(AOwner: TComponent);
Var i : Integer;
begin
  inherited;
  FOnUpdated := Nil;
  FAccountsBalance := 0;
  FAccountsCount := 0;
  FShowAllAccounts := false;
  FAccountsList := TOrderedCardinalList.Create;
  FDrawGrid := Nil;
  SetLength(FColumns,4);
  FColumns[0].ColumnType := act_account_number;
  FColumns[0].width := 80;
  FColumns[1].ColumnType := act_balance;
  FColumns[1].width := 100;
  FColumns[2].ColumnType := act_n_operation;
  FColumns[2].width := 50;
  FColumns[3].ColumnType := act_updated_state;
  FColumns[3].width := 50;
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
    goThumbTracking, goFixedColClick, goFixedRowClick, goFixedHotTrack];
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
  if (ARow=0) then begin
    // Header
    s := CT_ColumnHeader[C.ColumnType];
    DrawGrid.Canvas.TextRect(Rect,s,[tfCenter,tfVerticalCenter]);
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
          DrawGrid.Canvas.TextRect(Rect,s,[tfRight,tfVerticalCenter,tfSingleLine]);
        End;
        act_account_key : Begin
          s := Tcrypto.ToHexaString(TAccountComp.AccountKey2RawString(account.accountkey));
          DrawGrid.Canvas.TextRect(Rect,s,[tfLeft,tfVerticalCenter,tfSingleLine]);
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
          DrawGrid.Canvas.TextRect(Rect,s,[tfRight,tfVerticalCenter,tfSingleLine]);
        End;
        act_updated : Begin
          s := Inttostr(account.updated_block);
          DrawGrid.Canvas.TextRect(Rect,s,[tfRight,tfVerticalCenter,tfSingleLine]);
        End;
        act_n_operation : Begin
          s := InttoStr(account.n_operation);
          DrawGrid.Canvas.TextRect(Rect,s,[tfRight,tfVerticalCenter,tfSingleLine]);
        End;
        act_updated_state : Begin
          if TAccountComp.IsAccountBlockedByProtocol(account.account,Node.Bank.BlocksCount) then begin
            DrawGrid.Canvas.Brush.Color := clRed;
            DrawGrid.Canvas.Ellipse(Rect.Left+1,Rect.Top+1,Rect.Right-1,Rect.Bottom-1);
          end else if ndiff=0 then begin
            DrawGrid.Canvas.Brush.Color := RGB(255,128,0);
            DrawGrid.Canvas.Ellipse(Rect.Left+1,Rect.Top+1,Rect.Right-1,Rect.Bottom-1);
          end else if ndiff<=8 then begin
            DrawGrid.Canvas.Brush.Color := FromColorToColor(RGB(253,250,115),ColorToRGB(clGreen),ndiff-1,8-1);
            DrawGrid.Canvas.Ellipse(Rect.Left+1,Rect.Top+1,Rect.Right-1,Rect.Bottom-1);
          end else begin
            DrawGrid.Canvas.Brush.Color := clGreen;
            DrawGrid.Canvas.Ellipse(Rect.Left+1,Rect.Top+1,Rect.Right-1,Rect.Bottom-1);
          end;
        End;
      else
        s := '(???)';
        DrawGrid.Canvas.TextRect(Rect,s,[tfCenter,tfVerticalCenter,tfSingleLine]);
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

end.
