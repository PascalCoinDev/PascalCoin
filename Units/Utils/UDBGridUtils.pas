unit UDBGridUtils;

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  ABOUT THIS UNIT:

  This units works with UDBStorage.pas unit.
  So, is to use in Windows version due UDBStorage works with Access database

  }

interface

uses
  Classes, Grids, Windows, UNode, UAccounts, UBlockChain, UDBStorage, DBGrids, DB, ADODB,
  UWalletKeys, UAppParams;

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

  TOperationsGrid = Class(TComponent)
  private
    FDrawGrid: TDrawGrid;
    FAccountNumber: Int64;
    FOperationsResume : TOperationsResumeList;
    FNodeNotifyEvents : TNodeNotifyEvents;
    FPendingOperations: Boolean;
    Procedure OnNodeNewOperation(Sender : TObject);
    Procedure OnNodeNewAccount(Sender : TObject);
    Procedure InitGrid;
    procedure OnGridDrawCell(Sender: TObject; ACol, ARow: Longint; Rect: TRect; State: TGridDrawState);
    procedure SetDrawGrid(const Value: TDrawGrid);
    procedure SetAccountNumber(const Value: Int64);
    procedure SetNode(const Value: TNode);
    function GetNode: TNode;
    procedure SetPendingOperations(const Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); Override;
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Property DrawGrid : TDrawGrid read FDrawGrid write SetDrawGrid;
    Property PendingOperations : Boolean read FPendingOperations write SetPendingOperations;
    Property AccountNumber : Int64 read FAccountNumber write SetAccountNumber;
    Property Node : TNode read GetNode write SetNode;
    Procedure UpdateAccountOperations;
    Procedure ShowModalDecoder(WalletKeys: TWalletKeys; AppParams : TAppParams);
  End;

  TOperationsDBGrid = Class(TComponent)
  private
    FDisableds : Integer;
    FQrySQL : TAdoQuery;
    FDBGrid: TDBGrid;
    FAccountNumber: Int64;
    FNodeNotifyEvents : TNodeNotifyEvents;
    FDataSource : TDataSource;
    FDateEnd: TDate;
    FBlockStart: Int64;
    FDateStart: TDate;
    FBlockEnd: Int64;
    FNeedRefreshSQL : Boolean;
    function GetNode: TNode;
    procedure SetAccountNumber(const Value: Int64);
    procedure SetDBGrid(const Value: TDBGrid);
    procedure SetNode(const Value: TNode);
    Procedure OnGridDrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure SetAdoConnection(const Value: TADOConnection);
    function GetAdoConnection: TADOConnection;
    Procedure OnQryCalcFields(DataSet: TDataSet);
    Procedure OnNodeNewAccount(Sender : TObject);
    procedure SetBlockEnd(const Value: Int64);
    procedure SetBlockStart(const Value: Int64);
    procedure SetDateEnd(const Value: TDate);
    procedure SetDateStart(const Value: TDate);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); Override;
  published
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Property DBGrid : TDBGrid read FDBGrid write SetDBGrid;
    Property AccountNumber : Int64 read FAccountNumber write SetAccountNumber;
    Property BlockStart : Int64 read FBlockStart write SetBlockStart;
    Property BlockEnd : Int64 read FBlockEnd write SetBlockEnd;
    Procedure SetBlocks(bstart,bend : Int64);
    Property DateStart : TDate read FDateStart write SetDateStart;
    Property DateEnd : TDate read FDateEnd write SetDateEnd;
    Procedure SetDates(dStart,dEnd : TDate);
    Property Node : TNode read GetNode write SetNode;
    Procedure RefreshData;
    Property AdoConnection : TADOConnection read GetAdoConnection write SetAdoConnection;
    Procedure Disable;
    Procedure Enable;
    Procedure ShowModalDecoder(WalletKeys: TWalletKeys; AppParams : TAppParams);
  End;

  TBlockChainDBGrid = Class(TComponent)
  private
    FDisableds : Integer;
    FQrySQL : TAdoQuery;
    FDBGrid: TDBGrid;
    FNodeNotifyEvents : TNodeNotifyEvents;
    FDataSource : TDataSource;
    FDateEnd: TDate;
    FBlockStart: Int64;
    FDateStart: TDate;
    FBlockEnd: Int64;
    FAccountNumber: Int64;
    FNeedRefreshSQL : Boolean;
    function GetNode: TNode;
    procedure SetDBGrid(const Value: TDBGrid);
    procedure SetNode(const Value: TNode);
    Procedure OnGridDrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure SetAdoConnection(const Value: TADOConnection);
    function GetAdoConnection: TADOConnection;
    Procedure OnQryCalcFields(DataSet: TDataSet);
    Procedure OnNodeNewAccount(Sender : TObject);
    procedure SetBlockEnd(const Value: Int64);
    procedure SetBlockStart(const Value: Int64);
    procedure SetDateEnd(const Value: TDate);
    procedure SetDateStart(const Value: TDate);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); Override;
  published
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Property DBGrid : TDBGrid read FDBGrid write SetDBGrid;
    Property BlockStart : Int64 read FBlockStart write SetBlockStart;
    Property BlockEnd : Int64 read FBlockEnd write SetBlockEnd;
    Procedure SetBlocks(bstart,bend : Int64);
    Property DateStart : TDate read FDateStart write SetDateStart;
    Property DateEnd : TDate read FDateEnd write SetDateEnd;
    Procedure SetDates(dStart,dEnd : TDate);
    Property Node : TNode read GetNode write SetNode;
    Procedure RefreshData;
    Property AdoConnection : TADOConnection read GetAdoConnection write SetAdoConnection;
    Procedure Disable;
    Procedure Enable;
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

{ TOperationsGrid }

constructor TOperationsGrid.Create(AOwner: TComponent);
begin
  FAccountNumber := 0;
  FDrawGrid := Nil;
  FOperationsResume := TOperationsResumeList.Create;
  FNodeNotifyEvents := TNodeNotifyEvents.Create(Self);
  FNodeNotifyEvents.OnBlocksChanged := OnNodeNewAccount;
  FNodeNotifyEvents.OnOperationsChanged := OnNodeNewOperation;
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
  DrawGrid.ColWidths[1] := 50; // Block
  DrawGrid.ColWidths[2] := 60; // Account
  DrawGrid.ColWidths[3] := 150; // OpType
  DrawGrid.ColWidths[4] := 70; // Amount
  DrawGrid.ColWidths[5] := 60; // Operation Fee
  DrawGrid.ColWidths[6] := 70; // Balance
  DrawGrid.ColWidths[7] := 500; // Payload
  FDrawGrid.DefaultRowHeight := 18;
  FDrawGrid.Invalidate;
  DrawGrid.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
    {goRangeSelect, }goDrawFocusSelected, {goRowSizing, }goColSizing, {goRowMoving,}
    {goColMoving, goEditing, }goTabs, goRowSelect, {goAlwaysShowEditor,}
    goThumbTracking, goFixedColClick, goFixedRowClick, goFixedHotTrack];
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
  opr := CT_TOperationResume_NUL;
  Try
  if (ARow=0) then begin
    // Header
    case ACol of
      0 : s := 'Time';
      1 : s := 'Block';
      2 : s := 'Account';
      3 : s := 'Operation';
      4 : s := 'Amount';
      5 : s := 'Fee';
      6 : s := 'Balance';
      7 : s := 'Payload';
    else s:= '';
    end;
    DrawGrid.Canvas.TextRect(Rect,s,[tfCenter,tfVerticalCenter]);
  end else begin
    if (gdSelected in State) then
      If (gdFocused in State) then DrawGrid.Canvas.Brush.Color := clGradientActiveCaption
      else DrawGrid.Canvas.Brush.Color := clGradientInactiveCaption
    else DrawGrid.Canvas.Brush.Color := clWindow;
    DrawGrid.Canvas.FillRect(Rect);
    InflateRect(Rect,-2,-1);
    if (ARow<=FOperationsResume.Count) then begin
      opr := FOperationsResume.OperationResume[ARow-1];
      if ACol=0 then begin
        if opr.time=0 then s := '(Pending)'
        else s := DateTimeToStr(UnivDateTime2LocalDateTime(UnixToUnivDateTime(opr.time)));
        DrawGrid.Canvas.TextRect(Rect,s,[tfleft,tfVerticalCenter,tfSingleLine]);
      end else if ACol=1 then begin
        s := Inttostr(opr.Block);
        DrawGrid.Canvas.TextRect(Rect,s,[tfleft,tfVerticalCenter,tfSingleLine]);
      end else if ACol=2 then begin
        s := TAccountComp.AccountNumberToAccountTxtNumber(opr.AffectedAccount);
        DrawGrid.Canvas.TextRect(Rect,s,[tfleft,tfVerticalCenter,tfSingleLine]);
      end else if ACol=3 then begin
        s := opr.OperationTxt;
        DrawGrid.Canvas.TextRect(Rect,s,[tfleft,tfVerticalCenter,tfSingleLine]);
      end else if ACol=4 then begin
        s := TAccountComp.FormatMoney(opr.Amount);
        if opr.Amount>0 then DrawGrid.Canvas.Font.Color := ClGreen
        else if opr.Amount=0 then DrawGrid.Canvas.Font.Color := clGrayText
        else DrawGrid.Canvas.Font.Color := clRed;
        DrawGrid.Canvas.TextRect(Rect,s,[tfRight,tfVerticalCenter,tfSingleLine]);
      end else if ACol=5 then begin
        s := TAccountComp.FormatMoney(opr.Fee);
        if opr.Fee>0 then DrawGrid.Canvas.Font.Color := ClGreen
        else if opr.Fee=0 then DrawGrid.Canvas.Font.Color := clGrayText
        else DrawGrid.Canvas.Font.Color := clRed;
        DrawGrid.Canvas.TextRect(Rect,s,[tfRight,tfVerticalCenter,tfSingleLine]);
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
        DrawGrid.Canvas.TextRect(Rect,s,[tfRight,tfVerticalCenter,tfSingleLine]);
      end else if ACol=7 then begin
        s := opr.PrintablePayload;
        DrawGrid.Canvas.TextRect(Rect,s,[tfLeft,tfVerticalCenter,tfSingleLine]);
      end else begin
        s := '(???)';
        DrawGrid.Canvas.TextRect(Rect,s,[tfCenter,tfVerticalCenter,tfSingleLine]);
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
  UpdateAccountOperations;
end;

procedure TOperationsGrid.OnNodeNewOperation(Sender: TObject);
Var Op : TPCOperation;
  l : TList;
begin
  if AccountNumber<0 then UpdateAccountOperations
  else begin
    Op := TPCOperation(Sender);
    l := TList.Create;
    Try
      Op.AffectedAccounts(l);
      if l.IndexOf(TObject(Integer(AccountNumber)))>=0 then UpdateAccountOperations;
    Finally
      l.Free;
    End;
  end;
end;

procedure TOperationsGrid.SetAccountNumber(const Value: Int64);
begin
  if FAccountNumber=Value then exit;
  FAccountNumber := Value;
  if FAccountNumber>=0 then FPendingOperations := false;
  UpdateAccountOperations;
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
    FRM.Init(opr.Block,opr.time,opr.OperationTxt,opr.OriginalPayload,WalletKeys,AppParams);
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
begin
  FOperationsResume.Clear;
  Try
    if Not Assigned(Node) then exit;
    if FPendingOperations then begin
      for i := Node.Operations.Count - 1 downto 0 do begin
        Op := Node.Operations.OperationsHashTree.GetOperation(i);
        If TDBStorage.OperationToOperationResume(Op,Op.SenderAccount,OPR) then begin
          OPR.Block := Node.Operations.OperationBlock.block;
          OPR.Balance := Node.Operations.SafeBoxTransaction.Account(Op.SenderAccount).balance;
          FOperationsResume.Add(OPR);
        end;
      end;
    end else begin
      if AccountNumber<0 then begin
        list := TList.Create;
        try
          for i := 0 to Node.Operations.Count-1 do begin
            Op := Node.Operations.Operation[i];
            If TDBStorage.OperationToOperationResume(Op,Op.SenderAccount,OPR) then begin
              OPR.Block := Node.Operations.OperationBlock.block;
              OPR.Balance := Node.Operations.SafeBoxTransaction.Account(Op.SenderAccount).balance;
              FOperationsResume.Add(OPR);
            end;
          end;
        finally
          list.Free;
        end;
      end else begin
        list := TList.Create;
        Try
          Node.Operations.OperationsHashTree.GetOperationsAffectingAccount(AccountNumber,list);
          for i := list.Count - 1 downto 0 do begin
            Op := Node.Operations.OperationsHashTree.GetOperation(Integer(list[i]));
            If TDBStorage.OperationToOperationResume(Op,AccountNumber,OPR) then begin
              OPR.Block := Node.Operations.OperationBlock.block;
              OPR.Balance := Node.Operations.SafeBoxTransaction.Account(AccountNumber).balance;
              FOperationsResume.Add(OPR);
            end;
          end;
        Finally
          list.Free;
        End;
        if Node.Bank.Storage is TDBStorage then begin
          TDBStorage(Node.Bank.Storage).GetOperationsFromAccount(FOperationsResume,AccountNumber,0,200);
        end;
      end;
    end;
  Finally
    InitGrid;
  End;
end;

{ TOperationsDBGrid }

Type TAuxDBGrid = Class(TDBGrid);

constructor TOperationsDBGrid.Create(AOwner: TComponent);
Var i : Integer;
  fld : TField;
begin
  inherited;
  FNeedRefreshSQL := false;
  FDisableds := 0;
  FDBGrid := Nil;
  FQrySQL := TADOQuery.Create(Self);
  FQrySQL.OnCalcFields := OnQryCalcFields;
  FqrySQL.CursorLocation := clUseClient;
  FNodeNotifyEvents := TNodeNotifyEvents.Create(Self);
  FNodeNotifyEvents.OnBlocksChanged := OnNodeNewAccount;
  FDataSource := TDataSource.Create(Self);
  FDataSource.DataSet := FQrySQL;
  FAccountNumber := -1; // all
  FBlockStart := -1;
  FBlockEnd := -1;
  FDateStart := 0;
  FDateEnd := 0;
  fld := TIntegerField.Create(FQrySQL);
  fld.FieldName := CT_TblFld_Operations_block;
  fld.DataSet := FQrySQL;
  fld.Visible := false;
  //
  fld := TStringField.Create(FQrySQL);
  fld.FieldName := CT_TblFld_Operations_s_timestamp;
  fld.DataSet := FQrySQL;
  fld.Visible := false;
  //
  fld := TIntegerField.Create(FQrySQL);
  fld.FieldName := CT_TblFld_Operations_optype;
  fld.DataSet := FQrySQL;
  fld.Visible := false;
  fld := TIntegerField.Create(FQrySQL);
  fld.FieldName := CT_TblFld_Operations_optype_op;
  fld.DataSet := FQrySQL;
  fld.Visible := false;
  fld := TIntegerField.Create(FQrySQL);
  fld.FieldName := CT_TblFld_Operations_account;
  fld.DataSet := FQrySQL;
  fld.Visible := false;
  fld := TIntegerField.Create(FQrySQL);
  fld.FieldName := CT_TblFld_Operations_other_account;
  fld.DataSet := FQrySQL;
  fld.Visible := false;
  fld := TDateTimeField.Create(FQrySQL);
  fld.FieldName := 'datetime';
  fld.Calculated := true;
  fld.FieldKind := fkCalculated;
  fld.DataSet := FQrySQL;
  fld := TStringField.Create(FQrySQL);
  fld.FieldName := 'op_account';
  fld.Calculated := true;
  fld.FieldKind := fkCalculated;
  fld.DataSet := FQrySQL;
  TStringField(fld).Size := 100;
  fld := TStringField.Create(FQrySQL);
  fld.FieldName := 'operation';
  fld.Calculated := true;
  fld.FieldKind := fkCalculated;
  fld.DataSet := FQrySQL;
  TStringField(fld).Size := 100;
  fld := TStringField.Create(FQrySQL);
  fld.FieldName := 'payload_txt';
  fld.Calculated := true;
  fld.FieldKind := fkCalculated;
  fld.DataSet := FQrySQL;
  TStringField(fld).Size := 600;
  //
  fld := TStringField.Create(FQrySQL);
  fld.FieldName := CT_TblFld_Operations_s_amount;
  fld.DataSet := FQrySQL;
  fld := TLargeintField.Create(FQrySQL);
  fld.FieldName := 'amount';
  fld.Calculated := true;
  fld.FieldKind := fkCalculated;
  fld.DataSet := FQrySQL;

  fld := TStringField.Create(FQrySQL);
  fld.FieldName := CT_TblFld_Operations_s_fee;
  fld.DataSet := FQrySQL;
  fld := TLargeintField.Create(FQrySQL);
  fld.FieldName := 'fee';
  fld.Calculated := true;
  fld.FieldKind := fkCalculated;
  fld.DataSet := FQrySQL;

  fld := TStringField.Create(FQrySQL);
  fld.FieldName := CT_TblFld_Operations_s_balance;
  fld.DataSet := FQrySQL;
  fld := TLargeintField.Create(FQrySQL);
  fld.FieldName := 'balance';
  fld.Calculated := true;
  fld.FieldKind := fkCalculated;
  fld.DataSet := FQrySQL;

  fld := TStringField.Create(FQrySQL);
  fld.FieldName := CT_TblFld_Operations_rawpayload;
  fld.DataSet := FQrySQL;
  TStringField(fld).Size := 255;
end;

destructor TOperationsDBGrid.Destroy;
begin
  FNodeNotifyEvents.Free;
  FDataSource.Free;
  FQrySQL.Free;
  inherited;
end;

procedure TOperationsDBGrid.Disable;
begin
  inc(FDisableds);
end;

procedure TOperationsDBGrid.Enable;
begin
  dec(FDisableds);
  if FDisableds>0 then exit;
  FDisableds := 0;
  if FNeedRefreshSQL then RefreshData;
end;

function TOperationsDBGrid.GetAdoConnection: TADOConnection;
begin
  Result := FQrySQL.Connection;
end;

function TOperationsDBGrid.GetNode: TNode;
begin
  Result := FNodeNotifyEvents.Node;
end;

procedure TOperationsDBGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation=opRemove then begin
    if (AComponent=FDBGrid) then begin
      SetDBGrid(Nil);
    end;
  end;
end;

procedure TOperationsDBGrid.OnGridDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
Var c : TCanvas;
  s : String;
  r : TRect;
begin
  c := TDBGrid(Sender).Canvas;
  r := Rect;
  if (gdSelected in State) then
    If (gdFocused in State) then c.Brush.Color := clGradientActiveCaption
    else c.Brush.Color := clGradientInactiveCaption
  else c.Brush.Color := clWindow;
  c.FillRect(Rect);
  if SameText(Column.FieldName,'amount') Or
     SameText(Column.FieldName,'fee') Or
     SameText(Column.FieldName,'balance')
      then begin
    c.FillRect(Rect);
    if Column.Field.AsLargeInt>0 then c.Font.Color := ClGreen
    else if Column.Field.AsLargeInt=0 then c.Font.Color := clGrayText
    else c.Font.Color := clRed;
    s := TAccountComp.FormatMoney(Column.Field.AsLargeInt);
    c.TextRect(r,s,[tfRight,tfVerticalCenter,tfSingleLine]);
  end else begin
    TDBGrid(Sender).DefaultDrawColumnCell(Rect,DataCol,Column,State);
  end;

  if (gdFixed in State) and ([dgRowLines, dgColLines] * TDBGrid(Sender).Options =  [dgRowLines, dgColLines]) and
     not (gdPressed in State) then
  begin
    InflateRect(r, 1, 1);
    DrawEdge(c.Handle, r, BDR_RAISEDINNER, BF_BOTTOMRIGHT);
    DrawEdge(c.Handle, r, BDR_RAISEDINNER, BF_TOPLEFT);
  end;
  //
end;

procedure TOperationsDBGrid.OnNodeNewAccount(Sender: TObject);
begin
  RefreshData;
end;

procedure TOperationsDBGrid.OnQryCalcFields(DataSet: TDataSet);
Var fld : TField;
  raw : TRawBytes;
  s : AnsiString;
begin
  fld := DataSet.FieldByName('datetime');
  fld.AsDateTime :=  UnivDateTime2LocalDateTime(UnixToUnivDateTime( StrToIntDef( DataSet.FieldByName(CT_TblFld_Operations_s_timestamp).AsString, 0)));
  fld := DataSet.FieldByName('op_account');
  fld.AsString := TAccountComp.AccountNumberToAccountTxtNumber(DataSet.FieldByName(CT_TblFld_Operations_account).AsInteger);
  fld := DataSet.FieldByName('payload_txt');
  TDBStorage.DBStringFieldToRaw(DataSet.FieldByName(CT_TblFld_Operations_rawpayload),raw);
  If TDBStorage.DBPayloadToReadableText(raw,s) then begin
    fld.AsString := s;
  end else begin
    fld.AsString := TCrypto.ToHexaString(raw);
  end;
  //
  fld := DataSet.FieldByName('amount');
  TLargeintField(fld).Value := StrToInt64Def(Dataset.FieldByName(CT_TblFld_Operations_s_amount).AsString,0);
  fld := DataSet.FieldByName('balance');
  TLargeintField(fld).Value := StrToInt64Def(Dataset.FieldByName(CT_TblFld_Operations_s_balance).AsString,0);
  fld := DataSet.FieldByName('fee');
  TLargeintField(fld).Value := StrToInt64Def(Dataset.FieldByName(CT_TblFld_Operations_s_fee).AsString,0);

  fld := DataSet.FieldByName('operation');
          case dataset.FieldByName(CT_TblFld_Operations_optype).AsInteger of
            0 : fld.AsString := 'Blockchain reward';
            CT_Op_Transaction : begin
              if dataset.FieldByName(CT_TblFld_Operations_optype_op).AsInteger=0 then begin
                fld.AsString := 'Transaction Sent to '+TAccountComp.AccountNumberToAccountTxtNumber(dataset.FieldByName(CT_TblFld_Operations_other_account).AsLargeInt);
              end else begin
                fld.AsString := 'Transaction Received from '+TAccountComp.AccountNumberToAccountTxtNumber(dataset.FieldByName(CT_TblFld_Operations_other_account).AsLargeInt);
              end;
            end;
            CT_Op_Changekey : Begin
              fld.AsString := 'Change Key';
            End;
            CT_Op_Recover : begin
              fld.AsString := 'Recover founds';
            end;
          else
            fld.AsString := 'Unknown OpType:'+Inttostr(dataset.FieldByName(CT_TblFld_Operations_optype).AsInteger);
          end;
end;

procedure TOperationsDBGrid.RefreshData;
Var sql : AnsiString;
begin
  if FDisableds>0 then begin
    FNeedRefreshSQL := true;
    exit;
  end;
  FNeedRefreshSQL := false;
  sql := TDBStorage.GetOperationsFromAccountSQL(AccountNumber,BlockStart,BlockEnd,DateStart,DateEnd, 0,50000);
  If FQrySQL.Connection=Nil then exit;
  if Node=Nil then exit;
  if FDBGrid=Nil then exit;

  FQrySQL.DisableControls;
  try
    FQrySQL.Close;
    FQrySQL.SQL.Text := sql;
    FQrySQL.Open;
  finally
    FQrySQL.EnableControls;
  end;
end;

procedure TOperationsDBGrid.SetAccountNumber(const Value: Int64);
begin
  if FAccountNumber=Value then exit;
  FAccountNumber := Value;
  RefreshData;
end;

procedure TOperationsDBGrid.SetAdoConnection(const Value: TADOConnection);
begin
  FQrySQL.Connection := Value;
  RefreshData;
end;

procedure TOperationsDBGrid.SetBlockEnd(const Value: Int64);
begin
  if FBlockEnd=Value then exit;
  FBlockEnd := Value;
  RefreshData;
end;

procedure TOperationsDBGrid.SetBlocks(bstart, bend: Int64);
begin
  FBlockStart := bstart;
  FBlockEnd := bend;
  RefreshData;
end;

procedure TOperationsDBGrid.SetBlockStart(const Value: Int64);
begin
  if FBlockStart=Value then exit;
  FBlockStart := Value;
  RefreshData;
end;

procedure TOperationsDBGrid.SetDateEnd(const Value: TDate);
Var d,h,m,y : word;
begin
  if FDateEnd=Value then exit;
  decodedate(value,y,m,d);
  FDateEnd := EncodeDate(y,m,d);
  if FDateStart>=FDateEnd then FDateStart := FDateEnd;
  RefreshData;
end;

procedure TOperationsDBGrid.SetDates(dStart, dEnd: TDate);
begin
  Disable;
  try
    DateStart := dStart;
    DateEnd := dEnd;
  finally
    Enable;
  end;
end;

procedure TOperationsDBGrid.SetDateStart(const Value: TDate);
Var d,h,m,y : word;
begin
  if FDateStart=Value then exit;
  decodedate(value,y,m,d);
  FDateStart := EncodeDate(y,m,d);
  if FDateStart>=FDateEnd then FDateEnd := FDateStart;
  RefreshData;
end;

procedure TOperationsDBGrid.SetDBGrid(const Value: TDBGrid);
  Procedure AddColumn(fieldname,displayname : String; colwidth : integer);
  Var c : TColumn;
  begin
    c := DBgrid.Columns.Add;
    c.FieldName := fieldname;
    c.Title.Caption := displayname;
    c.Width := colwidth;
    c.Title.Font.Style := [fsBold];
    c.Title.Alignment := taCenter;
  end;
begin
  if FDBGrid=Value then exit;
  if Assigned(FDBGrid) then FDBGrid.DataSource := Nil;
  FDBGrid := Value;
  if Assigned(Value) then begin
    Value.FreeNotification(self);
    Value.DataSource := FDataSource;
    Value.ReadOnly := true;
    FDBGrid.OnDrawColumnCell := OnGridDrawColumnCell;
    FDBGrid.DefaultDrawing := false;
    FDBGrid.Columns.Clear;
    AddColumn(CT_TblFld_Operations_block,'Block',50);
    AddColumn('datetime','Date/Time',120);
    AddColumn('op_account','Account',70);
    AddColumn('operation','Operation',150);
    AddColumn('amount','Amount',80);
    AddColumn('fee','Fee',60);
    AddColumn('balance','Balance',80);
    AddColumn('payload_txt','Payload',280);
    RefreshData;
  end;
end;

procedure TOperationsDBGrid.SetNode(const Value: TNode);
begin
  FNodeNotifyEvents.Node:= value;
  RefreshData;
end;

procedure TOperationsDBGrid.ShowModalDecoder(WalletKeys: TWalletKeys; AppParams: TAppParams);
Var FRM : TFRMPayloadDecoder;
  raw : TRawBytes;
begin
  if Not Assigned(FDBGrid) Or Not Assigned(FQrySQL) then exit;
  If FQrySQL.Eof then exit;
  FRM := TFRMPayloadDecoder.Create(FDBGrid.Owner);
  try
    TDBStorage.DBStringFieldToRaw(FQrySQL.FieldByName(CT_TblFld_Operations_rawpayload),raw);
    FRM.Init(FQrySQL.FieldByName(CT_TblFld_Operations_block).AsInteger,
      StrToIntDef( FQrySQL.FieldByName(CT_TblFld_Operations_s_timestamp).AsString,0),
      FQrySQL.FieldByName('operation').AsString,
      raw,WalletKeys,AppParams);
    FRM.ShowModal;
  finally
    FRM.Free;
  end;
end;

{ TBlockChainDBGrid }

constructor TBlockChainDBGrid.Create(AOwner: TComponent);
Var fld : TField;
begin
  inherited;
  FNeedRefreshSQL := False;
  FBlockStart := -1;
  FBlockEnd := -1;
  FDateEnd := 0;
  FDateStart := 0;
  FDisableds := 0;
  FQrySQL := TADOQuery.Create(Self);
  FQrySQL.OnCalcFields := OnQryCalcFields;
  FqrySQL.CursorLocation := clUseClient;
  FDBGrid := Nil;
  FNodeNotifyEvents := TNodeNotifyEvents.Create(Self);
  FNodeNotifyEvents.OnBlocksChanged := OnNodeNewAccount;
  FDataSource := TDataSource.Create(Self);
  FDataSource.DataSet := FQrySQL;
  fld := TIntegerField.Create(FQrySQL);
  fld.FieldName := CT_TblFld_BlockChain_block;
  fld.DataSet := FQrySQL;
  fld.Visible := true;
  fld := TStringField.Create(FQrySQL);
  fld.FieldName := CT_TblFld_BlockChain_s_timestamp;
  fld.DataSet := FQrySQL;
  fld.Visible := false;
  fld := TStringField.Create(FQrySQL);
  fld.FieldName := CT_TblFld_BlockChain_s_reward;
  fld.DataSet := FQrySQL;
  fld.Visible := true;
  fld := TStringField.Create(FQrySQL);
  fld.FieldName := CT_TblFld_BlockChain_s_fee;
  fld.DataSet := FQrySQL;
  fld.Visible := true;
  fld := TStringField.Create(FQrySQL);
  fld.FieldName := CT_TblFld_BlockChain_s_compact_target;
  fld.DataSet := FQrySQL;
  fld.Visible := true;
  fld := TIntegerField.Create(FQrySQL);
  fld.FieldName := CT_TblFld_BlockChain_operations_count;
  fld.DataSet := FQrySQL;
  fld.Visible := false;
  fld := TDateTimeField.Create(FQrySQL);
  fld.FieldName := 'datetime';
  fld.Calculated := true;
  fld.FieldKind := fkCalculated;
  fld.DataSet := FQrySQL;
  fld := TStringField.Create(FQrySQL);
  fld.FieldName := CT_TblFld_BlockChain_proof_of_work;
  fld.DataSet := FQrySQL;
  fld.Visible := false;
  TStringField(fld).Size := 64;
  fld := TStringField.Create(FQrySQL);
  fld.FieldName := CT_TblFld_BlockChain_safe_box_hash;
  fld.DataSet := FQrySQL;
  fld.Visible := false;
  TStringField(fld).Size := 64;
  fld := TStringField.Create(FQrySQL);
  fld.FieldName := 'target';
  fld.Calculated := true;
  fld.FieldKind := fkCalculated;
  fld.DataSet := FQrySQL;
  TStringField(fld).Size := 8;

  fld := TLargeintField.Create(FQrySQL);
  fld.FieldName := 'reward';
  fld.Calculated := true;
  fld.FieldKind := fkCalculated;
  fld.DataSet := FQrySQL;

  fld := TLargeintField.Create(FQrySQL);
  fld.FieldName := 'fee';
  fld.Calculated := true;
  fld.FieldKind := fkCalculated;
  fld.DataSet := FQrySQL;

  fld := TStringField.Create(FQrySQL);
  fld.FieldName := CT_TblFld_BlockChain_rawpayload;
  fld.DataSet := FQrySQL;
  TStringField(fld).Size := 255;

  fld := TStringField.Create(FQrySQL);
  fld.FieldName := 'payload_decoded';
  fld.Calculated := true;
  fld.FieldKind := fkCalculated;
  fld.DataSet := FQrySQL;
  TStringField(fld).Size := 600;
end;

destructor TBlockChainDBGrid.Destroy;
begin
  SetDBGrid(Nil);
  FNodeNotifyEvents.Free;
  FDataSource.Free;
  FQrySQL.Free;
  inherited;
end;

procedure TBlockChainDBGrid.Disable;
begin
  Inc(FDisableds);
end;

procedure TBlockChainDBGrid.Enable;
begin
  if FDisableds<0 then exit;
  Dec(FDisableds);
  if FNeedRefreshSQL then RefreshData;
end;

function TBlockChainDBGrid.GetAdoConnection: TADOConnection;
begin
  Result := FQrySQL.Connection;
end;

function TBlockChainDBGrid.GetNode: TNode;
begin
  Result := FNodeNotifyEvents.Node;
end;

procedure TBlockChainDBGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation=OpRemove then begin
    if AComponent=FDBGrid then FDBGrid:=Nil;
  end;
end;

procedure TBlockChainDBGrid.OnGridDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
Var c : TCanvas;
  s : String;
  r : TRect;
begin
  c := TDBGrid(Sender).Canvas;
  r := Rect;
  if (gdSelected in State) then
    If (gdFocused in State) then c.Brush.Color := clGradientActiveCaption
    else c.Brush.Color := clGradientInactiveCaption
  else c.Brush.Color := clWindow;
  c.FillRect(Rect);
  if SameText(Column.FieldName,'reward') Or
     SameText(Column.FieldName,'fee')
      then begin
    c.FillRect(Rect);
    if Column.Field.AsLargeInt>0 then c.Font.Color := ClGreen
    else if Column.Field.AsLargeInt=0 then c.Font.Color := clGrayText
    else c.Font.Color := clRed;
    s := TAccountComp.FormatMoney(Column.Field.AsLargeInt);
    c.TextRect(r,s,[tfRight,tfVerticalCenter,tfSingleLine]);
  end else begin
    TDBGrid(Sender).DefaultDrawColumnCell(Rect,DataCol,Column,State);
  end;

  if (gdFixed in State) and ([dgRowLines, dgColLines] * TDBGrid(Sender).Options =  [dgRowLines, dgColLines]) and
     not (gdPressed in State) then
  begin
    InflateRect(r, 1, 1);
    DrawEdge(c.Handle, r, BDR_RAISEDINNER, BF_BOTTOMRIGHT);
    DrawEdge(c.Handle, r, BDR_RAISEDINNER, BF_TOPLEFT);
  end;
  //
end;

procedure TBlockChainDBGrid.OnNodeNewAccount(Sender: TObject);
begin
  RefreshData;
end;

procedure TBlockChainDBGrid.OnQryCalcFields(DataSet: TDataSet);
Var fld : TField;
  raw : TRawBytes;
  s : AnsiString;
begin
  fld := DataSet.FieldByName('datetime');
  fld.AsDateTime :=  UnivDateTime2LocalDateTime(UnixToUnivDateTime(StrToIntDef( DataSet.FieldByName(CT_TblFld_BlockChain_s_timestamp).AsString,0)));
  fld := DataSet.FieldByName('target');
  fld.AsString :=  IntToHex(StrToIntDef( DataSet.FieldByName(CT_TblFld_BlockChain_s_compact_target).AsString,0),8);
  DataSet.FieldByName('reward').AsLargeInt := StrToIntDef(DataSet.FieldByName(CT_TblFld_BlockChain_s_reward).AsString,0);
  DataSet.FieldByName('fee').AsLargeInt := StrToIntDef(DataSet.FieldByName(CT_TblFld_BlockChain_s_fee).AsString,0);
  fld := DataSet.FieldByName('payload_decoded');
  TDBStorage.DBStringFieldToRaw(DataSet.FieldByName(CT_TblFld_BlockChain_rawpayload),raw);
  If TDBStorage.DBPayloadToReadableText(raw,s) then begin
    fld.AsString := s;
  end else begin
    fld.AsString := TCrypto.ToHexaString(raw);
  end;
end;

procedure TBlockChainDBGrid.RefreshData;
Var sql : AnsiString;
begin
  if FDisableds>0 then begin
    FNeedRefreshSQL := true;
    exit;
  end;
  FNeedRefreshSQL := false;
  sql := TDBStorage.GetBlockChainSQL(BlockStart,BlockEnd,DateStart,DateEnd, 0,300);
  If FQrySQL.Connection=Nil then exit;
  if Node=Nil then exit;
  if FDBGrid=Nil then exit;

  FQrySQL.DisableControls;
  try
    FQrySQL.Close;
    FQrySQL.SQL.Text := sql;
    FQrySQL.Open;
  finally
    FQrySQL.EnableControls;
  end;
end;

procedure TBlockChainDBGrid.SetAdoConnection(const Value: TADOConnection);
begin
  FQrySQL.Connection := Value;
  RefreshData;
end;

procedure TBlockChainDBGrid.SetBlockEnd(const Value: Int64);
begin
  if FBlockEnd=Value then exit;
  FBlockEnd := Value;
  RefreshData;
end;

procedure TBlockChainDBGrid.SetBlocks(bstart, bend: Int64);
begin
  disable;
  Try
    BlockStart := bstart;
    BlockEnd := bEnd;
  Finally
    Enable;
  End;
end;

procedure TBlockChainDBGrid.SetBlockStart(const Value: Int64);
begin
  if FBlockStart=Value then exit;
  FBlockStart:=Value;
  RefreshData;
end;

procedure TBlockChainDBGrid.SetDateEnd(const Value: TDate);
begin
  if FDateEnd=Value then exit;
  FDateEnd := Value;
  RefreshData;
end;

procedure TBlockChainDBGrid.SetDates(dStart, dEnd: TDate);
begin
  Disable;
  Try
    DateStart := dStart;
    DateEnd := dEnd;
  Finally
    Enable;
  End;
end;

procedure TBlockChainDBGrid.SetDateStart(const Value: TDate);
begin
  if FDateStart=Value then exit;
  FDateStart := Value;
  RefreshData;
end;

procedure TBlockChainDBGrid.SetDBGrid(const Value: TDBGrid);
  Procedure AddColumn(fieldname,displayname : String; colwidth : integer);
  Var c : TColumn;
  begin
    c := DBgrid.Columns.Add;
    c.FieldName := fieldname;
    c.Title.Caption := displayname;
    c.Width := colwidth;
    c.Title.Font.Style := [fsBold];
    c.Title.Alignment := taCenter;
  end;
begin
  if FDBGrid=Value then exit;
  if Assigned(FDBGrid) then FDBGrid.DataSource := Nil;
  FDBGrid := Value;
  if Assigned(Value) then begin
    Value.FreeNotification(self);
    Value.DataSource := FDataSource;
    Value.ReadOnly := true;
    FDBGrid.OnDrawColumnCell := OnGridDrawColumnCell;
    FDBGrid.DefaultDrawing := false;
    FDBGrid.Columns.Clear;
    AddColumn(CT_TblFld_BlockChain_block,'Block',60);
    AddColumn('datetime','Date/Time',120);
    AddColumn(CT_TblFld_BlockChain_operations_count,'Ops.',30);
    AddColumn('reward','Reward',80);
    AddColumn('fee','Fee',60);
    AddColumn('target','Target',60);
    AddColumn('payload_decoded','Miner Payload',150);
    AddColumn(CT_TblFld_BlockChain_proof_of_work,'Proof of Work',400);
    AddColumn(CT_TblFld_BlockChain_safe_box_hash,'Safe Box Hash',400);
    RefreshData;
  end;
end;

procedure TBlockChainDBGrid.SetNode(const Value: TNode);
begin
  FNodeNotifyEvents.Node := Value;
  RefreshData;
end;

end.
