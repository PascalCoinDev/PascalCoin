unit UGridUtils;

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  If you like it, consider a donation using Bitcoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

{$I ../config.inc}

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Classes, Grids, UNode, UAccounts, UBlockChain, UAppParams, UThread, UPCDataTypes,
  UWallet, UCrypto, UPoolMining, URPC, UBaseTypes, UPCOrderedLists, USettings,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF};

Type
  // TAccountsGrid implements a visual integration of TDrawGrid
  // to show accounts information
  TAccountColumnType = (act_account_number,act_account_key,act_balance,act_updated,act_n_operation,act_updated_state,act_name,act_type,act_saleprice);
  TAccountColumn = Record
    ColumnType : TAccountColumnType;
    width : Integer;
  End;
  TAccountColumnArray = Array of TAccountColumn;

  TAccountsGrid = Class;

  TAccountsGridFilter = Record
    MinBalance,
    MaxBalance : Int64;
    OrderedAccountsKeyList : TOrderedAccountKeysList;
    indexAccountsKeyList : Integer;
  end;

  TAccountsGridUpdateThread = Class(TPCThread)
    FAccountsGrid : TAccountsGrid;
    FAccountsGridFilter : TAccountsGridFilter;
    FBalance : Int64;
    FIsProcessing : Boolean;
    FProcessedList : TOrderedCardinalList;
  protected
    procedure SynchronizedOnTerminated;
    procedure BCExecute; override;
  public
    constructor Create(AAccountsGrid : TAccountsGrid; AAccountsGridFilter : TAccountsGridFilter);
    destructor Destroy; override;
    property IsProcessing : Boolean read FisProcessing;
  End;

  TAccountsGridDatasource = (acds_Node, acds_InternalList, acds_NodeFiltered);

  TAccountsGrid = Class(TComponent)
  private
    FAccountsBalance : Int64;
    FAccountsList : TOrderedCardinalList;
    FColumns : TAccountColumnArray;
    FDrawGrid : TDrawGrid;
    FNodeNotifyEvents : TNodeNotifyEvents;
    FOnUpdated: TNotifyEvent;
    FAllowMultiSelect: Boolean;
    FAccountsGridUpdateThread : TAccountsGridUpdateThread;
    FAccountsGridFilter: TAccountsGridFilter;
    FOnAccountsGridUpdatedData: TNotifyEvent;
    FAccountsGridDatasource: TAccountsGridDatasource;
    //
    FBufferLastAccountNumber : Integer;
    FBufferLastAccount : TAccount;
    FBufferNodeAccountsCount : Integer;
    FBufferNodeBlocksCount : Integer;
    //
    procedure SetDrawGrid(const Value: TDrawGrid);
    Procedure InitGrid;
    Procedure InitGridRowCount;
    Procedure OnNodeNewOperation(Sender : TObject);
    procedure OnGridDrawCell(Sender: TObject; ACol, ARow: Longint; Rect: TRect; State: TGridDrawState);
    procedure SetNode(const Value: TNode);
    function GetNode: TNode;
    procedure SetAllowMultiSelect(const Value: Boolean);
    procedure TerminateAccountGridUpdateThread(AWaitUntilTerminated : Boolean);
    procedure SetAccountsGridFilter(const Value: TAccountsGridFilter);
    function GetAccountsCount: Integer;
    procedure SetAccountsGridDatasource(const Value: TAccountsGridDatasource);
    procedure UpdateAccountsBalance;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); Override;
    procedure BufferGetAccount(AAccountNumber : Integer; var AAccount : TAccount; var ANodeBlocksCount, ANodeAccountsCount : Integer);
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
    Property AccountsBalance : Int64 read FAccountsBalance;
    Property AccountsCount : Integer read GetAccountsCount;
    Function MoveRowToAccount(nAccount : Cardinal) : Boolean;
    Property OnUpdated : TNotifyEvent read FOnUpdated write FOnUpdated;
    Property AllowMultiSelect : Boolean read FAllowMultiSelect write SetAllowMultiSelect;
    Function SelectedAccounts(accounts : TOrderedCardinalList) : Integer;
    property AccountsGridFilter : TAccountsGridFilter read FAccountsGridFilter write SetAccountsGridFilter;
    procedure UpdateData;
    function IsUpdatingData : Boolean;
    property OnAccountsGridUpdatedData : TNotifyEvent read FOnAccountsGridUpdatedData write FOnAccountsGridUpdatedData;
    property AccountsGridDatasource : TAccountsGridDatasource read FAccountsGridDatasource write SetAccountsGridDatasource;
    function GetColumns : TAccountColumnArray;
    procedure SetColumns(const AColumns : TAccountColumnArray);
  End;

  TOperationsGrid = Class;

  TOperationsGridUpdateThread = Class(TPCThread)
    FOperationsGrid : TOperationsGrid;
    procedure DoUpdateOperationsGrid(const ANode : TNode; const AWalleTKeys : TWalletKeys; const APasswords : TList<String>; var AList : TList<TOperationResume>);
  protected
    procedure BCExecute; override;
  public
    constructor Create(AOperationsGrid : TOperationsGrid);
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
    FOperationsGridUpdateThread : TOperationsGridUpdateThread;
    FWalletKeys: TWalletKeys;
    FPasswords: TList<String>;
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
    property WalletKeys : TWalletKeys read FWalletKeys write FWalletKeys;
    property Passwords : TList<String> read FPasswords;
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
    HashRateTargetHs : Double;
    HashRateHs : Double;
    HashRateTargetKhs : Int64;
    HashRateKhs : Int64;
    MinerPayload : TRawBytes;
    PoW : TRawBytes;
    SafeBoxHash : TRawBytes;
    TimeAverage200 : Real;
    TimeAverage150 : Real;
    TimeAverage100 : Real;
    TimeAverage75 : Real;
    TimeAverage50 : Real;
    TimeAverage25 : Real;
    TimeAverage10 : Real;
    TimeAverage5 : Real;
  End;

  TBlockChainGrid = Class;

  { TBlockChainGridUpdateThread }

  TBlockChainGridUpdateThread = Class(TPCThread)
    FBlockChainGrid : TBlockChainGrid;
    FBlockStart, FBlockEnd : Int64;
    procedure DoUpdateBlockChainGrid(ANode : TNode; var AList : TList<TBlockChainData>; ABlockStart, ABlockEnd : Int64);
  protected
    FGridUpdateCount: Integer;
    procedure BCExecute; override;
    procedure RefreshGrid;
  public
    constructor Create(ABlockChainGrid : TBlockChainGrid);
  End;


  { TBlockChainGrid }

  TBlockChainGrid = Class(TComponent)
  private
    FBlockChainDataList : TList<TBlockChainData>;
    FBlockStart: Int64;
    FHashRateAs: TShowHashRateAs;
    FMaxBlocks: Integer;
    FBlockEnd: Int64;
    FDrawGrid: TDrawGrid;
    FNodeNotifyEvents : TNodeNotifyEvents;
    FHashRateAverageBlocksCount: Integer;
    FShowTimeAverageColumns: Boolean;
    FBlockChainGridUpdateThread : TBlockChainGridUpdateThread;
    Procedure OnNodeNewAccount(Sender : TObject);
    Procedure InitGrid;
    procedure OnGridDrawCell(Sender: TObject; ACol, ARow: Longint; Rect: TRect; State: TGridDrawState);
    function GetNode: TNode;
    procedure SetBlockEnd(const Value: Int64);
    procedure SetBlockStart(const Value: Int64);
    procedure SetDrawGrid(const Value: TDrawGrid);
    procedure SetHashRateAs(AValue: TShowHashRateAs);
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
    Property HashRateAs : TShowHashRateAs read FHashRateAs write SetHashRateAs;
  End;

Const
  CT_TBlockChainData_NUL : TBlockChainData = (Block:0;Timestamp:0;BlockProtocolVersion:0;BlockProtocolAvailable:0;OperationsCount:-1;Volume:-1;Reward:0;Fee:0;Target:0;HashRateTargetHs:0;HashRateHs:0;HashRateTargetKhs:0;HashRateKhs:0;MinerPayload:Nil;PoW:Nil;SafeBoxHash:Nil;
    TimeAverage200:0;TimeAverage150:0;TimeAverage100:0;TimeAverage75:0;TimeAverage50:0;TimeAverage25:0;TimeAverage10:0;TimeAverage5:0);
  CT_TAccountsGridFilter_NUL : TAccountsGridFilter = (MinBalance:-1;MaxBalance:-1;OrderedAccountsKeyList:Nil;indexAccountsKeyList:-1);

implementation

uses
  Graphics, SysUtils, UTime, UOpTransaction, UConst,
  UEPasa, UEPasaDecoder,
  UFRMPayloadDecoder, ULog;

{ TAccountsGridUpdateThread }

procedure TAccountsGridUpdateThread.BCExecute;
Var
  LAccountsNumbersList : TAccountsNumbersList;
  i,j, j_min : Integer;
  c  : Cardinal;
  LApplyfilter : Boolean;
  LAccount : TAccount;
  LNode : TNode;
  LAccountsList : TList<Int64>;
begin
  LApplyfilter := ((FAccountsGridFilter.MinBalance>0) Or ((FAccountsGridFilter.MaxBalance>=0) And (FAccountsGridFilter.MaxBalance<CT_MaxWalletAmount)));
  FBalance := 0;
  LNode := FAccountsGrid.Node;
  try
    if (Assigned(FAccountsGridFilter.OrderedAccountsKeyList)) then begin
      if (FAccountsGridFilter.indexAccountsKeyList<0) then i := 0
      else i := FAccountsGridFilter.indexAccountsKeyList;

      while (Not Terminated) and (i<FAccountsGridFilter.OrderedAccountsKeyList.Count)
        and ((FAccountsGridFilter.indexAccountsKeyList<0) or (FAccountsGridFilter.indexAccountsKeyList=i)) do begin

        j_min := 0;

          while (j_min>=0) do begin

          LNode.bank.SafeBox.StartThreadSafe;
          FAccountsGridFilter.OrderedAccountsKeyList.Lock; // Protection v4
          Try
            LAccountsNumbersList := FAccountsGridFilter.OrderedAccountsKeyList.AccountKeyList[i];
            if Assigned(LAccountsNumbersList) then begin

              LAccountsList := TList<Int64>.Create;
              Try
                LAccountsNumbersList.FillList(j_min,50000,LAccountsList);
                for j := 0 to LAccountsList.Count - 1 do begin
                  LAccount := LNode.Bank.SafeBox.Account(LAccountsList[j]);

                  if LApplyfilter then begin
                    if (LAccount.balance>=FAccountsGridFilter.MinBalance) And ((FAccountsGridFilter.MaxBalance<0) Or (LAccount.balance<=FAccountsGridFilter.MaxBalance)) then begin
                      FProcessedList.Add(LAccount.account);
                      FBalance := FBalance + LAccount.balance;
                    end;
                  end else begin
                    FProcessedList.Add(LAccount.account);
                    FBalance := FBalance + LAccount.balance;
                  end;
                  if Terminated then Exit;
                end;
                if LAccountsList.Count>0 then inc(j_min,LAccountsList.Count)
                else break;

              Finally
                LAccountsList.Free;
              End;

            end;
          finally
            FAccountsGridFilter.OrderedAccountsKeyList.Unlock;
            LNode.Bank.SafeBox.EndThreadSave;
          end;

        end;
        inc(i);
      end;
    end else begin
        c := 0;
        while (c<LNode.Bank.SafeBox.AccountsCount) and (Not Terminated) do begin
          LAccount := LNode.Bank.SafeBox.Account(c);
          if (LAccount.balance>=FAccountsGridFilter.MinBalance) And ((FAccountsGridFilter.MaxBalance<0) Or (LAccount.balance<=FAccountsGridFilter.MaxBalance)) then begin
            FProcessedList.Add(LAccount.account);
            FBalance := FBalance + LAccount.balance;
          end;
          inc(c);
        end;
      end;
  Finally
    if Not Terminated then begin
      Synchronize(SynchronizedOnTerminated);
    end;
    FisProcessing := False;
  End;
end;

constructor TAccountsGridUpdateThread.Create(AAccountsGrid: TAccountsGrid; AAccountsGridFilter: TAccountsGridFilter);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FAccountsGrid := AAccountsGrid;
  FAccountsGridFilter := AAccountsGridFilter;
  FisProcessing := True;
  FProcessedList := TOrderedCardinalList.Create;
  Suspended := False;
end;

destructor TAccountsGridUpdateThread.Destroy;
begin
  FreeAndNil(FProcessedList);
  inherited;
end;

procedure TAccountsGridUpdateThread.SynchronizedOnTerminated;
var LacclTemp : TOrderedCardinalList;
begin
  if Not Terminated then begin
    FAccountsGrid.FAccountsBalance := FBalance;
    LacclTemp := FAccountsGrid.LockAccountsList;
    try
      LacclTemp.CopyFrom( FProcessedList );
    finally
      FAccountsGrid.UnlockAccountsList;
    end;
    FisProcessing := False;
    if Assigned(FAccountsGrid.FOnAccountsGridUpdatedData) then  FAccountsGrid.FOnAccountsGridUpdatedData(FAccountsGrid);
  end;
end;

{ TAccountsGrid }

Const CT_ColumnHeader : Array[TAccountColumnType] Of String =
  ('Account No','Key','Balance','Updated','N Op.','S','Name','Type','Price');

function TAccountsGrid.AccountNumber(GridRow: Integer): Int64;
begin
  if GridRow<1 then Result := -1
  else if (FAccountsGridDatasource=acds_Node) then begin
    if Assigned(Node) then begin
      Result := GridRow-1;
    end else Result := -1;
  end else if GridRow<=FAccountsList.Count then begin
    Result := (FAccountsList.Get(GridRow-1));
  end else Result := -1;
end;

procedure TAccountsGrid.BufferGetAccount(AAccountNumber: Integer;
  var AAccount: TAccount; var ANodeBlocksCount, ANodeAccountsCount: Integer);
begin
  if FBufferLastAccountNumber<>AAccountNumber then begin
    FBufferNodeAccountsCount := Node.Bank.AccountsCount;
    if (AAccountNumber>=FBufferNodeAccountsCount) then FBufferLastAccount := CT_Account_NUL
    else FBufferLastAccount := Node.GetMempoolAccount(AAccountNumber);
    FBufferNodeBlocksCount := Node.Bank.BlocksCount;
    FBufferLastAccountNumber := AAccountNumber;
  end;
  AAccount := FBufferLastAccount;
  ANodeBlocksCount := FBufferNodeBlocksCount;
  ANodeAccountsCount := FBufferNodeAccountsCount;
end;

constructor TAccountsGrid.Create(AOwner: TComponent);
Var i : Integer;
begin
  inherited;
  FAllowMultiSelect := false;
  FOnUpdated := Nil;
  FAccountsBalance := 0;
  FAccountsList := TOrderedCardinalList.Create;
  FDrawGrid := Nil;
  SetLength(FColumns,7);
  FColumns[0].ColumnType := act_account_number;
  FColumns[0].width := 75;
  FColumns[1].ColumnType := act_name;
  FColumns[1].width := 80;
  FColumns[2].ColumnType := act_balance;
  FColumns[2].width := 80;
  FColumns[3].ColumnType := act_n_operation;
  FColumns[3].width := 35;
  FColumns[4].ColumnType := act_type;
  FColumns[4].width := 35;
  FColumns[5].ColumnType := act_saleprice;
  FColumns[5].width := 45;
  FColumns[6].ColumnType := act_updated_state;
  FColumns[6].width := 20;
  FNodeNotifyEvents := TNodeNotifyEvents.Create(Self);
  FNodeNotifyEvents.OnOperationsChanged := OnNodeNewOperation;
  FAccountsGridUpdateThread := Nil;
  FOnAccountsGridUpdatedData := Nil;
  FAccountsGridFilter := CT_TAccountsGridFilter_NUL;
  FAccountsGridDatasource := acds_Node;
  FBufferLastAccountNumber := -1;
end;

destructor TAccountsGrid.Destroy;
begin
  TerminateAccountGridUpdateThread(True);
  FNodeNotifyEvents.Free;
  FAccountsList.Free;
  inherited;
end;

function TAccountsGrid.GetAccountsCount: Integer;
begin
  if Not Assigned(Node) then Exit(0);

  case FAccountsGridDatasource of
    acds_Node: Result := Node.Bank.AccountsCount;
  else
    Result := FAccountsList.Count;
  end;
end;

function TAccountsGrid.GetColumns: TAccountColumnArray;
begin
  Result := FColumns;
end;

function TAccountsGrid.GetNode: TNode;
begin
  Result := FNodeNotifyEvents.Node;
end;

procedure TAccountsGrid.InitGrid;
Var i : Integer;
begin
  if Not assigned(DrawGrid) then exit;
  InitGridRowCount;
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
  FBufferLastAccountNumber := -1;
  FDrawGrid.Invalidate;
  if Assigned(FOnUpdated) then FOnUpdated(Self);
end;

procedure TAccountsGrid.InitGridRowCount;
var LRowCount : Integer;
begin
  if Not assigned(DrawGrid) then exit;
  if FAccountsGridDatasource=acds_Node then begin
    if Assigned(Node) then begin
      if Node.Bank.AccountsCount<1 then LRowCount := 2
      else LRowCount := Node.Bank.AccountsCount+1;
    end else LRowCount := 2;
  end else begin
    if FAccountsList.Count<1 then LRowCount := 2
    else LRowCount := FAccountsList.Count+1;
  end;
  if DrawGrid.RowCount<>LRowCount then DrawGrid.RowCount:=LRowCount;
  FBufferLastAccountNumber := -1;
  FDrawGrid.Invalidate;
end;

function TAccountsGrid.IsUpdatingData: Boolean;
begin
  if Assigned(FAccountsGridUpdateThread) then Result := FAccountsGridUpdateThread.IsProcessing
  else Result := False;
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
  if FAccountsGridDatasource=acds_Node then begin
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

procedure TAccountsGrid.OnGridDrawCell(Sender: TObject; ACol, ARow: Longint;
  Rect: TRect; State: TGridDrawState);
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
  LNodeBlocksCount,LNodeAccountsCount : Integer;
begin
  if Not Assigned(Node) then exit;

  if (ACol>=0) AND (ACol<length(FColumns)) then begin
    C := FColumns[ACol];
  end else begin
    C.ColumnType := act_account_number;
    C.width := -1;
  end;
  DrawGrid.Canvas.Font.Color:=clWindowText;
  if (ARow=0) then begin
    // Header
    s := CT_ColumnHeader[C.ColumnType];
    Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfCenter,tfVerticalCenter]);
  end else begin
    n_acc := AccountNumber(ARow);
    if (n_acc>=0) then begin
      BufferGetAccount(n_acc,account,LNodeBlocksCount,LNodeAccountsCount);
      ndiff := LNodeBlocksCount - account.GetLastUpdatedBlock;
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
          s := Inttostr(account.GetLastUpdatedBlock);
          Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfRight,tfVerticalCenter,tfSingleLine]);
        End;
        act_n_operation : Begin
          s := InttoStr(account.n_operation);
          Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfRight,tfVerticalCenter,tfSingleLine]);
        End;
        act_updated_state : Begin
          if TAccountComp.IsAccountBlockedByProtocol(account.account,LNodeBlocksCount) then begin
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
          s := account.name.ToPrintable;
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
            if TAccountComp.IsAccountForPrivateSale(account.accountInfo) then begin
              if TAccountComp.IsAccountLocked(account.accountInfo,LNodeBlocksCount) then begin
                DrawGrid.Canvas.Font.Color := clNavy;
              end else begin
                DrawGrid.Canvas.Font.Color := clRed;
              end;
            end else begin
              DrawGrid.Canvas.Font.Color := clGrayText
            end;
          end else if TAccountComp.IsAccountForSwap(account.accountInfo) then begin
            if TAccountComp.IsAccountForAccountSwap(account.accountInfo) then begin
              s := 'Account SWAP';
            end else if TAccountComp.IsAccountForCoinSwap(account.accountInfo) then begin
              s := 'SWAP '+TAccountComp.FormatMoney(account.accountInfo.price);
            end else s := 'SWAP';
            if TAccountComp.IsAccountLocked(account.accountInfo,LNodeBlocksCount) then begin
              DrawGrid.Canvas.Font.Color := clNavy;
            end else begin
              DrawGrid.Canvas.Font.Color := clRed;
            end;
          end else s := '';
          Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfLeft,tfVerticalCenter,tfSingleLine]);
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
  FBufferLastAccountNumber := -1;
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

procedure TAccountsGrid.SetAccountsGridDatasource(const Value: TAccountsGridDatasource);
begin
  if FAccountsGridDatasource=Value then Exit;
  FAccountsGridDatasource := Value;
  UpdateData;
end;

procedure TAccountsGrid.SetAccountsGridFilter(const Value: TAccountsGridFilter);
begin
  FAccountsGridFilter := Value;
  UpdateData;
end;

procedure TAccountsGrid.SetAllowMultiSelect(const Value: Boolean);
begin
  FAllowMultiSelect := Value;
  InitGrid;
end;

procedure TAccountsGrid.SetColumns(const AColumns: TAccountColumnArray);
begin
  FColumns := AColumns;
  InitGrid;
end;

procedure TAccountsGrid.SetDrawGrid(const Value: TDrawGrid);
begin
  if FDrawGrid=Value then exit;
  FDrawGrid := Value;
  if Assigned(Value) then begin
    Value.FreeNotification(self);
    FDrawGrid.OnDrawCell := OnGridDrawCell;
    UpdateData;
  end;
end;

procedure TAccountsGrid.SetNode(const Value: TNode);
begin
  if GetNode=Value then exit;
  FNodeNotifyEvents.Node := Value;
  UpdateData;
end;

procedure TAccountsGrid.TerminateAccountGridUpdateThread(AWaitUntilTerminated : Boolean);
var LTmp : TAccountsGridUpdateThread;
begin
  LTmp := FAccountsGridUpdateThread;
  FAccountsGridUpdateThread := Nil;
  if Assigned(Ltmp) then begin
    if Not LTmp.IsProcessing then AWaitUntilTerminated := True;
    if Not AWaitUntilTerminated then begin
      LTmp.FreeOnTerminate := True;
    end;
    LTmp.Terminate;
    if AWaitUntilTerminated then begin
      LTmp.WaitFor;
      FreeAndNil(LTmp);
    end;
  end;
end;

procedure TAccountsGrid.UnlockAccountsList;
begin
  UpdateAccountsBalance;
  InitGridRowCount;
  if Assigned(FOnUpdated) then FOnUpdated(Self);
end;

procedure TAccountsGrid.UpdateAccountsBalance;
var i : Integer;
  LAcc : TAccount;
begin
  if Assigned(Node) then begin
    case FAccountsGridDatasource of
      acds_Node: FAccountsBalance := Node.Bank.SafeBox.TotalBalance;
      acds_InternalList: begin
        FAccountsBalance := 0;
        for i := 0 to FAccountsList.Count - 1 do begin
          LAcc := Node.Bank.SafeBox.Account( FAccountsList.Get(i) );
          inc(FAccountsBalance, LAcc.balance);
        end;
      end;
    end;
  end;
end;

procedure TAccountsGrid.UpdateData;
begin
  UpdateAccountsBalance;
  TerminateAccountGridUpdateThread(False);
  if Assigned(Node) then begin
    case FAccountsGridDatasource of
      acds_NodeFiltered: begin
        FAccountsBalance := 0;
        FAccountsGridUpdateThread := TAccountsGridUpdateThread.Create(Self,AccountsGridFilter);
      end;
    end;
  end;
  InitGridRowCount;
  if Assigned(FOnAccountsGridUpdatedData) then FOnAccountsGridUpdatedData(Self);
end;

{ TOperationsGridUpdateThread }

procedure TOperationsGridUpdateThread.BCExecute;
var list : TList<TOperationResume>;
  i : Integer;
begin
  list := TList<TOperationResume>.Create;
  try
    DoUpdateOperationsGrid(FOperationsGrid.Node,FOperationsGrid.WalletKeys,FOperationsGrid.Passwords,list);
    if (Not Terminated) then begin
      FOperationsGrid.FOperationsResume.Clear;
      for i := 0 to list.Count-1 do begin
        FOperationsGrid.FOperationsResume.Add(list[i]);
      end;
      Queue(FOperationsGrid.InitGrid);
    end;
  finally
    list.Free;
  end;
end;

constructor TOperationsGridUpdateThread.Create(AOperationsGrid: TOperationsGrid);
begin
  FOperationsGrid := AOperationsGrid;
  inherited Create(True);
  FreeOnTerminate := False;
  Suspended := False;
end;

procedure TOperationsGridUpdateThread.DoUpdateOperationsGrid(const ANode : TNode; const AWalleTKeys : TWalletKeys;
  const APasswords : TList<String>; var AList: TList<TOperationResume>);
Var list : TList<Cardinal>;
  i,j : Integer;
  OPR : TOperationResume;
  Op : TPCOperation;
  opc : TPCOperationsComp;
  bstart,bend : int64;
  LOperationsResume : TOperationsResumeList;
  LLockedMempool : TPCOperationsComp;
  LEPasa : TEPasa;
begin
  if Not Assigned(ANode) then exit;
  AList.Clear;
  Try
    if (FOperationsGrid.MustShowAlwaysAnAccount) And (FOperationsGrid.AccountNumber<0) then exit;

    if FOperationsGrid.FPendingOperations then begin
      LLockedMempool := ANode.LockMempoolRead;
      try
        for i := LLockedMempool.Count - 1 downto 0 do begin
          Op := LLockedMempool.OperationsHashTree.GetOperation(i);
          If TPCOperation.OperationToOperationResume(0,Op,True,Op.SignerAccount,OPR) then begin
            OPR.NOpInsideBlock := i;
            OPR.Block := ANode.Bank.BlocksCount;
            OPR.Balance := LLockedMempool.SafeBoxTransaction.Account(Op.SignerAccount).balance;
            AList.Add(OPR);
          end;
        end;
      finally
        ANode.UnlockMempoolRead;
      end;
    end else begin
      if FOperationsGrid.AccountNumber<0 then begin
        opc := TPCOperationsComp.Create(Nil);
        try
          opc.bank := ANode.Bank;
          If FOperationsGrid.FBlockEnd<0 then begin
            If ANode.Bank.BlocksCount>0 then bend := ANode.Bank.BlocksCount-1
            else bend := 0;
          end else bend := FOperationsGrid.FBlockEnd;
          if FOperationsGrid.FBlockStart<0 then begin
            if (bend > 300) then bstart := bend - 300
            else bstart := 0;
          end else bstart:= FOperationsGrid.FBlockStart;
          If bstart<0 then bstart := 0;
          if bend>=ANode.Bank.BlocksCount then bend:=ANode.Bank.BlocksCount;
          while (bstart<=bend) and (Not Terminated) do begin
            opr := CT_TOperationResume_NUL;
            if (ANode.Bank.Storage.LoadBlockChainBlock(opc,bend)) then begin
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
              AList.Add(OPR);
              // Reverse operations inside a block
              for i := opc.Count - 1 downto 0 do begin
                if TPCOperation.OperationToOperationResume(bend,opc.Operation[i],True,opc.Operation[i].SignerAccount,opr) then begin
                  opr.NOpInsideBlock := i;
                  opr.Block := bend;
                  opr.time := opc.OperationBlock.timestamp;
                  AList.Add(opr);
                end;
              end;
            end else break;
            dec(bend);
          end;
        finally
          opc.Free;
        end;

      end else begin
        list := TList<Cardinal>.Create;
        Try
          LLockedMempool := ANode.LockMempoolRead;
          try
            LLockedMempool.OperationsHashTree.GetOperationsAffectingAccount(FOperationsGrid.AccountNumber,list);
            for i := list.Count - 1 downto 0 do begin
              Op := LLockedMempool.OperationsHashTree.GetOperation((list[i]));
              If TPCOperation.OperationToOperationResume(0,Op,False,FOperationsGrid.AccountNumber,OPR) then begin
                OPR.NOpInsideBlock := i;
                OPR.Block := LLockedMempool.OperationBlock.block;
                OPR.Balance := LLockedMempool.SafeBoxTransaction.Account(FOperationsGrid.AccountNumber).balance;
                AList.Add(OPR);
              end;
            end;
          finally
            ANode.UnlockMempoolRead;
          end;
        Finally
          list.Free;
        End;
        ANode.Bank.Storage.GetAccountOperations(FOperationsGrid.AccountNumber,-1,0,5000,0,AList);
      end;
    end;
  Finally
    for i := 0 to AList.Count-1 do begin
      OPR := AList[i];
      if TEPasaDecoder.TryDecodeEPASA(OPR.DestAccount,OPR.OriginalPayload,ANode,AWalleTKeys,APasswords,LEPasa) then begin
        OPR.DecodedEPasaPayload := LEPasa.ToString(True);
        AList[i] := OPR;
      end;
    end;
  End;
end;

{ TOperationsGrid }

constructor TOperationsGrid.Create(AOwner: TComponent);
begin
  FPasswords := TList<String>.Create;
  FWalletKeys := Nil;
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
  FOperationsGridUpdateThread := Nil;
  inherited;
end;

destructor TOperationsGrid.Destroy;
begin
  If Assigned(FOperationsGridUpdateThread) then begin
    FOperationsGridUpdateThread.Terminate;
    FOperationsGridUpdateThread.WaitFor;
    FreeAndNil(FOperationsGridUpdateThread);
  end;
  FOperationsResume.Free;
  FNodeNotifyEvents.Free;
  FPasswords.Free;
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
Var s, saux : String;
  opr : TOperationResume;
  LRectLeft, LRectRight : TRect;
begin
  DrawGrid.Canvas.Font.Color:=clWindowText;
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
      opr := FOperationsResume.Items[ARow-1];
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
        LRectRight := Rect;
        if opr.OriginalPayload.payload_type>0 then begin
          saux := '0x'+IntToHex(opr.OriginalPayload.payload_type,2);
          LRectLeft := Rect;
          LRectLeft.Width := 30;
          Rect.Inflate(-32,0,0,0);
          DrawGrid.Canvas.Font.Color := clBlue;
          DrawGrid.Canvas.Font.Style := [fsBold];
          Canvas_TextRect(DrawGrid.Canvas,LRectLeft,saux,State,[tfLeft,tfVerticalCenter,tfSingleLine]);
          if opr.DecodedEPasaPayload<>'' then begin
            DrawGrid.Canvas.Font.Style := [fsBold];
            s := opr.DecodedEPasaPayload
          end else DrawGrid.Canvas.Font.Style := [];
        end else if opr.OriginalPayload.payload_raw.ToString=s then begin
          DrawGrid.Canvas.Font.Style := [fsBold];
        end;
        DrawGrid.Canvas.Font.Color := clBlack;
        Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfLeft,tfVerticalCenter,tfSingleLine])
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
Var l : TList<Cardinal>;
  LLockedMempool : TPCOperationsComp;
begin
  Try
    if (AccountNumber<0) then begin
      If (FPendingOperations) then UpdateAccountOperations;
    end else begin
      l := TList<Cardinal>.Create;
      Try
        LLockedMempool := Node.LockMempoolRead;
        try
          If LLockedMempool.OperationsHashTree.GetOperationsAffectingAccount(AccountNumber,l)>0 then begin
            if l.IndexOf(AccountNumber)>=0 then UpdateAccountOperations;
          end;
        finally
          Node.UnlockMempoolRead;
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
  If Assigned(FOperationsGridUpdateThread) then begin
    FOperationsGridUpdateThread.Terminate;
    FOperationsGridUpdateThread.WaitFor;
    FreeAndNil(FOperationsGridUpdateThread);
  end;
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
  Result := FOperationsResume.Items[FDrawGrid.Row-1];
end;

procedure TOperationsGrid.ShowModalDecoder(WalletKeys: TWalletKeys; AppParams : TAppParams);
Var i : Integer;
  opr : TOperationResume;
  FRM : TFRMPayloadDecoder;
begin
  if Not Assigned(FDrawGrid) then exit;
  if (FDrawGrid.Row<=0) Or (FDrawGrid.Row>FOperationsResume.Count) then exit;
  opr := FOperationsResume.Items[FDrawGrid.Row-1];
  FRM := TFRMPayloadDecoder.Create(FDrawGrid.Owner);
  try
    FRM.Init(opr,WalletKeys,AppParams);
    FRM.ShowModal;
  finally
    FRM.Free;
  end;
end;

procedure TOperationsGrid.UpdateAccountOperations;
begin
  if Not Assigned(Node) then exit;
  If Assigned(FOperationsGridUpdateThread) then begin
    FOperationsGridUpdateThread.Terminate;
    FOperationsGridUpdateThread.WaitFor;
    FreeAndNil(FOperationsGridUpdateThread);
  end;
  FOperationsGridUpdateThread := TOperationsGridUpdateThread.Create(Self);
end;

{ TBlockChainGridUpdateThread }

procedure TBlockChainGridUpdateThread.BCExecute;
var Llist : TList<TBlockChainData>;
  i : Integer;
  LBlockStart, LBlockEnd : Integer;
begin
  if (Not Assigned(FBlockChainGrid.Node)) Or (Terminated) then Exit;

  if (FBlockChainGrid.FBlockStart>FBlockChainGrid.FBlockEnd) And (FBlockChainGrid.FBlockStart>=0) then FBlockChainGrid.FBlockEnd := -1;
  if (FBlockChainGrid.FBlockEnd>=0) And (FBlockChainGrid.FBlockEnd<FBlockChainGrid.FBlockStart) then FBlockChainGrid.FBlockStart:=-1;

  if FBlockChainGrid.FBlockStart>(FBlockChainGrid.FNodeNotifyEvents.Node.Bank.BlocksCount-1) then FBlockChainGrid.FBlockStart := -1;
  if (FBlockChainGrid.FBlockEnd>=0) And (FBlockChainGrid.FBlockEnd<FBlockChainGrid.Node.Bank.BlocksCount) then begin
    LBlockEnd := FBlockChainGrid.FBlockEnd
  end else begin
    if (FBlockChainGrid.FBlockStart>=0) And (FBlockChainGrid.FBlockStart+FBlockChainGrid.MaxBlocks<=FBlockChainGrid.Node.Bank.BlocksCount) then LBlockEnd := FBlockChainGrid.FBlockStart + FBlockChainGrid.MaxBlocks - 1
    else LBlockEnd := FBlockChainGrid.Node.Bank.BlocksCount-1;
  end;

  if (FBlockChainGrid.FBlockStart>=0) And (FBlockChainGrid.FBlockStart<FBlockChainGrid.Node.Bank.BlocksCount) then LBlockStart := FBlockChainGrid.FBlockStart
  else begin
    if LBlockEnd>FBlockChainGrid.MaxBlocks then LBlockStart := LBlockEnd - FBlockChainGrid.MaxBlocks + 1
    else LBlockStart := 0;
  end;


  Llist := TList<TBlockChainData>.Create;
  try
    DoUpdateBlockChainGrid(FBlockChainGrid.Node,Llist,LBlockStart,LBlockEnd);
    if (Not Terminated) then begin
      FBlockChainGrid.FBlockChainDataList.clear;
      for i := 0 to Llist.Count-1 do begin
        FBlockChainGrid.FBlockChainDataList.Add(Llist[i]);
      end;
      if Assigned(FBlockChainGrid.DrawGrid) then begin
        if Llist.Count>0 then FGridUpdateCount := Llist.Count+1
        else FGridUpdateCount := 2;
        Queue(RefreshGrid);
      end;
    end;
  finally
    Llist.Free;
  end;
end;

procedure TBlockChainGridUpdateThread.RefreshGrid;
begin
  if not Assigned(FBlockChainGrid) or not Assigned(FBlockChainGrid.DrawGrid)
    then Exit;
  FBlockChainGrid.DrawGrid.RowCount := FGridUpdateCount;
  FBlockChainGrid.FDrawGrid.Invalidate;
end;

constructor TBlockChainGridUpdateThread.Create(ABlockChainGrid : TBlockChainGrid);
begin
  FBlockChainGrid := ABlockChainGrid;
  inherited Create(True);
  FreeOnTerminate := False;
  Suspended := False;
end;

procedure TBlockChainGridUpdateThread.DoUpdateBlockChainGrid(ANode: TNode; var AList: TList<TBlockChainData>; ABlockStart, ABlockEnd : Int64);
Var //opc : TPCOperationsComp;
  bcd : TBlockChainData;
  opb : TOperationBlock;
  bn : TBigNum;
begin
    while (ABlockStart<=ABlockEnd) and (Not Terminated) do begin
      bcd := CT_TBlockChainData_NUL;
      if Not ANode.Bank.Storage.GetBlockInformation(ABlockEnd,opb,bcd.OperationsCount,bcd.Volume) then begin
        opb := ANode.Bank.SafeBox.GetBlockInfo(ABlockEnd);
      end;
      bcd.Block:=opb.block;
      bcd.Timestamp := opb.timestamp;
      bcd.BlockProtocolVersion := opb.protocol_version;
      bcd.BlockProtocolAvailable := opb.protocol_available;
      bcd.Reward := opb.reward;
      bcd.Fee := opb.fee;
      bcd.Target := opb.compact_target;
      bn := ANode.Bank.SafeBox.CalcBlockHashRateInHs(bcd.Block,FBlockChainGrid.HashRateAverageBlocksCount);
      try
        bcd.HashRateHs := bn.Value;
        bcd.HashRateKhs := bn.Divide(1000).Value;
      finally
        bn.Free;
      end;
      bn := TBigNum.TargetToHashRate(opb.compact_target);
      Try
        bcd.HashRateTargetHs := bn.Value / (CT_NewLineSecondsAvg);
        bcd.HashRateTargetKhs := bn.Divide(1000).Divide(CT_NewLineSecondsAvg).Value;
      finally
        bn.Free;
      end;
      bcd.MinerPayload := opb.block_payload;
      bcd.PoW := opb.proof_of_work;
      bcd.SafeBoxHash := opb.initial_safe_box_hash;
      if (Not Terminated) then begin
        bcd.TimeAverage200:=ANode.Bank.GetTargetSecondsAverage(bcd.Block,200);
        bcd.TimeAverage150:=ANode.Bank.GetTargetSecondsAverage(bcd.Block,150);
        bcd.TimeAverage100:=ANode.Bank.GetTargetSecondsAverage(bcd.Block,100);
        bcd.TimeAverage75:=ANode.Bank.GetTargetSecondsAverage(bcd.Block,75);
        bcd.TimeAverage50:=ANode.Bank.GetTargetSecondsAverage(bcd.Block,50);
        bcd.TimeAverage25:=ANode.Bank.GetTargetSecondsAverage(bcd.Block,25);
        bcd.TimeAverage10:=ANode.Bank.GetTargetSecondsAverage(bcd.Block,10);
        bcd.TimeAverage5:=ANode.Bank.GetTargetSecondsAverage(bcd.Block,5);
        AList.Add(bcd);
        if (ABlockEnd>0) then dec(ABlockEnd) else Break;
      end;
    end;
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
  FBlockChainDataList := TList<TBlockChainData>.Create;
  FShowTimeAverageColumns:=False;
  FHashRateAs:={$IFDEF PRODUCTION}hr_Giga{$ELSE}hr_Mega{$ENDIF};
  FBlockChainGridUpdateThread := Nil;
end;

destructor TBlockChainGrid.Destroy;
begin
  If Assigned(FBlockChainGridUpdateThread) then begin
    FBlockChainGridUpdateThread.Terminate;
    FBlockChainGridUpdateThread.WaitFor;
    FreeAndNil(FBlockChainGridUpdateThread);
  end;
  FNodeNotifyEvents.OnBlocksChanged := Nil;
  FNodeNotifyEvents.Node := Nil;
  FreeAndNil(FNodeNotifyEvents);
  FreeAndNil(FBlockChainDataList);
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
  If ShowTimeAverageColumns then DrawGrid.ColCount:=14
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
    DrawGrid.ColWidths[12] := 55; // Deviation
    DrawGrid.ColWidths[13] := 350; // Time average
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
  hr_base : Int64;
begin
  DrawGrid.Canvas.Font.Color:=clWindowText;
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
      7 : begin
        case HashRateAs of
          hr_Unit : s := 'h/s';
          hr_Kilo : s := 'Kh/s';
          hr_Mega : s := 'Mh/s';
          hr_Giga : s := 'Gh/s';
          hr_Tera : s := 'Th/s';
          hr_Peta : s := 'Ph/s';
          hr_Exa  : s := 'Eh/s';
        else s := '?h/s';
        end;
      end;
      8 : s := 'Miner Payload';
      9 : s := 'Proof of Work';
      10 : s := 'SafeBox Hash';
      11 : s := 'Protocol';
      12 : s := 'Deviation';
      13 : s := 'Time average';
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
    if ((ARow-1)<FBlockChainDataList.Count) then begin
      bcd := FBlockChainDataList[ARow-1];
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
          if FBlockChainDataList[ARow-1].Volume>0 then DrawGrid.Canvas.Font.Color := ClGreen
          else DrawGrid.Canvas.Font.Color := clGrayText;
          Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfRight,tfVerticalCenter,tfSingleLine]);
        end else begin
          DrawGrid.Canvas.Font.Color := clGrayText;
          s := '(no data)';
          Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfCenter,tfVerticalCenter,tfSingleLine]);
        end;
      end else if ACol=4 then begin
        s := TAccountComp.FormatMoney(bcd.Reward);
        if FBlockChainDataList[ARow-1].Reward>0 then DrawGrid.Canvas.Font.Color := ClGreen
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
        if (HashRateAs = hr_Unit) then begin
          s := Format('%.0n (%.0n)',[bcd.HashRateHs,bcd.HashRateTargetHs]);
        end else if (HashRateAs = hr_Kilo) then begin
          s := Format('%.2n (%.2n)',[bcd.HashRateHs/1000,bcd.HashRateTargetHs/1000]);
        end else begin
          case HashRateAs of
            hr_Kilo : hr_base := 1;
            hr_Mega : hr_base := 1000;
            hr_Giga : hr_base := 1000000;
            hr_Tera : hr_base := 1000000000;
            hr_Peta : hr_base := 1000000000000;
            hr_Exa  : hr_base := 1000000000000000;
          else hr_base := 1;
          end;
          s := Format('%.2n (%.2n)',[bcd.HashRateKhs/hr_base,bcd.HashRateTargetKhs/hr_base]);
        end;
        Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfRight,tfVerticalCenter]);
      end else if ACol=8 then begin
        if TCrypto.IsHumanReadable(bcd.MinerPayload) then
          s := TEncoding.ANSI.GetString(bcd.MinerPayload)
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
        deviation := ((CT_NewLineSecondsAvg - bcd.TimeAverage100) / CT_NewLineSecondsAvg)*100;
        s := Format('%.2f',[deviation])+' %';
        Canvas_TextRect(DrawGrid.Canvas,Rect,s,State,[tfRight,tfVerticalCenter,tfSingleLine]);
      end else if ACol=13 then begin
        s := Format('200:%.1f 150:%.1f 100:%.1f 50:%.1f 25:%.1f 10:%.1f 5:%.1f',[bcd.TimeAverage200,
           bcd.TimeAverage150,
           bcd.TimeAverage100,
           bcd.TimeAverage50,
           bcd.TimeAverage25,
           bcd.TimeAverage10,
           bcd.TimeAverage5]);
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

procedure TBlockChainGrid.SetHashRateAs(AValue: TShowHashRateAs);
begin
  if FHashRateAs=AValue then Exit;
  FHashRateAs:=AValue;
  if Assigned(FDrawGrid) then begin
    FDrawGrid.Invalidate;
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
begin
  If Assigned(FBlockChainGridUpdateThread) then begin
    FBlockChainGridUpdateThread.Terminate;
    FBlockChainGridUpdateThread.WaitFor;
    FreeAndNil(FBlockChainGridUpdateThread);
  end;
  FBlockChainGridUpdateThread := TBlockChainGridUpdateThread.Create(Self);
end;

end.
