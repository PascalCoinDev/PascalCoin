unit UDataSources;

{$mode delphi}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, UAccounts, UNode, UBlockchain, UCrypto, UCoreObjects,
  UCommon, UMemory, UConst, UCommon.Data, UCoreUtils,
  UCommon.Collections, Generics.Collections, Generics.Defaults, syncobjs;

type

  { TAccountsDataSourceBase }

  TAccountsDataSourceBase = class(TCustomDataSource<TAccount>)
  protected
    function GetColumns: TDataColumns; override;
  public
    function GetItemField(constref AItem: TAccount; const ABindingName: ansistring): variant; override;
  end;

  { TAccountsDataSource }

  TAccountsDataSource = class(TAccountsDataSourceBase)
  protected
    FIncludePending: boolean;
    FKeys: TSortedHashSet<TAccountKey>;
    function GetFilterKeys: TArray<TAccountKey>;
    procedure SetFilterKeys(const AKeys: TArray<TAccountKey>);
  public
    property IncludePending: boolean read FIncludePending write FIncludePending;
    property FilterKeys: TArray<TAccountKey> read GetFilterKeys write SetFilterKeys;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FetchAll(const AContainer: TList<TAccount>); override;
  end;

  { TMyAccountsDataSource }

  TMyAccountsDataSource = class(TAccountsDataSource)
  private
    FPBalanceSummary : PBalanceSummary;
  public
    property BalancePointer : PBalanceSummary read FPBalanceSummary write FPBalanceSummary;
    procedure FetchAll(const AContainer: TList<TAccount>); override;
  end;

  { TOperationsDataSourceBase }

  TOperationsDataSourceBase = class(TCustomDataSource<TOperationResume>)
  protected
    function GetColumns: TDataColumns; override;
  public
    function GetItemField(constref AItem: TOperationResume; const ABindingName: ansistring): variant; override;
  end;

  { TAccountsOperationsDataSource }

  TAccountsOperationsDataSource = class(TOperationsDataSourceBase)
  private
    FBlockDepth: Cardinal;
    FAccounts: TArray<cardinal>;
    FLastFetchID : String;
    FLastFetchBlockOperations : TArray<TOperationResume>;
  public
    property Accounts: TArray<cardinal> read FAccounts write FAccounts;
    property BlockDepth: cardinal read FBlockDepth write FBlockDepth;
    procedure FetchAll(const AContainer: TList<TOperationResume>); override;
  end;

  { TPendingOperationsDataSource }

  TPendingOperationsDataSource = class(TOperationsDataSourceBase)
  public
    procedure FetchAll(const AContainer: TList<TOperationResume>); override;
  end;

  { TOperationsDataSource }

  TOperationsDataSource = class(TOperationsDataSourceBase)
  private
    FStartBlock, FEndBlock: Cardinal;
  public
    property StartBlock : Cardinal read FStartBlock write FStartBlock;
    property EndBlock : Cardinal read FEndBlock write FEndBlock;
    procedure FetchAll(const AContainer: TList<TOperationResume>); override;
  end;

  { TBlockSummarySourceBase }

  TBlockSummarySourceBase = class(TCustomDataSource<TBlockSummary>)
  public type
    TShowHashRateAs = (hr_Kilo, hr_Mega, hr_Giga, hr_Tera);
  private
    FStart, FEnd: cardinal;
    FHashRateAs: TShowHashRateAs;
    FHashRateAverageBlocksCount: integer;
    function GetTimeSpan: TTimeSpan;
    procedure SetTimeSpan(const ASpan: TTimeSpan);
  protected
    function GetColumns: TDataColumns; override;
  public
    constructor Create(AOwner: TComponent); override;
    property TimeSpan: TTimeSpan read GetTimeSpan write SetTimeSpan;
    property StartBlock: cardinal read FStart write FStart;
    property EndBlock: cardinal read FEnd write FEnd;
    property HashRateAs: TShowHashRateAs read FHashRateAs write FHashRateAs;
    property HashRateAverageBlocksCount: integer read FHashRateAverageBlocksCount write FHashRateAverageBlocksCount;
    function GetItemField(constref AItem: TBlockSummary; const ABindingName: ansistring): variant; override;
  end;

  { TBlockSummarySource }

  TBlockSummarySource = class(TBlockSummarySourceBase)
  public
    procedure FetchAll(const AContainer: TList<TBlockSummary>); override;
  end;

implementation

uses
  Math, UWallet, UTime;

{ TAccountsDataSourceBase }

function TAccountsDataSourceBase.GetColumns: TDataColumns;
begin
  Result := TDataColumns.Create(
    TDataColumn.From('AccountNumber', True),
    TDataColumn.From('Account'),
    TDataColumn.From('Name'),
    TDataColumn.From('Display'),
    TDataColumn.From('Balance'),
    TDataColumn.From('BalanceDecimal'),
    TDataColumn.From('Key'),
    TDataColumn.From('Type'),
    TDataColumn.From('State'),
    TDataColumn.From('Price'),
    TDataColumn.From('PriceDecimal'),
    TDataColumn.From('LockedUntil'),
    TDataColumn.From('NOperation'),
    TDataColumn.From('LastUpdatedBlock')
  );
end;

function TAccountsDataSourceBase.GetItemField(constref AItem: TAccount; const ABindingName: ansistring): variant;
var
  index: integer;
begin
  if ABindingName = 'AccountNumber' then
    Result := AItem.account
  else if ABindingName = 'Account' then
    Result := AItem.AccountString
  else if ABindingName = 'Name' then
    Result := AItem.Name
  else if ABindingName = 'Display' then
    Result := AItem.DisplayString
  else if ABindingName = 'Balance' then
    Result := AItem.Balance
  else if ABindingName = 'BalanceDecimal' then
    Result := TAccountComp.FormatMoneyDecimal(AItem.Balance)
  else if ABindingName = 'Key' then
    Result := TAccountComp.AccountPublicKeyExport(AItem.accountInfo.accountKey)
  else if ABindingName = 'Type' then
    Result := AItem.account_type
  else if ABindingName = 'State' then
    Result := AItem.accountInfo.state
  else if ABindingName = 'Price' then
    Result := AItem.accountInfo.price
  else if ABindingName = 'PriceDecimal' then
    Result := TAccountComp.FormatMoneyDecimal(AItem.accountInfo.price)
  else if ABindingName = 'LockedUntil' then
    Result := AItem.accountInfo.locked_until_block
  else if ABindingName = 'NOperation' then
    Result := AItem.n_operation
  else if ABindingName = 'LastUpdatedBlock' then
    Result := AItem.updated_block
  else
    raise Exception.Create(Format('Field not found "%s"', [ABindingName]));
end;

{ TAccountsDataSource }

constructor TAccountsDataSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKeys := TSortedHashSet<TAccountKey>.Create(TAccountKeyComparer.Create, TAccountKeyEqualityComparer.Create);
end;

destructor TAccountsDataSource.Destroy;
begin
  FKeys.Free;
end;

function TAccountsDataSource.GetFilterKeys: TArray<TAccountKey>;
begin
  EnterLock;
  try
    Result := FKeys.ToArray;
  finally
    ReleaseLock;
  end;
end;

procedure TAccountsDataSource.SetFilterKeys(const AKeys: TArray<TAccountKey>);
var
  i: integer;
begin
  EnterLock;
  try
    FKeys.Clear;
    for i := Low(AKeys) to High(AKeys) do
      FKeys.Add(AKeys[i]);
  finally
    ReleaseLock;
  end;
end;

procedure TAccountsDataSource.FetchAll(const AContainer: TList<TAccount>);
var
  i: integer;
  acc: TAccount;
  safeBox: TPCSafeBox;
begin
  safeBox := TNode.Node.Bank.SafeBox;
  safeBox.StartThreadSafe;
  try
    if FKeys.Count = 0 then
      for i := 0 to safeBox.AccountsCount - 1 do
        AContainer.Add(safeBox.Account(i)) // Load all accounts
    else
      for i := 0 to safeBox.AccountsCount - 1 do begin
        // Load key-matching accounts
        if FIncludePending then
          acc := TNode.Node.Operations.SafeBoxTransaction.Account(i)
        else
          acc := safeBox.Account(i);
        if FKeys.Contains(acc.accountInfo.accountKey) then
          AContainer.Add(acc);
      end;
  finally
    safeBox.EndThreadSave;
  end;
end;

{ TMyAccountsDataSource }

procedure TMyAccountsDataSource.FetchAll(const AContainer: TList<TAccount>);
var
  i: integer;
  LAccs: TArray<TAccount>;
  LBalanceSummary : TBalanceSummary;
begin
  LAccs := TCoreTool.GetUserAccounts(LBalanceSummary, FIncludePending);
  FPBalanceSummary^ := LBalanceSummary;
  if FKeys.Count > 0 then begin
    for i := Low(LAccs) to High(LAccs) do
      if FKeys.Contains(LAccs[i].accountInfo.accountKey) then
        AContainer.Add(LAccs[i]);
  end else AContainer.AddRange(LAccs);
end;

{ TOperationsDataSourceBase }

function TOperationsDataSourceBase.GetColumns: TDataColumns;
begin
  Result := TDataColumns.Create(
    TDataColumn.From('OPHASH', True),
    TDataColumn.From('UnixTime'),
    TDataColumn.From('Time'),
    TDataColumn.From('Block'),
    TDataColumn.From('Index'),
    TDataColumn.From('BlockLocation'),
    TDataColumn.From('BlockLocationSortable'),
    TDataColumn.From('Account'),
    TDataColumn.From('AccountNumber'),
    TDataColumn.From('Type'),
    TDataColumn.From('SubType'),
    TDataColumn.From('Amount'),
    TDataColumn.From('AmountDecimal'),
    TDataColumn.From('Fee'),
    TDataColumn.From('FeeDecimal'),
    TDataColumn.From('Balance'),
    TDataColumn.From('BalanceDecimal'),
    TDataColumn.From('Payload'),
    TDataColumn.From('Description')
    );
end;

function TOperationsDataSourceBase.GetItemField(constref AItem: TOperationResume; const ABindingName: ansistring): variant;
var
  index: integer;
begin
  if ABindingName = 'OPHASH' then
    Result := TPCOperation.OperationHashAsHexa(AItem.OperationHash)
  else if ABindingName = 'UnixTime' then
    Result := AItem.Time
  else if ABindingName = 'Time' then
    Result := UnixTimeToLocalStr(AItem.time)
  else if ABindingName = 'Block' then
    Result := AItem.Block
  else if ABindingName = 'Index' then
    Result := AItem.NOpInsideBlock
  else if ABindingName = 'BlockLocation' then
    Result := IIF(AItem.OpType <> CT_PseudoOp_Reward, IntToStr(AItem.Block) + '/' + IntToStr(AItem.NOpInsideBlock + 1), IntToStr(AItem.Block))
  else if ABindingName = 'BlockLocationSortable' then
    Result := IIF(AItem.OpType <> CT_PseudoOp_Reward, UInt64(AItem.Block) * 4294967296 + UInt32(AItem.NOpInsideBlock), 0)  // number pattern = [block][opindex]
  else if ABindingName = 'Account' then
    Result := TAccountComp.AccountNumberToAccountTxtNumber(AItem.AffectedAccount)
  else if ABindingName = 'AccountNumber' then
    Result := AItem.AffectedAccount
  else if ABindingName = 'Type' then
    Result := AItem.OpType
  else if ABindingName = 'SubType' then
    Result := AItem.OpSubtype
  else if ABindingName = 'Amount' then
    Result := AItem.Amount
  else if ABindingName = 'AmountDecimal' then
    Result := TAccountComp.FormatMoneyDecimal(AItem.Amount)
  else if ABindingName = 'Fee' then
    Result := AItem.Fee
  else if ABindingName = 'FeeDecimal' then
    Result := TAccountComp.FormatMoneyDecimal(AItem.Fee)
  else if ABindingName = 'Balance' then
    Result := AItem.Balance
  else if ABindingName = 'BalanceDecimal' then
    Result := TAccountComp.FormatMoneyDecimal(AItem.Balance)
  else if ABindingName = 'Payload' then
    Result := AItem.PrintablePayload
  else if ABindingName = 'Description' then
    Result := AItem.OperationTxt
  else
    raise Exception.Create(Format('Field not found [%s]', [ABindingName]));
end;

{ TAccountsOperationsDataSource }

procedure TAccountsOperationsDataSource.FetchAll(const AContainer: TList<TOperationResume>);
var
  LNode: TNode;
  LBlockDepth : Integer;
  LFetchID : String;
  LAccounts : TArray<Cardinal>;

  function ComputeFetchID : String;
  var LAccFetchID : string;
  begin
    // note: FAccounts is never empty
    LAccFetchID := Length(FAccounts).ToString + FAccounts[Low(FAccounts)].ToString + FAccounts[High(FAccounts)].ToString;
    Result := LAccFetchID + FBlockDepth.ToString + TCrypto.ToHexaString(LNode.Bank.SafeBox.SafeBoxHash);

  end;

begin
  if Length(FAccounts) = 0 then
    exit;
  LNode := TNode.Node;
  if not Assigned(LNode) then
    exit;
  LBlockDepth := ClipValue(FBlockDepth, 0, LNode.Bank.BlocksCount);
  LNode.Bank.SafeBox.StartThreadSafe;
  try
    AContainer.AddRange( LNode.GetPendingOperationsAffectingAccounts(FAccounts, 0, MaxInt) );
    // Performance: only fetch from storage if state/args are different from last call
    LFetchID := ComputeFetchID;
    if LFetchID <> FLastFetchID then begin
      FLastFetchID := LFetchID;
      FLastFetchBlockOperations := LNode.GetStoredOperationsAffectingAccounts(FAccounts, LBlockDepth, 0, MaxInt);
    end;
    AContainer.AddRange( FLastFetchBlockOperations );
  finally
    LNode.Bank.SafeBox.EndThreadSave;
  end;
end;

{ TPendingOperationsDataSource }

procedure TPendingOperationsDataSource.FetchAll(const AContainer: TList<TOperationResume>);
var
  i: integer;
  node: TNode;
  Op: TPCOperation;
  OPR: TOperationResume;
begin
  node := TNode.Node;
  if not Assigned(Node) then
    exit;
  for i := Node.Operations.Count - 1 downto 0 do
  begin
    Op := Node.Operations.OperationsHashTree.GetOperation(i);
    if TPCOperation.OperationToOperationResume(0, Op, False, Op.SignerAccount, OPR) then
    begin
      OPR.NOpInsideBlock := i;
      OPR.Block := Node.Bank.BlocksCount;
      OPR.Balance := Node.Operations.SafeBoxTransaction.Account(Op.SignerAccount).balance;
      AContainer.Add(OPR);
    end;
  end;
end;

{ TOperationsDataSource }

procedure TOperationsDataSource.FetchAll(const AContainer: TList<TOperationResume>);
var
  block, i, j, keyIndex: integer;
  OPR: TOperationResume;
  blockOps: TPCOperationsComp;
  node: TNode;
  GC: TDisposables;

begin
  node := TNode.Node;
  if not Assigned(Node) then
    exit;
  blockOps := GC.AddObject(TPCOperationsComp.Create(nil)) as TPCOperationsComp;
  for block := FEndBlock downto FStartBlock do
  begin  /// iterate blocks correctly
    opr := CT_TOperationResume_NUL;
    if (Node.Bank.Storage.LoadBlockChainBlock(blockOps, block)) then begin
      AContainer.Add(blockOps.GetMinerRewardPseudoOperation);
      // if blockOps.Count = 0 then exit;
      for i := blockOps.Count - 1 downto 0 do
        if TPCOperation.OperationToOperationResume(block, blockOps.Operation[i], False, blockOps.Operation[i].SignerAccount, opr) then begin
          opr.NOpInsideBlock := i;
          opr.Block := block;
          opr.time := blockOps.OperationBlock.timestamp;
          AContainer.Add(opr);
        end; // reverse order
    end else break;
  end;
end;

{ TBlockSummarySourceBase }

function TBlockSummarySourceBase.GetTimeSpan: TTimeSpan;
begin
  Result := TTimeSpan.FromSeconds(CT_NewLineSecondsAvg * (FEnd - FStart + 1));
end;

procedure TBlockSummarySourceBase.SetTimeSpan(const ASpan: TTimeSpan);
var
  LNode: TNode;
begin
  LNode := TNode.Node;
  if not Assigned(LNode) then
    exit;
  FEnd := LNode.Bank.BlocksCount - 1;
  FStart := ClipValue(FEnd - (ASpan.TotalBlockCount + 1), 0, FEnd);
end;

function TBlockSummarySourceBase.GetColumns: TDataColumns;
var
  LHashType: string;
begin
  case HashRateAs of
    hr_Kilo: LHashType := 'Kh/s';
    hr_Mega: LHashType := 'Mh/s';
    hr_Giga: LHashType := 'Gh/s';
    hr_Tera: LHashType := 'Th/s';
    else LHashType := '?h/s';
  end;
  Result := TDataColumns.Create(
    TDataColumn.From('UnixTime'),
    TDataColumn.From('Time'),
    TDataColumn.From('Block'),
    TDataColumn.From('Ops'),
    TDataColumn.From(LHashType),
    TDataColumn.From('Volume'),
    TDataColumn.From('Reward'),
    TDataColumn.From('Fee'),
    TDataColumn.From('FeeDecimal'),
    TDataColumn.From('Target'),
    TDataColumn.From('MinerPayload'),
    TDataColumn.From('POW'),
    TDataColumn.From('SBH'),
    TDataColumn.From('Protocol'),
    TDataColumn.From('Deviation'),
    TDataColumn.From('TimeAverage')
  );
end;

constructor TBlockSummarySourceBase.Create(AOwner: TComponent);
var
  LNode: TNode;
begin
  inherited Create(AOwner);
  LNode := TNode.Node;
  if Assigned(LNode) then begin
    FStart := 0;
    FEnd := LNode.Bank.BlocksCount - 1;
  end else begin
    FStart := 0;
    FEnd := 0;
  end;
end;

function TBlockSummarySourceBase.GetItemField(constref AItem: TBlockSummary; const ABindingName: ansistring): variant;
var
  LHR_Base: int64;
  LHashType: string;
  LDeviation: double;
begin
  case FHashRateAs of
    hr_Kilo: LHashType := 'Kh/s';
    hr_Mega: LHashType := 'Mh/s';
    hr_Giga: LHashType := 'Gh/s';
    hr_Tera: LHashType := 'Th/s';
    else LHashType := '?h/s';
  end;
  //if ABindingName = 'UnixTime' then
  //  Result := DateTimeToStr(UnivDateTime2LocalDateTime(UnixToUnivDateTime((AItem.Timestamp))))
  //else if ABindingName = 'Time' then
  //  Result := DateTimeToStr(UnivDateTime2LocalDateTime(UnixToUnivDateTime((AItem.Timestamp))))
  if ABindingName = 'UnixTime' then
    Result := AItem.Timestamp
  else if ABindingName = 'Time' then
    Result := UnixTimeToLocalStr(AItem.Timestamp)
  else if ABindingName = 'Block' then
    Result := AItem.Block
  else if ABindingName = 'Ops' then
    Result := AItem.OperationsCount
  else if ABindingName = 'Volume' then
    Result := TAccountComp.FormatMoney(AItem.Volume)
  else if ABindingName = 'Reward' then
    Result := TAccountComp.FormatMoney(AItem.Reward)
  else if ABindingName = 'Fee' then
    Result := AItem.Fee
  else if ABindingName = 'FeeDecimal' then
    Result := TAccountComp.FormatMoneyDecimal(AItem.Fee)
  else if ABindingName = 'Target' then
    Result := IntToHex(AItem.Target, 8)
  else if ABindingName = LHashType then begin
    case HashRateAs of
      hr_Kilo: LHR_Base := 1;
      hr_Mega: LHR_Base := 1000;
      hr_Giga: LHR_Base := 1000000;
      hr_Tera: LHR_Base := 1000000000;
      else
        Result := 1;
    end;
    Result := Format('%.2n (%.2n)', [AItem.HashRateKhs / LHR_Base, AItem.HashRateTargetKhs / LHR_Base]);
  end else if ABindingName = 'MinerPayload' then begin
    if TCrypto.IsHumanReadable(AItem.MinerPayload) then
      Result := AItem.MinerPayload
    else
      Result := TCrypto.ToHexaString(AItem.MinerPayload);
  end else if ABindingName = 'POW' then
    Result := TCrypto.ToHexaString(AItem.PoW)
  else if ABindingName = 'SBH' then
    Result := TCrypto.ToHexaString(AItem.SafeBoxHash)
  else if ABindingName = 'Protocol' then
    Result := IntToStr(AItem.BlockProtocolVersion) + '-' + IntToStr(AItem.BlockProtocolAvailable)
  else if ABindingName = 'Deviation' then begin
    LDeviation := ((CT_NewLineSecondsAvg - AItem.TimeAverage100) / CT_NewLineSecondsAvg) * 100;
    Result := Format('%.2f', [LDeviation]) + ' %';
  end else if ABindingName = 'TimeAverage' then
    Result := Format('200:%.1f 150:%.1f 100:%.1f 75:%.1f 50:%.1f 25:%.1f 10:%.1f', [AItem.TimeAverage200, AItem.TimeAverage150, AItem.TimeAverage100, AItem.TimeAverage75, AItem.TimeAverage50, AItem.TimeAverage25, AItem.TimeAverage10])
  else
    raise Exception.Create(Format('Field not found [%s]', [ABindingName]));
end;

{ TBlockSummarySource }

procedure TBlockSummarySource.FetchAll(const AContainer: TList<TBlockSummary>);
var
  LStart, LEnd, LIdx: cardinal;
  LOperationComp: TPCOperationsComp;
  LBlockChainData: TBlockSummary;
  LOperationBlock: TOperationBlock;
  LBigNum: TBigNum;
  LNode: TNode;
begin
  LNode := TNode.Node;
  if not Assigned(LNode) then
    Exit;
  if LNode.Bank.BlocksCount <= 0 then
    Exit;
  LEnd := EndBlock;
  LStart := StartBlock;
  LOperationComp := TPCOperationsComp.Create(nil);
  try
    LOperationComp.bank := LNode.Bank;

    for LIdx := LEnd downto LStart do begin
      LBlockChainData := CT_BlockSummary_NUL;
      LOperationBlock := LNode.Bank.SafeBox.Block(LIdx).blockchainInfo;
      LBlockChainData.Block := LOperationBlock.block;
      LBlockChainData.Timestamp := LOperationBlock.timestamp;
      LBlockChainData.BlockProtocolVersion := LOperationBlock.protocol_version;
      LBlockChainData.BlockProtocolAvailable := LOperationBlock.protocol_available;
      LBlockChainData.Reward := LOperationBlock.reward;
      LBlockChainData.Fee := LOperationBlock.fee;
      LBlockChainData.Target := LOperationBlock.compact_target;
      LBlockChainData.HashRateKhs := LNode.Bank.SafeBox.CalcBlockHashRateInKhs(LBlockChainData.Block, HashRateAverageBlocksCount);
      LBigNum := TBigNum.TargetToHashRate(LOperationBlock.compact_target);
      try
        LBlockChainData.HashRateTargetKhs := LBigNum.Divide(1024).Divide(CT_NewLineSecondsAvg).Value;
      finally
        LBigNum.Free;
      end;
      LBlockChainData.MinerPayload := LOperationBlock.block_payload;
      LBlockChainData.PoW := LOperationBlock.proof_of_work;
      LBlockChainData.SafeBoxHash := LOperationBlock.initial_safe_box_hash;
      LBlockChainData.AccumulatedWork := LNode.Bank.SafeBox.Block(LBlockChainData.Block).AccumulatedWork;
      if (LNode.Bank.LoadOperations(LOperationComp, LIdx)) then begin
        LBlockChainData.OperationsCount := LOperationComp.Count;
        LBlockChainData.Volume := LOperationComp.OperationsHashTree.TotalAmount + LOperationComp.OperationsHashTree.TotalFee;
      end;
      LBlockChainData.TimeAverage200 := LNode.Bank.GetTargetSecondsAverage(LBlockChainData.Block, 200);
      LBlockChainData.TimeAverage150 := LNode.Bank.GetTargetSecondsAverage(LBlockChainData.Block, 150);
      LBlockChainData.TimeAverage100 := LNode.Bank.GetTargetSecondsAverage(LBlockChainData.Block, 100);
      LBlockChainData.TimeAverage75 := LNode.Bank.GetTargetSecondsAverage(LBlockChainData.Block, 75);
      LBlockChainData.TimeAverage50 := LNode.Bank.GetTargetSecondsAverage(LBlockChainData.Block, 50);
      LBlockChainData.TimeAverage25 := LNode.Bank.GetTargetSecondsAverage(LBlockChainData.Block, 25);
      LBlockChainData.TimeAverage10 := LNode.Bank.GetTargetSecondsAverage(LBlockChainData.Block, 10);
      AContainer.Add(LBlockChainData);
    end;
  finally
    LOperationComp.Free;
  end;
end;

end.
