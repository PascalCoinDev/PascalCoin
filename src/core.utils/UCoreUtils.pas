unit UCoreUtils;

{ Copyright (c) 2018 by PascalCoin Project

  Contains common types for Core module.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
    Herman Schoenfeld <herman@sphere10.com>: unit creator
    Ugochukwu Mmaduekwe - added TOperationsManager class
}

{$mode delphi}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, UCrypto, UAccounts, UBlockChain, UOpTransaction, UNode, UCommon, UNetProtocol,
  Generics.Collections, Generics.Defaults, UCoreObjects, Forms, Dialogs, LCLType, UCellRenderers, UCommon.Collections;

type

  { TAccountComparer }

  TAccountComparer = class(TComparer<TAccount>)
    function Compare(constref ALeft, ARight: T): integer; override;
    class function DoCompare(constref ALeft, ARight: TAccount): integer; inline;
  end;

  { TAccountEqualityComparer }

  TAccountEqualityComparer = class(TEqualityComparer<TAccount>)
  public
    function Equals(constref ALeft, ARight: TAccount): boolean; override;
    function GetHashCode(constref AValue: TAccount): UInt32; override;
    class function AreEqual(constref ALeft, ARight: TAccount): boolean;
    class function CalcHashCode(constref AValue: TAccount): UInt32;
  end;

  { TAccountKeyComparer }

  TAccountKeyComparer = class(TComparer<TAccountKey>)
    function Compare(constref ALeft, ARight: T): integer; override;
    class function DoCompare(constref ALeft, ARight: TAccountKey): integer; inline;
  end;

  { TAccountKeyEqualityComparer }

  TAccountKeyEqualityComparer = class(TEqualityComparer<TAccountKey>)
  public
    function Equals(constref ALeft, ARight: TAccountKey): boolean; override;
    function GetHashCode(constref AValue: TAccountKey): UInt32; override;
    class function AreEqual(constref ALeft, ARight: TAccountKey): boolean;
    class function CalcHashCode(constref AValue: TAccountKey): UInt32;
  end;

  { TCoreTool }

  TCoreTool = class
  public
    class function GetSignerCandidates(ANumOps: integer; ASingleOperationFee: int64; const ACandidates: array of TAccount): TArray<TAccount>; static;
    class function GetOperationShortText(const OpType, OpSubType: DWord): ansistring; static; inline;
    class function GetUserBalance(IncludePending: boolean = False): TBalanceSummary;
    class function GetUserAccounts(IncludePending: boolean = False): TArray<TAccount>; overload;
    class function GetUserAccounts(out Balance: TBalanceSummary; IncludePending: boolean = False): TArray<TAccount>; overload;
    class function GetUserAccountNumbers: TArray<cardinal>;
  end;

  { TNodeHelper }

  TNodeHelper = class helper for TNode
   function HasBestKnownBlockchainTip: boolean;
   function BlockTip : Cardinal;
   function GetAccount(AAccountNumber : Cardinal; AIncludePending : boolean = true) : TAccount;
   function GetAccounts(const AAccountNumbers : array of Cardinal; AIncludePending : boolean = true) : TArray<TAccount>;
   function GetPendingOperationsAffectingAccounts(const AAccountNumbers: array of Cardinal; ASkipCount, ATakeCount : Integer) : TArray<TOperationResume>;
   function GetStoredOperationsAffectingAccounts(const AAccountNumbers : array of Cardinal; ABlockDepth, ASkipCount, ATakeCount : Integer) : TArray<TOperationResume>;
  end;

  { TAccountHelper }

  TAccountHelper = record helper for TAccount
    function GetAccountString : AnsiString;
    function GetDisplayString : AnsiString;
    function GetInfoText(const ABank : TPCBank) : utf8string;
    property AccountString : AnsiString read GetAccountString;
    property DisplayString : AnsiString read GetDisplayString;
  end;

  { TOperationResumeHelper }

  TOperationResumeHelper = record helper for TOperationResume
    function GetPrintableOPHASH : AnsiString;
    function GetInfoText(const ABank : TPCBank) : utf8string;
  end;

  { TTimeSpanHelper }

  TTimeSpanHelper = record helper for TTimeSpan
    function TotalBlockCount : Integer;
  end;

implementation

uses
  UMemory, UConst, UWallet, UECIES, UAES, ULog;

{ TCoreTool }

class function TCoreTool.GetSignerCandidates(ANumOps: integer; ASingleOperationFee: int64; const ACandidates: array of TAccount): TArray<TAccount>;
var
  i, PoorSenderCount: integer;
  Fee, maxSignerFee, minSignerFee: int64;
  acc: TAccount;
begin
  //make deep copy of accounts!!! Very Important
  Result := TArrayTool<TAccount>.Copy(ACandidates);
  Fee := ASingleOperationFee;
  PoorSenderCount := 0;
  for i := Low(Result) to High(Result) do
  begin
    acc := Result[i];
    if (acc.Balance < Fee) then
      Inc(PoorSenderCount);
  end;

  maxSignerFee := ANumOps * Fee;
  minSignerFee := maxSignerFee - (PoorSenderCount * Fee);

  for i := High(Result) downto Low(Result) do
  begin
    acc := Result[i];
    if not (acc.Balance >= maxSignerFee) then
      TArrayTool<TAccount>.RemoveAt(Result, i);
  end;
end;

class function TCoreTool.GetOperationShortText(const OpType, OpSubType: DWord): ansistring;
begin
  case OpType of
    CT_PseudoOp_Reward: case OpSubType of
        0, CT_PseudoOpSubtype_Miner: Result := 'Miner Reward';
        CT_PseudoOpSubtype_Developer: Result := 'Developer Reward';
        else
          Result := 'Unknown';
      end;
    CT_Op_Transaction: case OpSubType of
        CT_OpSubtype_TransactionSender: Result := 'Send';
        CT_OpSubtype_TransactionReceiver: Result := 'Receive';
        CT_OpSubtype_BuyTransactionBuyer: Result := 'Buy Account Direct';
        CT_OpSubtype_BuyTransactionTarget: Result := 'Purchased Account Direct';
        CT_OpSubtype_BuyTransactionSeller: Result := 'Sold Account Direct';
        else
          Result := 'Unknown';
      end;
    CT_Op_Changekey: Result := 'Change Key (legacy)';
    CT_Op_Recover: Result := 'Recover';
    CT_Op_ListAccountForSale: case OpSubType of
        CT_OpSubtype_ListAccountForPublicSale: Result := 'For Sale';
        CT_OpSubtype_ListAccountForPrivateSale: Result := 'Exclusive Sale';
        else
          Result := 'Unknown';
      end;
    CT_Op_DelistAccount: Result := 'Remove Sale';
    CT_Op_BuyAccount: case OpSubType of
        CT_OpSubtype_BuyAccountBuyer: Result := 'Buy Account';
        CT_OpSubtype_BuyAccountTarget: Result := 'Purchased Account';
        CT_OpSubtype_BuyAccountSeller: Result := 'Sold Account';
        else
          Result := 'Unknown';
      end;
    CT_Op_ChangeKeySigned: Result := 'Change Key';
    CT_Op_ChangeAccountInfo: Result := 'Change Info';
    CT_Op_MultiOperation: case OpSubType of
        CT_OpSubtype_MultiOperation_Global: Result := 'Mixed-Transfer';
        CT_OpSubtype_MultiOperation_AccountInfo: Result := 'Mixed-Change';
      end;
    else
      Result := 'Unknown';
  end;
end;

class function TCoreTool.GetUserBalance(IncludePending: boolean = False): TBalanceSummary;
begin
  GetUserAccounts(Result, IncludePending);
end;

class function TCoreTool.GetUserAccounts(IncludePending: boolean = False): TArray<TAccount>;
var
  LBalance : TBalanceSummary;
begin
  Result := GetUserAccounts(LBalance, IncludePending);
end;

class function TCoreTool.GetUserAccounts(out Balance: TBalanceSummary; IncludePending: boolean = False): TArray<TAccount>;
var
  i, j: integer;
  LAccs: TList<TAccount>;
  LAcc: TAccount;
  LList: TOrderedCardinalList;
  Disposables: TDisposables;
begin
  Balance := CT_BalanceSummary_Nil;
  LAccs := Disposables.AddObject(TList<TAccount>.Create) as TList<TAccount>;
  TNode.Node.Bank.SafeBox.StartThreadSafe;
  try
    for i := 0 to TWallet.Keys.Count - 1 do begin
      LList := TWallet.Keys.AccountsKeyList.AccountKeyList[i];
      for j := 0 to LList.Count - 1 do begin
        if IncludePending then
          LAcc := TNode.Node.Operations.SafeBoxTransaction.Account(LList.Get(j))
        else begin
          LAcc := TNode.Node.Bank.SafeBox.Account(LList.Get(j));
        end;
        LAccs.Add(LAcc);
        Inc(Balance.TotalPASA);
        Inc(Balance.TotalPASC, LAcc.Balance);
      end;
    end;
  finally
    TNode.Node.Bank.SafeBox.EndThreadSave;
  end;
  LAccs.Sort(TAccountComparer.Create);
  Result := LAccs.ToArray;
end;

class function TCoreTool.GetUserAccountNumbers: TArray<cardinal>;
var
  i, j: integer;
  LAccs: TSortedList<cardinal>;
  LList: TOrderedCardinalList;
  Disposables: TDisposables;
begin
  LAccs := Disposables.AddObject(TSortedList<cardinal>.Create) as TSortedList<cardinal>;
  for i := 0 to TWallet.Keys.AccountsKeyList.Count - 1 do
  begin
    LList := TWallet.Keys.AccountsKeyList.AccountKeyList[i];
    for j := 0 to LList.Count - 1 do
      LAccs.Add(LList.Get(j));
  end;
  Result := LAccs.ToArray;
end;


{ TNodeHelper }

function TNodeHelper.HasBestKnownBlockchainTip: boolean;
var
  LReady: boolean;
  LMsg: ansistring;
  LDestBlock : Cardinal;
begin
  LReady := Self.Bank.IsReady(LMsg);
  if LReady and TNetData.NetData.IsGettingNewBlockChainFromClient then begin
    LDestBlock := TNetData.NetData.MaxRemoteOperationBlock.block;
    Result := Self.Bank.BlocksCount = TNetData.NetData.MaxRemoteOperationBlock.block;
  end;
end;

function TNodeHelper.BlockTip : Cardinal;
begin
  Result := ClipValue(Self.Bank.BlocksCount - 1, 0, MaxInt);
end;

function TNodeHelper.GetAccount(AAccountNumber : Cardinal; AIncludePending : boolean = true) : TAccount;
var LOps : TArray<TAccount>;
begin
  LOps := Self.GetAccounts([AAccountNumber], AIncludePending);
  Result := LOps[Low(Lops)];
end;

function TNodeHelper.GetAccounts(const AAccountNumbers : array of Cardinal; AIncludePending : boolean = true) : TArray<TAccount>;
var i : integer;
begin
  SetLength(Result, Length(AAccountNumbers));
  if AIncludePending then
    for i := Low(AAccountNumbers) to High(AAccountNumbers) do
      Result[i] := Self.Operations.SafeBoxTransaction.Account(AAccountNumbers[i])
  else
    for i := Low(AAccountNumbers) to High(AAccountNumbers) do
      Result[i] := Self.Bank.SafeBox.Account(AAccountNumbers[i]);
end;

function TNodeHelper.GetPendingOperationsAffectingAccounts(const AAccountNumbers: array of Cardinal; ASkipCount, ATakeCount : Integer) : TArray<TOperationResume>;
var
  LList : Classes.TList;
  LOps : TList<TOperationResume>;
  LOp : TPCOperation;
  LOpResume : TOperationResume;
  LAccNo : Cardinal;
  LNumOps, i : Integer;
  GC : TDisposables;
begin
  LNumOps := 0;
  LList := GC.AddObject(Classes.TList.Create) as Classes.TList;
  LOps := GC.AddObject( TList<TOperationResume>.Create ) as TList<TOperationResume>;
  for LAccNo in AAccountNumbers do begin
    LList.Clear;
    Self.Operations.OperationsHashTree.GetOperationsAffectingAccount(LAccNo, LList);
    if LList.Count > 0 then
      for i := LList.Count - 1 downto 0 do begin
        Inc(LNumOps);
        if (LNumOps > ASkipCount) AND (LNumOps <= ASkipCount + ATakeCount) then begin
          LOp := Self.Operations.OperationsHashTree.GetOperation(PtrInt(LList[i]));
          if TPCOperation.OperationToOperationResume(0, LOp, False, LAccNo, LOpResume) then begin
            LOpResume.NOpInsideBlock := i;
            LOpResume.Block := Node.Operations.OperationBlock.block;
            LOpResume.Balance := Node.Operations.SafeBoxTransaction.Account(LAccNo {Op.SignerAccount}).balance;
            LOps.Add(LOpResume);
          end;
        end;
      end;
  end;
  Result := LOps.ToArray;
end;

function TNodeHelper.GetStoredOperationsAffectingAccounts(const AAccountNumbers : array of Cardinal; ABlockDepth, ASkipCount, ATakeCount : Integer) : TArray<TOperationResume>;
type
  __TList_Cardinal = TList<Cardinal>;
var
  i : Integer;
  LBlock : Cardinal;
  LRelevantBlockOps : Classes.TList;
  LOp : TPCOperation;
  LOpResume : TOperationResume;
  LFoundOps : TList<TOperationResume>;
  LOpsComp : TPCOperationsComp;
  LAccountBalances : TDictionary<Cardinal, Cardinal>;
  LAccounts : TArray<TAccount>;
  LDisposables : TDisposables;
  LBlockEnd, LNumOps : integer;
  LBlockTraversal : TSortedHashSet<Cardinal>;
  LAccountsToScanAtBlock : TObjectDictionary<Cardinal, __TList_Cardinal>;
  LAcc : TAccount;

  procedure MarkAccountAsScannableAtBlock(AAccountNo, ABlockNo : cardinal);
  begin
    if NOT LAccountsToScanAtBlock.ContainsKey(ABlockNo) then
      LAccountsToScanAtBlock.Add(ABlockNo, __TList_Cardinal.Create);
    LAccountsToScanAtBlock[ABlockNo].Add(AAccountNo);
  end;

  procedure ScanBlock(ABlockNum : Cardinal);
  var
    i : integer;
    LAccNo : Cardinal;
    LPrevUpdatedBlock : Cardinal;
    LDisposables : TDisposables;
  begin
    LOpsComp := LDisposables.AddObject( TPCOperationsComp.Create(nil) ) as TPCOperationsComp;
    LRelevantBlockOps := LDisposables.AddObject( Classes.TList.Create ) as Classes.TList;

    // load block
    if not Bank.Storage.LoadBlockChainBlock(LOpsComp, ABlockNum) then begin
      TLog.NewLog(ltdebug, ClassName, 'Block ' + inttostr(ABlockNum)+' not found. Cannot read operations');
      exit;
    end;

    // scan for each account
    for LAccNo in LAccountsToScanAtBlock[ABlockNum] do begin
      LRelevantBlockOps.Clear;
      LOpsComp.OperationsHashTree.GetOperationsAffectingAccount(LAccNo, LRelevantBlockOps);
      for i := LRelevantBlockOps.Count - 1 downto 0 do begin
        LOp := LOpsComp.Operation[PtrInt(LRelevantBlockOps.Items[i])];
        If TPCOperation.OperationToOperationResume(i, LOp, False, LAccNo, LOpResume) then begin
          LOpResume.NOpInsideBlock := LOp.tag; // Note: Used Op.tag to include operation index inside a list
          LOpResume.time := LOpsComp.OperationBlock.timestamp;
          LOpResume.Block := ABlockNum;
          If LAccountBalances[LAccNo] >= 0 then begin
            LOpResume.Balance := LAccountBalances[LAccNo];
            LAccountBalances.AddOrSetValue(LAccNo, LOpResume.Balance - (LOpResume.Amount + LOpResume.Fee));
          end else LOpResume.Balance := -1; // Undetermined

          // Apply skip/take
          inc(LNumOps);
          if (LNumOps > ASkipCount) And (LNumOps <= ASkipCount + ATakeCount) then
            LFoundOps.Add(LOpResume);

          // short-cirtcuit exit if taken enough
          if LFoundOps.Count >= ATakeCount then exit;
        end;
      end;

      // Add previous updated block into traversal set
      LPrevUpdatedBlock := LOpsComp.PreviousUpdatedBlocks.GetPreviousUpdatedBlock(LAccNo, ABlockNum);
      if LPrevUpdatedBlock < ABlockNum then begin
        LBlockTraversal.Add(LPrevUpdatedBlock);
        MarkAccountAsScannableAtBlock(LAccNo, LPrevUpdatedBlock);
      end;
    end;
  end;

  function GetAccountLastUpdateBlock(constref AAccount : TAccount) : Cardinal;
  begin
    Result := AAccount.updated_block;
  end;

begin
  // Init
  LNumOps := 0;
  LBlockTraversal := LDisposables.AddObject( TSortedHashSet<Cardinal>.Create( TComparerTool<Cardinal>.Inverted( TComparer<Cardinal>.Default ) ) ) as TSortedHashSet<Cardinal>;
  LAccountsToScanAtBlock := LDisposables.AddObject( TObjectDictionary<Cardinal, __TList_Cardinal>.Create([doOwnsValues])) as TObjectDictionary<Cardinal, __TList_Cardinal>;
  LFoundOps := LDisposables.AddObject( TList<TOperationResume>.Create ) as TList<TOperationResume>;
  LAccountBalances := LDisposables.AddObject(TDictionary<Cardinal, Cardinal>.Create) as TDictionary<Cardinal, Cardinal>;
  LBlockEnd := ClipValue( Self.BlockTip - ABlockDepth, 0, Self.BlockTip);
  // First get all accounts, their balances and initial traversal set
  LAccounts := Self.GetAccounts(AAccountNumbers, False);
  for i := Low(LAccounts) to High(LAccounts) do begin
    // if account is modified in block-tip
    LAcc := LAccounts[i];
    LAccountBalances.AddOrSetValue(LAcc.account, LAcc.Balance);  // track account balances
    LBlockTraversal.Add(LAcc.updated_block);
    MarkAccountAsScannableAtBlock(LAcc.account, LAcc.updated_block);
  end;

  // Traverse the set of "last updated" blocks in DESCENDING order
  while LBlockTraversal.Count > 0 do begin
    LBlock := TSortedHashSetTool<Cardinal>.Pop( LBlockTraversal );
    if LBlock < LBlockEnd then continue;
    ScanBlock(LBlock);   // note: this will update LBlockTraversals with prev updated blocks, so loops until finished
    if LFoundOps.Count >= ATakeCount then exit;
  end;

  // return array result
  Result := LFoundOps.ToArray;
end;

{ TAccountComparer }

function TAccountComparer.Compare(constref ALeft, ARight: TAccount): integer;
begin
  Result := TAccountComparer.DoCompare(ALeft, ARight);
end;

class function TAccountComparer.DoCompare(constref ALeft, ARight: TAccount): integer;
begin
  Result := TCompare.UInt64(ALeft.account, ARight.account);
end;

{ TAccountEqualityComparer }

function TAccountEqualityComparer.Equals(constref ALeft, ARight: TAccount): boolean;
begin
  Result := TAccountEqualityComparer.AreEqual(ALeft, ARight);
end;

function TAccountEqualityComparer.GetHashCode(constref AValue: TAccount): UInt32;
begin
  Result := TAccountEqualityComparer.CalcHashCode(AValue);
end;

class function TAccountEqualityComparer.AreEqual(constref ALeft, ARight: TAccount): boolean;
begin
  Result :=
    (ALeft.account = ARight.account) and
    (ALeft.balance = ARight.balance) and
    (ALeft.updated_block = ARight.updated_block) and
    (ALeft.n_operation = ARight.n_operation) and
    TAccountKeyEqualityComparer.AreEqual(ALeft.accountInfo.accountKey, ARight.accountInfo.accountKey);
end;

class function TAccountEqualityComparer.CalcHashCode(constref AValue: TAccount): UInt32;
begin
  Result := AValue.account;
end;

{ TAccountKeyComparer }

function TAccountKeyComparer.Compare(constref ALeft, ARight: T): integer;
begin
  Result := TAccountKeyComparer.DoCompare(ALeft, ARight);
end;

class function TAccountKeyComparer.DoCompare(constref ALeft, ARight: TAccountKey): integer;
begin
  Result := BinStrComp(ALeft.x, ARight.x);
  if Result = 0 then
    Result := BinStrComp(ALeft.y, ARight.y);
end;

{ TAccountKeyEqualityComparer }

function TAccountKeyEqualityComparer.Equals(constref ALeft, ARight: TAccountKey): boolean;
begin
  Result := TAccountKeyEqualityComparer.AreEqual(ALeft, ARight);
end;

function TAccountKeyEqualityComparer.GetHashCode(constref AValue: TAccountKey): UInt32;
begin
  Result := TAccountKeyEqualityComparer.CalcHashCode(AValue);
end;

class function TAccountKeyEqualityComparer.AreEqual(constref ALeft, ARight: TAccountKey): boolean;
begin
  Result := TAccountKeyComparer.DoCompare(ALeft, ARight) = 0;
end;

class function TAccountKeyEqualityComparer.CalcHashCode(constref AValue: TAccountKey): UInt32;
begin
  Result := TEqualityComparer<ansistring>.Default.GetHashCode(IntToStr(AValue.EC_OpenSSL_NID) + AValue.x + AValue.y);
end;

{ TAccountHelper }

function TAccountHelper.GetAccountString: ansistring;
begin
  Result := TAccountComp.AccountNumberToAccountTxtNumber(Self.account);
end;

function TAccountHelper.GetDisplayString: ansistring;
begin
  Result := GetAccountString;
  if Self.Name <> '' then
    Result := Result + ': ' + Self.Name;
end;

function TAccountHelper.GetInfoText(const ABank: TPCBank): utf8string;
var
  builder: TStrings;
  GC: TDisposables;
begin
  builder := GC.AddObject(TStringList.Create) as TStrings;
  builder.Append(Format('Account: %s %s Type:%d', [TAccountComp.AccountNumberToAccountTxtNumber(self.account), IIF(Self.Name <> '', 'Name: ' + Self.Name, ''), Self.account_type]));
  builder.Append('');
  builder.Append(Format('Current balance: %s', [TAccountComp.FormatMoney(Self.balance)]));
  builder.Append('');
  builder.Append(Format('Updated on block: %d  (%d blocks ago)', [Self.updated_block, ABank.BlocksCount - Self.updated_block]));
  builder.Append(Format('Public key type: %s', [TAccountComp.GetECInfoTxt(Self.accountInfo.accountKey.EC_OpenSSL_NID)]));
  builder.Append(Format('Base58 Public key: %s', [TAccountComp.AccountPublicKeyExport(Self.accountInfo.accountKey)]));
  if TAccountComp.IsAccountForSale(Self.accountInfo) then
  begin
    builder.Append('');
    builder.Append('** Account is for sale: **');
    builder.Append(Format('Price: %s', [TAccountComp.FormatMoney(Self.accountInfo.price)]));
    builder.Append(Format('Seller account (where to pay): %s', [TAccountComp.AccountNumberToAccountTxtNumber(Self.accountInfo.account_to_pay)]));
    if TAccountComp.IsAccountForSaleAcceptingTransactions(Self.accountInfo) then
    begin
      builder.Append('');
      builder.Append('** Private sale **');
      builder.Append(Format('New Base58 Public key: %s', [TAccountComp.AccountPublicKeyExport(Self.accountInfo.new_publicKey)]));
      builder.Append('');
      if TAccountComp.IsAccountLocked(Self.accountInfo, ABank.BlocksCount) then
        builder.Append(Format('PURCHASE IS SECURE UNTIL BLOCK %d (current %d, remains %d)',
          [Self.accountInfo.locked_until_block, ABank.BlocksCount, Self.accountInfo.locked_until_block - ABank.BlocksCount]))
      else
        builder.Append(Format('PURCHASE IS NOT SECURE (Expired on block %d, current %d)',
          [Self.accountInfo.locked_until_block, ABank.BlocksCount]));
    end;
  end;
  Result := builder.Text;
end;

{ TOperationResumeHelper }

function TOperationResumeHelper.GetPrintableOPHASH: ansistring;
begin
  Result := TCrypto.ToHexaString(Self.OperationHash);
end;

function TOperationResumeHelper.GetInfoText(const ABank: TPCBank): utf8string;
var
  builder: TStrings;
  GC: TDisposables;
begin
  if (not Self.valid) then
    exit;
  builder := GC.AddObject(TStringList.Create) as TStrings;
  if Self.Block < ABank.BlocksCount then
    if (Self.NOpInsideBlock >= 0) then
      builder.Add(Format('Block: %d/%d', [Self.Block, Self.NOpInsideBlock]))
    else
    begin
      builder.Add(Format('Block: %d', [Self.Block]));
    end
  else
    builder.Add('** Pending operation not included on blockchain **');
  builder.Add(Format('%s', [Self.OperationTxt]));
  builder.Add(Format('OpType:%d Subtype:%d', [Self.OpType, Self.OpSubtype]));
  builder.Add(Format('Operation Hash (ophash): %s', [TCrypto.ToHexaString(Self.OperationHash)]));
  if (Self.OperationHash_OLD <> '') then
    builder.Add(Format('Old Operation Hash (old_ophash): %s', [TCrypto.ToHexaString(Self.OperationHash_OLD)]));
  if (Self.OriginalPayload <> '') then
  begin
    builder.Add(Format('Payload length:%d', [length(Self.OriginalPayload)]));
    if Self.PrintablePayload <> '' then
      builder.Add(Format('Payload (human): %s', [Self.PrintablePayload]));
    builder.Add(Format('Payload (Hexadecimal): %s', [TCrypto.ToHexaString(Self.OriginalPayload)]));
  end;
  if Self.Balance >= 0 then
    builder.Add(Format('Final balance: %s', [TAccountComp.FormatMoney(Self.Balance)]));
  Result := builder.Text;
end;

{ TTimeSpanHelper }

function TTimeSpanHelper.TotalBlockCount: integer;
begin
  Result := Round(Self.TotalSeconds / CT_NewLineSecondsAvg);
end;

end.
