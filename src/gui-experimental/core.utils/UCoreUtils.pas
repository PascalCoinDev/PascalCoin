unit UCoreUtils;

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  Acknowledgements:
  - Ugochukwu Mmaduekwe - added TOperationsManager class

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$mode delphi}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, Forms, Dialogs, LCLType, UAccounts, UBlockChain, UNode, UWallet,
  UBaseTypes, UCommon, UCoreObjects, UCommon.Collections, Generics.Defaults;

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
    class function AreAccountBalancesGreaterThan(const ACandidates: array of TAccount; AAmount: int64; var AFaultyAccount: TAccount): boolean; static;
  end;

  { TNodeHelper }

  TNodeHelper = class helper for TNode
    function HasBestKnownBlockchainTip: boolean;
    function BlockTip: cardinal;
    function GetAccount(AAccountNumber: cardinal; AIncludePending: boolean = True): TAccount;
    function GetAccounts(const AAccountNumbers: array of cardinal; AIncludePending: boolean = True): TArray<TAccount>;
    function GetPendingOperationsAffectingAccounts(const AAccountNumbers: array of cardinal; ASkipCount, ATakeCount: integer): TArray<TOperationResume>;
    function GetStoredOperationsAffectingAccounts(const AAccountNumbers: array of cardinal; ABlockDepth, ASkipCount, ATakeCount: integer): TArray<TOperationResume>;
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

  { TWIZOperationsHelper }

  TWIZOperationsHelper = class
  private
    class function IsOwnerOfWallet(AAccount: TAccount; AWalletKeys: TWalletKeys; out AWalletKey: TWalletKey; var AErrorMessage: string): boolean; static;
    class function ValidateOperationsInput(const ASelectedAccounts: TArray<TAccount>; AWalletKeys: TWalletKeys; ANode: TNode; var AErrorMessage: string): boolean; static;
    class function UpdatePayload(const ASenderPublicKey, ADestinationPublicKey: TAccountKey; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent: string; var AEncodedPayloadBytes: TRawBytes; const APayloadEncryptionPassword: string; var AErrorMessage: string): boolean; static;
    class function SendPASCFinalizeAndDisplayMessage(const AOperationsTxt, AOperationToString: string; ANoOfOperations: integer; ATotalAmount, ATotalFee: int64; AOperationsHashTree: TOperationsHashTree; var AErrorMessage: string): boolean; static;
    class function OthersFinalizeAndDisplayMessage(const AOperationsTxt, AOperationToString: string; ANoOfOperations: integer; ATotalFee: int64; AOperationsHashTree: TOperationsHashTree; var AErrorMessage: string): boolean; static;
  public
    class function ExecuteSendPASC(const ASelectedAccounts: TArray<TAccount>; const ADestinationAccount: TAccount; AAmount, AFee: int64; const ASendPASCMode: TSendPASCMode; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string): boolean; static;
    class function ExecuteChangeKey(const ASelectedAccounts: TArray<TAccount>; const ASignerAccount: TAccount; APublicKey: TAccountKey; AFee: int64; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string): boolean; static;
    class function ExecuteEnlistAccountForSale(const ASelectedAccounts: TArray<TAccount>; const ASignerAccount, ASellerAccount: TAccount; const APublicKey: TAccountKey; AFee, ASalePrice: int64; ALockedUntilBlock: UInt32; const AAccountSaleMode: TAccountSaleMode; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string): boolean; static;
    class function ExecuteDelistAccountFromSale(const ASelectedAccounts: TArray<TAccount>; const ASignerAccount: TAccount; AFee: int64; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string): boolean; static;
    class function ExecuteChangeAccountInfo(const ASelectedAccounts, ASignerAccounts: TArray<TAccount>; AFee: int64; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; const ANewName: TRawBytes; const ANewType: word; var AErrorMessage: string): boolean; static;
    class function ExecuteBuyAccount(const ASelectedAccounts: TArray<TAccount>; const AAccountToBuy: TAccount; AFee: int64; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; const AAmount: int64; const ANewOwnerPublicKey: TAccountKey; var AErrorMessage: string): boolean; static;
  end;


implementation

uses
  ULog,
  UAES,
  UConst,
  UECIES,
  UCrypto,
  UMemory,
  UNetProtocol,
  UOpTransaction,
  UPCOrderedLists,
  Generics.Collections;

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
  LBalance: TBalanceSummary;
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
    for i := 0 to TWallet.Keys.Count - 1 do
    begin
      LList := TWallet.Keys.AccountsKeyList.AccountKeyList[i];
      for j := 0 to LList.Count - 1 do
      begin
        if IncludePending then
          LAcc := TNode.Node.Operations.SafeBoxTransaction.Account(LList.Get(j))
        else
          LAcc := TNode.Node.Bank.SafeBox.Account(LList.Get(j));
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

class function TCoreTool.AreAccountBalancesGreaterThan(const ACandidates: array of TAccount; AAmount: int64; var AFaultyAccount: TAccount): boolean;
var
  LIdx: integer;
  LAcc: TAccount;
begin
  Result := True;
  for LIdx := Low(ACandidates) to High(ACandidates) do
  begin
    LAcc := ACandidates[LIdx];
    if not (LAcc.Balance > AAmount) then
    begin
      AFaultyAccount := LAcc;
      Exit(False);
    end;
  end;
end;


{ TNodeHelper }

function TNodeHelper.HasBestKnownBlockchainTip: boolean;
var
  LReady: boolean;
  LMsg: ansistring;
  LDestBlock: cardinal;
begin
  LReady := Self.Bank.IsReady(LMsg);
  if LReady and TNetData.NetData.IsGettingNewBlockChainFromClient(LMsg) then
  begin
    LDestBlock := TNetData.NetData.MaxRemoteOperationBlock.block;
    Result := Self.Bank.BlocksCount = TNetData.NetData.MaxRemoteOperationBlock.block;
  end;
end;

function TNodeHelper.BlockTip: cardinal;
begin
  Result := ClipValue(Self.Bank.BlocksCount - 1, 0, MaxInt);
end;

function TNodeHelper.GetAccount(AAccountNumber: cardinal; AIncludePending: boolean = True): TAccount;
var
  LOps: TArray<TAccount>;
begin
  LOps := Self.GetAccounts([AAccountNumber], AIncludePending);
  Result := LOps[Low(Lops)];
end;

function TNodeHelper.GetAccounts(const AAccountNumbers: array of cardinal; AIncludePending: boolean = True): TArray<TAccount>;
var
  i: integer;
begin
  SetLength(Result, Length(AAccountNumbers));
  if AIncludePending then
    for i := Low(AAccountNumbers) to High(AAccountNumbers) do
      Result[i] := Self.Operations.SafeBoxTransaction.Account(AAccountNumbers[i])
  else
    for i := Low(AAccountNumbers) to High(AAccountNumbers) do
      Result[i] := Self.Bank.SafeBox.Account(AAccountNumbers[i]);
end;

function TNodeHelper.GetPendingOperationsAffectingAccounts(const AAccountNumbers: array of cardinal; ASkipCount, ATakeCount: integer): TArray<TOperationResume>;
var
  LList: Classes.TList;
  LOps: TList<TOperationResume>;
  LOp: TPCOperation;
  LOpResume: TOperationResume;
  LAccNo: cardinal;
  LNumOps, i: integer;
  GC: TDisposables;
begin
  LNumOps := 0;
  LList := GC.AddObject(Classes.TList.Create) as Classes.TList;
  LOps := GC.AddObject(TList<TOperationResume>.Create) as TList<TOperationResume>;
  for LAccNo in AAccountNumbers do
  begin
    LList.Clear;
    Self.Operations.OperationsHashTree.GetOperationsAffectingAccount(LAccNo, LList);
    if LList.Count > 0 then
      for i := LList.Count - 1 downto 0 do
      begin
        Inc(LNumOps);
        if (LNumOps > ASkipCount) and (LNumOps <= ASkipCount + ATakeCount) then
        begin
          LOp := Self.Operations.OperationsHashTree.GetOperation(PtrInt(LList[i]));
          if TPCOperation.OperationToOperationResume(0, LOp, False, LAccNo, LOpResume) then
          begin
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

function TNodeHelper.GetStoredOperationsAffectingAccounts(const AAccountNumbers: array of cardinal; ABlockDepth, ASkipCount, ATakeCount: integer): TArray<TOperationResume>;
type
  __TList_Cardinal = TList<cardinal>;
var
  i: integer;
  LBlock: cardinal;
  LRelevantBlockOps: Classes.TList;
  LOp: TPCOperation;
  LOpResume: TOperationResume;
  LFoundOps: TList<TOperationResume>;
  LOpsComp: TPCOperationsComp;
  LAccountBalances: TDictionary<cardinal, cardinal>;
  LAccounts: TArray<TAccount>;
  LDisposables: TDisposables;
  LBlockEnd, LNumOps: integer;
  LBlockTraversal: TSortedHashSet<cardinal>;
  LAccountsToScanAtBlock: TObjectDictionary<cardinal, __TList_Cardinal>;
  LAcc: TAccount;

  procedure MarkAccountAsScannableAtBlock(AAccountNo, ABlockNo: cardinal);
  begin
    if not LAccountsToScanAtBlock.ContainsKey(ABlockNo) then
      LAccountsToScanAtBlock.Add(ABlockNo, __TList_Cardinal.Create);
    LAccountsToScanAtBlock[ABlockNo].Add(AAccountNo);
  end;

  procedure ScanBlock(ABlockNum: cardinal);
  var
    i: integer;
    LAccNo: cardinal;
    LPrevUpdatedBlock: cardinal;
    LDisposables: TDisposables;
  begin
    LOpsComp := LDisposables.AddObject(TPCOperationsComp.Create(nil)) as TPCOperationsComp;
    LRelevantBlockOps := LDisposables.AddObject(Classes.TList.Create) as Classes.TList;

    // load block
    if not Bank.Storage.LoadBlockChainBlock(LOpsComp, ABlockNum) then
    begin
      TLog.NewLog(ltdebug, ClassName, 'Block ' + IntToStr(ABlockNum) + ' not found. Cannot read operations');
      exit;
    end;

    // scan for each account
    for LAccNo in LAccountsToScanAtBlock[ABlockNum] do
    begin
      LRelevantBlockOps.Clear;
      LOpsComp.OperationsHashTree.GetOperationsAffectingAccount(LAccNo, LRelevantBlockOps);
      for i := LRelevantBlockOps.Count - 1 downto 0 do
      begin
        LOp := LOpsComp.Operation[PtrInt(LRelevantBlockOps.Items[i])];
        if TPCOperation.OperationToOperationResume(i, LOp, False, LAccNo, LOpResume) then
        begin
          LOpResume.NOpInsideBlock := PtrInt(LRelevantBlockOps.Items[i]);
          LOpResume.time := LOpsComp.OperationBlock.timestamp;
          LOpResume.Block := ABlockNum;
          if LAccountBalances[LAccNo] >= 0 then
          begin
            LOpResume.Balance := LAccountBalances[LAccNo];
            LAccountBalances.AddOrSetValue(LAccNo, LOpResume.Balance - (LOpResume.Amount + LOpResume.Fee));
          end
          else
            LOpResume.Balance := -1; // Undetermined

          // Apply skip/take
          Inc(LNumOps);
          if (LNumOps > ASkipCount) and (LNumOps <= ASkipCount + ATakeCount) then
            LFoundOps.Add(LOpResume);

          // short-cirtcuit exit if taken enough
          if LFoundOps.Count >= ATakeCount then
            exit;
        end;
      end;

      // Add previous updated block into traversal set
      LPrevUpdatedBlock := LOpsComp.PreviousUpdatedBlocks.GetPreviousUpdatedBlock(LAccNo, ABlockNum);
      if LPrevUpdatedBlock < ABlockNum then
      begin
        LBlockTraversal.Add(LPrevUpdatedBlock);
        MarkAccountAsScannableAtBlock(LAccNo, LPrevUpdatedBlock);
      end;
    end;
  end;

  function GetAccountLastUpdateBlock(constref AAccount: TAccount): cardinal;
  begin
    Result := AAccount.updated_block;
  end;

begin
  // Init
  LNumOps := 0;
  LBlockTraversal := LDisposables.AddObject(TSortedHashSet<cardinal>.Create(TComparerTool<cardinal>.Inverted(TComparer<cardinal>.Default))) as TSortedHashSet<cardinal>;
  LAccountsToScanAtBlock := LDisposables.AddObject(TObjectDictionary<cardinal, __TList_Cardinal>.Create([doOwnsValues])) as TObjectDictionary<cardinal, __TList_Cardinal>;
  LFoundOps := LDisposables.AddObject(TList<TOperationResume>.Create) as TList<TOperationResume>;
  LAccountBalances := LDisposables.AddObject(TDictionary<cardinal, cardinal>.Create) as TDictionary<cardinal, cardinal>;
  LBlockEnd := ClipValue(Self.BlockTip - ABlockDepth, 0, Self.BlockTip);
  // First get all accounts, their balances and initial traversal set
  LAccounts := Self.GetAccounts(AAccountNumbers, False);
  for i := Low(LAccounts) to High(LAccounts) do
  begin
    // if account is modified in block-tip
    LAcc := LAccounts[i];
    LAccountBalances.AddOrSetValue(LAcc.account, LAcc.Balance);  // track account balances
    LBlockTraversal.Add(LAcc.updated_block);
    MarkAccountAsScannableAtBlock(LAcc.account, LAcc.updated_block);
  end;

  // Traverse the set of "last updated" blocks in DESCENDING order
  while LBlockTraversal.Count > 0 do
  begin
    LBlock := TSortedHashSetTool<cardinal>.Pop(LBlockTraversal);
    if LBlock < LBlockEnd then
      continue;
    ScanBlock(LBlock);   // note: this will update LBlockTraversals with prev updated blocks, so loops until finished
    if LFoundOps.Count >= ATakeCount then
      exit;
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
  Result := BytesCompare(ALeft.x, ARight.x);
  if Result = 0 then
    Result := BytesCompare(ALeft.y, ARight.y);
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
  Result := TEqualityComparer<ansistring>.Default.GetHashCode(IntToStr(AValue.EC_OpenSSL_NID) + AValue.x.ToString + AValue.y.ToString);
end;

{ TAccountHelper }

function TAccountHelper.GetAccountString: ansistring;
begin
  Result := TAccountComp.AccountNumberToAccountTxtNumber(Self.account);
end;

function TAccountHelper.GetDisplayString: ansistring;
begin
  Result := GetAccountString;
  if Self.Name <> nil then
    Result := Result + ': ' + Self.Name.ToString;
end;

function TAccountHelper.GetInfoText(const ABank: TPCBank): utf8string;
var
  builder: TStrings;
  GC: TDisposables;
begin
  builder := GC.AddObject(TStringList.Create) as TStrings;
  builder.Append(Format('Account: %s %s Type:%d', [TAccountComp.AccountNumberToAccountTxtNumber(self.account), IIF(Self.Name <> nil, 'Name: ' + Self.Name.ToString, ''), Self.account_type]));
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
  if (Self.OperationHash_OLD <> nil) then
    builder.Add(Format('Old Operation Hash (old_ophash): %s', [TCrypto.ToHexaString(Self.OperationHash_OLD)]));
  if (Self.OriginalPayload <> nil) then
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

{ TWIZOperationsHelper }

class function TWIZOperationsHelper.ValidateOperationsInput(const ASelectedAccounts: TArray<TAccount>; AWalletKeys: TWalletKeys; ANode: TNode; var AErrorMessage: string): boolean;
begin
  Result := True;
  if Length(ASelectedAccounts) = 0 then
  begin
    AErrorMessage := 'No Selected Account Found';
    Exit(False);
  end;

  if not Assigned(AWalletKeys) then
  begin
    AErrorMessage := 'No Wallet Keys Found';
    Exit(False);
  end;

  if not Assigned(ANode) then
  begin
    AErrorMessage := 'No Node Found';
    Exit(False);
  end;
end;

class function TWIZOperationsHelper.IsOwnerOfWallet(AAccount: TAccount; AWalletKeys: TWalletKeys; out AWalletKey: TWalletKey; var AErrorMessage: string): boolean;
var
  LIdx: Int32;
begin
  Result := True;
  LIdx := AWalletKeys.IndexOfAccountKey(AAccount.accountInfo.accountKey);
  if LIdx < 0 then
  begin
    AErrorMessage := Format('Selected Account "%s" Private Key Not Found In Wallet', [AAccount.AccountString]);
    Exit(False);
  end;
  AWalletKey := AWalletKeys.Key[LIdx];

  if not Assigned(AWalletKey.PrivateKey) then
  begin
    if AWalletKey.HasPrivateKey then
      AErrorMessage := 'Wallet is Password Protected. Please Unlock Before You Proceed.'
    else
      AErrorMessage := Format('Only Public Key of Account %s Was Found in Wallet. You Cannot Operate This Account', [AAccount.AccountString]);
    Exit(False);
  end;
end;

class function TWIZOperationsHelper.SendPASCFinalizeAndDisplayMessage(const AOperationsTxt, AOperationToString: string; ANoOfOperations: integer; ATotalAmount, ATotalFee: int64; AOperationsHashTree: TOperationsHashTree; var AErrorMessage: string): boolean;
var
  LAuxs, LOperationsTxt: string;
  i: integer;
begin
  LOperationsTxt := AOperationsTxt;
  if (ANoOfOperations > 1) then
  begin
    LAuxs := 'Total amount that dest will receive: ' + TAccountComp.FormatMoney(
      ATotalAmount) + #10;
    if Application.MessageBox(
      PChar('Execute ' + IntToStr(ANoOfOperations) +
      ' operations?' + #10 + 'Operation: ' + LOperationsTxt + #10 +
      LAuxs + 'Total fee: ' + TAccountComp.FormatMoney(ATotalFee) +
      #10 + #10 + 'Note: This operation will be transmitted to the network!'),
      PChar(Application.Title), MB_YESNO + MB_ICONINFORMATION + MB_DEFBUTTON2) <>
      idYes then
      Exit;
  end
  else
  if Application.MessageBox(PChar('Execute this operation:' +
    #10 + #10 + AOperationToString + #10 + #10 +
    'Note: This operation will be transmitted to the network!'),
    PChar(Application.Title), MB_YESNO + MB_ICONINFORMATION + MB_DEFBUTTON2) <>
    idYes then
    Exit;
  Result := True;
  i := TNode.Node.AddOperations(nil, AOperationsHashTree, nil, AErrorMessage);
  if (i = AOperationsHashTree.OperationsCount) then
  begin
    LOperationsTxt := 'Successfully executed ' + IntToStr(i) +
      ' operations!' + #10 + #10 + AOperationToString;
    if i > 1 then
      ShowMessage(LOperationsTxt)
    else
    begin
      Application.MessageBox(
        PChar('Successfully executed ' + IntToStr(i) + ' operations!' +
        #10 + #10 + AOperationToString),
        PChar(Application.Title), MB_OK + MB_ICONINFORMATION);
    end;
  end
  else if (i > 0) then
  begin
    LOperationsTxt := 'One or more of your operations has not been executed:' +
      #10 + 'Errors:' + #10 + AErrorMessage + #10 + #10 +
      'Total successfully executed operations: ' + IntToStr(i);
    ShowMessage(LOperationsTxt);
  end
  else
    Result := False;
end;

class function TWIZOperationsHelper.UpdatePayload(const ASenderPublicKey, ADestinationPublicKey: TAccountKey; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent: string; var AEncodedPayloadBytes: TRawBytes; const APayloadEncryptionPassword: string; var AErrorMessage: string): boolean;
var
  LValid: boolean;
begin

  if (APayloadContent = '') then
    Exit(True);

  LValid := False;
  AErrorMessage := 'An Error Occured During Payload Encryption.';

  try

    case APayloadEncryptionMode of

      pemEncryptWithSender:
      begin
        // Use sender public key
        AEncodedPayloadBytes := ECIESEncrypt(ASenderPublicKey, TEncoding.ANSI.GetBytes(APayloadContent));
        LValid := AEncodedPayloadBytes <> nil;
      end;

      pemEncryptWithRecipient:
      begin
        // With destination public key
        AEncodedPayloadBytes := ECIESEncrypt(ADestinationPublicKey, TEncoding.ANSI.GetBytes(APayloadContent));
        LValid := AEncodedPayloadBytes <> nil;
      end;

      pemEncryptWithPassword:
      begin
        // With defined password
        if APayloadEncryptionPassword = '' then
        begin
          AErrorMessage := 'Payload Encryption Password Cannot Be Empty With The Chosen Option : "Encrypt With Password."';
          Exit(False);
        end;
        AEncodedPayloadBytes := TAESComp.EVP_Encrypt_AES256(
          TEncoding.ANSI.GetBytes(APayloadContent), TEncoding.ANSI.GetBytes(APayloadEncryptionPassword));
        LValid := AEncodedPayloadBytes <> nil;
      end;

      pemNotEncrypt:
      begin
        // no encryption
        AEncodedPayloadBytes := TEncoding.ANSI.GetBytes(APayloadContent);
        LValid := True;
      end

      else
      begin
        AErrorMessage := 'Unknown Encryption Selected';
        Exit(False);
      end;
    end;

  finally
    if LValid then
      if Length(AEncodedPayloadBytes) > CT_MaxPayloadSize then
      begin
        LValid := False;
        AErrorMessage := Format('Payload Size Is %d Which Is Bigger Than %d', [Length(AEncodedPayloadBytes), CT_MaxPayloadSize]);
      end;
    Result := LValid;
  end;
end;

class function TWIZOperationsHelper.OthersFinalizeAndDisplayMessage(const AOperationsTxt, AOperationToString: string; ANoOfOperations: integer; ATotalFee: int64; AOperationsHashTree: TOperationsHashTree; var AErrorMessage: string): boolean;
var
  LAuxs, LOperationsTxt: string;
  i: integer;
begin
  LOperationsTxt := AOperationsTxt;
  if (ANoOfOperations > 1) then
  begin
    LAuxs := '';
    if Application.MessageBox(
      PChar('Execute ' + IntToStr(ANoOfOperations) +
      ' operations?' + #10 + 'Operation: ' + LOperationsTxt + #10 +
      LAuxs + 'Total fee: ' + TAccountComp.FormatMoney(ATotalFee) +
      #10 + #10 + 'Note: This operation will be transmitted to the network!'),
      PChar(Application.Title), MB_YESNO + MB_ICONINFORMATION + MB_DEFBUTTON2) <>
      idYes then
      Exit;
  end
  else
  if Application.MessageBox(PChar('Execute this operation:' +
    #10 + #10 + AOperationToString + #10 + #10 +
    'Note: This operation will be transmitted to the network!'),
    PChar(Application.Title), MB_YESNO + MB_ICONINFORMATION + MB_DEFBUTTON2) <>
    idYes then
    Exit;
  Result := True;
  i := TNode.Node.AddOperations(nil, AOperationsHashTree, nil, AErrorMessage);
  if (i = AOperationsHashTree.OperationsCount) then
  begin
    LOperationsTxt := 'Successfully executed ' + IntToStr(i) +
      ' operations!' + #10 + #10 + AOperationToString;
    if i > 1 then
      ShowMessage(LOperationsTxt)
    else
    begin
      Application.MessageBox(
        PChar('Successfully executed ' + IntToStr(i) + ' operations!' +
        #10 + #10 + AOperationToString),
        PChar(Application.Title), MB_OK + MB_ICONINFORMATION);
    end;
  end
  else if (i > 0) then
  begin
    LOperationsTxt := 'One or more of your operations has not been executed:' +
      #10 + 'Errors:' + #10 + AErrorMessage + #10 + #10 +
      'Total successfully executed operations: ' + IntToStr(i);
    ShowMessage(LOperationsTxt);
  end
  else
    Result := False;
end;

class function TWIZOperationsHelper.ExecuteSendPASC(const ASelectedAccounts: TArray<TAccount>; const ADestinationAccount: TAccount; AAmount, AFee: int64; const ASendPASCMode: TSendPASCMode; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string): boolean;
var
  LWalletKey: TWalletKey;
  LWalletKeys: TWalletKeys;
  LNode: TNode;
  LPCOperation: TPCOperation;
  LOperationsHashTree: TOperationsHashTree;
  LTotalAmount, LTotalSignerFee, LAmount, LFee: int64;
  LDoOperation: boolean;
  LOperationsTxt, LOperationToString, LTemp: string;
  LAccountIdx, LNoOfOperations: integer;
  LCurrentAccount: TAccount;
  LPayloadEncodedBytes: TRawBytes;
begin

  LWalletKeys := TWallet.Keys;
  LNode := TNode.Node;

  if not TWIZOperationsHelper.ValidateOperationsInput(ASelectedAccounts, LWalletKeys, LNode, AErrorMessage) then
    Exit(False);

  LOperationsHashTree := TOperationsHashTree.Create;
  try
    LTotalAmount := 0;
    LTotalSignerFee := 0;
    LNoOfOperations := 0;
    LOperationsTxt := '';
    LOperationToString := '';
    for LAccountIdx := Low(ASelectedAccounts) to High(ASelectedAccounts) do
    begin
      LPCOperation := nil; // reset LPCOperation to Nil
      LCurrentAccount := ASelectedAccounts[LAccountIdx];

      if not TWIZOperationsHelper.IsOwnerOfWallet(LCurrentAccount, LWalletKeys, LWalletKey, AErrorMessage) then
        Exit(False);

      if LCurrentAccount.account = ADestinationAccount.account then
      begin
        AErrorMessage := Format('Sender "%s" And Destination "%s" Accounts Are The Same', [LCurrentAccount.AccountString, ADestinationAccount.AccountString]);
        Exit(False);
      end;

      if not UpdatePayload(LCurrentAccount.accountInfo.accountKey, ADestinationAccount.accountInfo.accountKey, APayloadEncryptionMode, APayloadContent, LPayloadEncodedBytes, APayloadEncryptionPassword, AErrorMessage) then
      begin
        AErrorMessage := Format('Error Encoding Payload Of Selected Account "%s. ", Specific Error Is "%s"', [LCurrentAccount.AccountString, AErrorMessage]);
        Exit(False);
      end;

      LDoOperation := True;

      case ASendPASCMode of
        akaAllBalance:
          if LCurrentAccount.balance > 0 then
          begin
            if LCurrentAccount.balance > AFee then
            begin
              LAmount := LCurrentAccount.balance - AFee;
              LFee := AFee;
            end
            else
            begin
              LAmount := LCurrentAccount.balance;
              LFee := 0;
            end;
          end
          else
            LDoOperation := False;

        akaSpecifiedAmount:
          if LCurrentAccount.balance > UInt64(AFee) then
            LFee := AFee
          else
            LFee := LCurrentAccount.balance;
      end;

      if LDoOperation then
      begin
        LPCOperation := TOpTransaction.CreateTransaction(
          TNode.Node.Bank.Safebox.CurrentProtocol, LCurrentAccount.account, LCurrentAccount.n_operation + 1, ADestinationAccount.account, LWalletKey.PrivateKey, LAmount, LFee, LPayloadEncodedBytes);
        try
          LTemp := Format('%d. Transfer of %s PASC from %s to %s %s', [LNoOfOperations + 1, TAccountComp.FormatMoney(LAmount), LCurrentAccount.AccountString, ADestinationAccount.AccountString, sLineBreak]);
          if LOperationsTxt <> '' then
            LOperationsTxt := LOperationsTxt + LTemp + sLineBreak
          else
            LOperationsTxt := sLineBreak + LTemp;

          if Assigned(LPCOperation) then
          begin
            LOperationsHashTree.AddOperationToHashTree(LPCOperation);
            Inc(LTotalAmount, LAmount);
            Inc(LTotalSignerFee, LFee);
            Inc(LNoOfOperations);
            if LOperationToString <> '' then
              LOperationToString := LOperationToString + #10;
            LOperationToString := LOperationToString + LPCOperation.ToString;
          end;
        finally
          FreeAndNil(LPCOperation);
        end;
      end;

    end;
    if (LOperationsHashTree.OperationsCount = 0) then
    begin
      AErrorMessage := 'No Valid Operation To Execute';
      Exit(False);
    end;

    Exit(TWIZOperationsHelper.SendPASCFinalizeAndDisplayMessage(LOperationsTxt, LOperationToString, LNoOfOperations, LTotalAmount, LTotalSignerFee, LOperationsHashTree, AErrorMessage));
  finally
    LOperationsHashTree.Free;
  end;

end;

class function TWIZOperationsHelper.ExecuteChangeKey(const ASelectedAccounts: TArray<TAccount>; const ASignerAccount: TAccount; APublicKey: TAccountKey; AFee: int64; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string): boolean;
var
  LWalletKey: TWalletKey;
  LWalletKeys: TWalletKeys;
  LNode: TNode;
  LPCOperation: TPCOperation;
  LOperationsHashTree: TOperationsHashTree;
  LTotalSignerFee, LFee: int64;
  LIsV2: boolean;
  LOperationsTxt, LOperationToString, LTemp: string;
  LAccountIdx, LNoOfOperations: integer;
  LCurrentAccount, LSignerAccount: TAccount;
  LPayloadEncodedBytes: TRawBytes;
label
  loop_start;
begin

  LWalletKeys := TWallet.Keys;
  LNode := TNode.Node;
  LSignerAccount := ASignerAccount;

  if not TWIZOperationsHelper.ValidateOperationsInput(ASelectedAccounts, LWalletKeys, LNode, AErrorMessage) then
    Exit(False);

  LOperationsHashTree := TOperationsHashTree.Create;
  try
    LIsV2 := LNode.Bank.SafeBox.CurrentProtocol >= CT_PROTOCOL_2;
    LTotalSignerFee := 0;
    LNoOfOperations := 0;
    LOperationsTxt := '';
    LOperationToString := '';
    for LAccountIdx := Low(ASelectedAccounts) to High(ASelectedAccounts) do
    begin
      loop_start:
        LPCOperation := nil; // reset LPCOperation to Nil
      LCurrentAccount := ASelectedAccounts[LAccountIdx];

      if not TWIZOperationsHelper.IsOwnerOfWallet(LCurrentAccount, LWalletKeys, LWalletKey, AErrorMessage) then
        Exit(False);

      if (TAccountComp.EqualAccountKeys(LCurrentAccount.accountInfo.accountKey,
        APublicKey)) then
      begin
        AErrorMessage := 'New Key Is Same As Current Key';
        Exit(False);
      end;

      if LNode.Bank.SafeBox.CurrentProtocol >= CT_PROTOCOL_1 then
      begin
        // Signer:
        LSignerAccount := ASignerAccount;
        if (TAccountComp.IsAccountLocked(LSignerAccount.accountInfo,
          LNode.Bank.BlocksCount)) then
        begin
          AErrorMessage := Format('Signer Account "%s"  Is Locked Until Block %u', [LSignerAccount.AccountString, LSignerAccount.accountInfo.locked_until_block]);
          Exit(False);
        end;
        if (not TAccountComp.EqualAccountKeys(
          LSignerAccount.accountInfo.accountKey, LCurrentAccount.accountInfo.accountKey)) then
        begin
          AErrorMessage := Format('Signer Account %s Is Not The Owner Of Account %s', [LSignerAccount.AccountString, LCurrentAccount.AccountString]);
          Exit(False);
        end;
      end
      else
        LSignerAccount := LCurrentAccount;

      if not UpdatePayload(LCurrentAccount.accountInfo.accountKey, APublicKey, APayloadEncryptionMode, APayloadContent, LPayloadEncodedBytes, APayloadEncryptionPassword, AErrorMessage) then
      begin
        AErrorMessage := Format('Error Encoding Payload Of Selected Account "%s. ", Specific Error Is "%s"', [LCurrentAccount.AccountString, AErrorMessage]);
        Exit(False);
      end;

      if LIsV2 then
      begin
        // must ensure is Signer account last if included in sender accounts (not necessarily ordered enumeration)
        if (LAccountIdx < Length(ASelectedAccounts) - 1) and
          (LCurrentAccount.account = LSignerAccount.account) then
        begin
          TArrayTool<TAccount>.Swap(ASelectedAccounts, LAccountIdx,
            Length(ASelectedAccounts) - 1); // ensure signer account processed last
          goto loop_start; // TODO: remove ugly hack with refactoring!
        end;

        // Maintain correct signer fee distribution
        if UInt64(LTotalSignerFee) >= LSignerAccount.balance then
          LFee := 0
        else if LSignerAccount.balance - UInt64(LTotalSignerFee) >
          UInt64(AFee) then
          LFee := AFee
        else
          LFee := LSignerAccount.balance - UInt64(LTotalSignerFee);

        LPCOperation := TOpChangeKeySigned.Create(LNode.Bank.Safebox.CurrentProtocol, LSignerAccount.account,
          LSignerAccount.n_operation + LNoOfOperations + 1, LCurrentAccount.account,
          LWalletKey.PrivateKey, APublicKey, LFee, LPayloadEncodedBytes);
      end
      else
        LPCOperation := TOpChangeKey.Create(LNode.Bank.Safebox.CurrentProtocol, LCurrentAccount.account, LCurrentAccount.n_operation +
          1, LCurrentAccount.account, LWalletKey.PrivateKey, APublicKey, LFee, LPayloadEncodedBytes);

      try
        LTemp := Format('%d. Change Key To %s', [LNoOfOperations + 1, TAccountComp.GetECInfoTxt(APublicKey.EC_OpenSSL_NID), sLineBreak]);
        if LOperationsTxt <> '' then
          LOperationsTxt := LOperationsTxt + LTemp + sLineBreak
        else
          LOperationsTxt := sLineBreak + LTemp;

        if Assigned(LPCOperation) then
        begin
          LOperationsHashTree.AddOperationToHashTree(LPCOperation);
          Inc(LNoOfOperations);
          Inc(LTotalSignerFee, LFee);
          if LOperationToString <> '' then
            LOperationToString := LOperationToString + #10;
          LOperationToString := LOperationToString + LPCOperation.ToString;
        end;
      finally
        FreeAndNil(LPCOperation);
      end;

    end;

    if (LOperationsHashTree.OperationsCount = 0) then
    begin
      AErrorMessage := 'No Valid Operation to Execute';
      Exit(False);
    end;

    Exit(TWIZOperationsHelper.OthersFinalizeAndDisplayMessage(LOperationsTxt, LOperationToString, LNoOfOperations, LTotalSignerFee, LOperationsHashTree, AErrorMessage));
  finally
    LOperationsHashTree.Free;
  end;

end;

class function TWIZOperationsHelper.ExecuteEnlistAccountForSale(const ASelectedAccounts: TArray<TAccount>; const ASignerAccount, ASellerAccount: TAccount; const APublicKey: TAccountKey; AFee, ASalePrice: int64; ALockedUntilBlock: UInt32; const AAccountSaleMode: TAccountSaleMode; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string): boolean;
var
  LWalletKey: TWalletKey;
  LWalletKeys: TWalletKeys;
  LNode: TNode;
  LPCOperation: TPCOperation;
  LOperationsHashTree: TOperationsHashTree;
  LTotalSignerFee, LFee: int64;
  LOperationsTxt, LOperationToString, LTemp: string;
  LAccountIdx, LNoOfOperations: integer;
  LCurrentAccount, LSignerAccount: TAccount;
  LPayloadEncodedBytes: TRawBytes;
begin

  LWalletKeys := TWallet.Keys;
  LNode := TNode.Node;
  LSignerAccount := ASignerAccount;

  if not TWIZOperationsHelper.ValidateOperationsInput(ASelectedAccounts, LWalletKeys, LNode, AErrorMessage) then
    Exit(False);

  LOperationsHashTree := TOperationsHashTree.Create;
  try
    LTotalSignerFee := 0;
    LNoOfOperations := 0;
    LOperationsTxt := '';
    LOperationToString := '';

    for LAccountIdx := Low(ASelectedAccounts) to High(ASelectedAccounts) do
    begin
      LPCOperation := nil; // reset LPCOperation to Nil
      LCurrentAccount := ASelectedAccounts[LAccountIdx];

      if not TWIZOperationsHelper.IsOwnerOfWallet(LCurrentAccount, LWalletKeys, LWalletKey, AErrorMessage) then
        Exit(False);

      if TAccountComp.IsAccountForSale(LCurrentAccount.accountInfo) then
      begin
        AErrorMessage := Format('Account "%s" Is Already Enlisted For Sale', [LCurrentAccount.AccountString]);
        Exit(False);
      end;

      if (ASellerAccount.account = LCurrentAccount.account) then
      begin
        AErrorMessage := 'Seller Account Cannot Be Same As Account To Be Sold.';
        Exit(False);
      end;

      if AAccountSaleMode = akaPrivateSale then
      begin

        if TAccountComp.EqualAccountKeys(APublicKey,
          LCurrentAccount.accountInfo.accountKey) then
        begin
          AErrorMessage := 'You Cannot Sell To An Account That You Want To Enlist For Sale.';
          Exit(False);
        end;

        if ALockedUntilBlock = 0 then
        begin
          AErrorMessage := 'You Didn''t Insert a Valid Locking Block.';
          Exit(False);
        end;
      end;

      if (LNode.Bank.SafeBox.CurrentProtocol = CT_PROTOCOL_1) then
      begin
        AErrorMessage := 'This Operation Needs PROTOCOL 2 Active';
        Exit(False);
      end;

      if LSignerAccount.balance > UInt64(AFee) then
        LFee := AFee
      else
        LFee := LSignerAccount.balance;

      if not UpdatePayload(LCurrentAccount.accountInfo.accountKey, LSignerAccount.accountInfo.accountKey, APayloadEncryptionMode, APayloadContent, LPayloadEncodedBytes, APayloadEncryptionPassword, AErrorMessage) then
      begin
        AErrorMessage := Format('Error Encoding Payload Of Selected Account "%s. ", Specific Error Is "%s"', [LCurrentAccount.AccountString, AErrorMessage]);
        Exit(False);
      end;

      case AAccountSaleMode of
        akaPublicSale:

          LPCOperation := TOpListAccountForSale.CreateListAccountForSale(
            LNode.Bank.Safebox.CurrentProtocol, LSignerAccount.account, LSignerAccount.n_operation + 1 + LAccountIdx,
            LCurrentAccount.account, ASalePrice, LFee, ASellerAccount.account,
            APublicKey, 0, LWalletKey.PrivateKey, LPayloadEncodedBytes);

        akaPrivateSale:

          LPCOperation := TOpListAccountForSale.CreateListAccountForSale(
            LNode.Bank.Safebox.CurrentProtocol, LSignerAccount.account, LSignerAccount.n_operation + 1 + LAccountIdx,
            LCurrentAccount.account, ASalePrice, LFee, ASellerAccount.account,
            APublicKey, ALockedUntilBlock, LWalletKey.PrivateKey, LPayloadEncodedBytes)
        else
          raise Exception.Create('Invalid Account Sale Type')
      end;

      try
        LTemp := Format('%d. Enlist Account %s For Sale At a Price Of %s PASC %s', [LNoOfOperations + 1, LCurrentAccount.DisplayString, TAccountComp.FormatMoney(ASalePrice), sLineBreak]);
        if LOperationsTxt <> '' then
          LOperationsTxt := LOperationsTxt + LTemp + sLineBreak
        else
          LOperationsTxt := sLineBreak + LTemp;

        if Assigned(LPCOperation) then
        begin
          LOperationsHashTree.AddOperationToHashTree(LPCOperation);
          Inc(LNoOfOperations);
          Inc(LTotalSignerFee, LFee);
          if LOperationToString <> '' then
            LOperationToString := LOperationToString + #10;
          LOperationToString := LOperationToString + LPCOperation.ToString;
        end;
      finally
        FreeAndNil(LPCOperation);
      end;

    end;

    if (LOperationsHashTree.OperationsCount = 0) then
    begin
      AErrorMessage := 'No Valid Operation to Execute';
      Exit(False);
    end;

    Exit(TWIZOperationsHelper.OthersFinalizeAndDisplayMessage(LOperationsTxt, LOperationToString, LNoOfOperations, LTotalSignerFee, LOperationsHashTree, AErrorMessage));

  finally
    LOperationsHashTree.Free;
  end;
end;

class function TWIZOperationsHelper.ExecuteDelistAccountFromSale(const ASelectedAccounts: TArray<TAccount>; const ASignerAccount: TAccount; AFee: int64; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string): boolean;
var
  LWalletKey: TWalletKey;
  LWalletKeys: TWalletKeys;
  LNode: TNode;
  LPCOperation: TPCOperation;
  LOperationsHashTree: TOperationsHashTree;
  LTotalSignerFee, LFee: int64;
  LOperationsTxt, LOperationToString, LTemp: string;
  LAccountIdx, LNoOfOperations: integer;
  LCurrentAccount, LSignerAccount: TAccount;
  LPayloadEncodedBytes: TRawBytes;
begin

  LWalletKeys := TWallet.Keys;
  LNode := TNode.Node;
  LSignerAccount := ASignerAccount;

  if not TWIZOperationsHelper.ValidateOperationsInput(ASelectedAccounts, LWalletKeys, LNode, AErrorMessage) then
    Exit(False);

  LOperationsHashTree := TOperationsHashTree.Create;
  try
    LTotalSignerFee := 0;
    LNoOfOperations := 0;
    LOperationsTxt := '';
    LOperationToString := '';

    for LAccountIdx := Low(ASelectedAccounts) to High(ASelectedAccounts) do
    begin
      LPCOperation := nil; // reset LPCOperation to Nil
      LCurrentAccount := ASelectedAccounts[LAccountIdx];

      if not TWIZOperationsHelper.IsOwnerOfWallet(LCurrentAccount, LWalletKeys, LWalletKey, AErrorMessage) then
        Exit(False);

      if not TAccountComp.IsAccountForSale(LCurrentAccount.accountInfo) then
      begin
        AErrorMessage := Format('Account "%s" is not enlisted for sale so cannot be delisted', [LCurrentAccount.AccountString]);
        Exit(False);
      end;

      if (TAccountComp.IsAccountLocked(LCurrentAccount.accountInfo, LNode.Bank.BlocksCount)) then
      begin
        AErrorMessage := Format('Target Account "%s"  Is Locked Until Block %u', [LCurrentAccount.AccountString, LCurrentAccount.accountInfo.locked_until_block]);
        Exit(False);
      end;

      if (TAccountComp.IsAccountLocked(LSignerAccount.accountInfo, LNode.Bank.BlocksCount)) then
      begin
        AErrorMessage := Format('Signer Account "%s"  Is Locked Until Block %u', [LSignerAccount.AccountString, LSignerAccount.accountInfo.locked_until_block]);
        Exit(False);
      end;

      if (not TAccountComp.EqualAccountKeys(LSignerAccount.accountInfo.accountKey, LCurrentAccount.accountInfo.accountKey)) then
      begin
        AErrorMessage := Format('Signer Account %s Is Not The Owner Of Delisted Account %s', [LSignerAccount.AccountString, LCurrentAccount.AccountString]);
        Exit(False);
      end;

      if (LNode.Bank.SafeBox.CurrentProtocol = CT_PROTOCOL_1) then
      begin
        AErrorMessage := 'This Operation Needs PROTOCOL 2 Active';
        Exit(False);
      end;

      if LSignerAccount.balance > UInt64(AFee) then
        LFee := AFee
      else
        LFee := LSignerAccount.balance;

      if not UpdatePayload(LCurrentAccount.accountInfo.accountKey, LSignerAccount.accountInfo.accountKey, APayloadEncryptionMode, APayloadContent, LPayloadEncodedBytes, APayloadEncryptionPassword, AErrorMessage) then
      begin
        AErrorMessage := Format('Error Encoding Payload Of Selected Account "%s. ", Specific Error Is "%s"', [LCurrentAccount.AccountString, AErrorMessage]);
        Exit(False);
      end;

      LPCOperation := TOpDelistAccountForSale.CreateDelistAccountForSale(LNode.Bank.Safebox.CurrentProtocol,
        LSignerAccount.account, LSignerAccount.n_operation + 1 + LAccountIdx, LCurrentAccount.account, LFee, LWalletKey.PrivateKey,
        LPayloadEncodedBytes);

      try
        LTemp := Format('%d. Delist Account %s From Sale %s', [LNoOfOperations + 1, LCurrentAccount.DisplayString, sLineBreak]);
        if LOperationsTxt <> '' then
          LOperationsTxt := LOperationsTxt + LTemp + sLineBreak
        else
          LOperationsTxt := sLineBreak + LTemp;

        if Assigned(LPCOperation) then
        begin
          LOperationsHashTree.AddOperationToHashTree(LPCOperation);
          Inc(LNoOfOperations);
          Inc(LTotalSignerFee, LFee);
          if LOperationToString <> '' then
            LOperationToString := LOperationToString + #10;
          LOperationToString := LOperationToString + LPCOperation.ToString;
        end;
      finally
        FreeAndNil(LPCOperation);
      end;

    end;

    if (LOperationsHashTree.OperationsCount = 0) then
    begin
      AErrorMessage := 'No Valid Operation to Execute';
      Exit(False);
    end;

    Exit(TWIZOperationsHelper.OthersFinalizeAndDisplayMessage(LOperationsTxt, LOperationToString, LNoOfOperations, LTotalSignerFee, LOperationsHashTree, AErrorMessage));

  finally
    LOperationsHashTree.Free;
  end;
end;

class function TWIZOperationsHelper.ExecuteChangeAccountInfo(const ASelectedAccounts, ASignerAccounts: TArray<TAccount>; AFee: int64; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; const ANewName: TRawBytes; const ANewType: word; var AErrorMessage: string): boolean;
var
  LWalletKey: TWalletKey;
  LWalletKeys: TWalletKeys;
  LNode: TNode;
  LPCOperation: TPCOperation;
  LOperationsHashTree: TOperationsHashTree;
  LTotalSignerFee, LFee: int64;
  LOperationsTxt, LOperationToString, LTemp: string;
  LAccountIdx, LNoOfOperations, LAccNumberIndex: integer;
  LCurrentAccount, LSignerAccount: TAccount;
  LPayloadEncodedBytes, LNewName: TRawBytes;
  LChangeType, LChangeName: boolean;
begin

  LWalletKeys := TWallet.Keys;
  LNode := TNode.Node;
  LChangeName := False;
  LChangeType := False;

  if not TWIZOperationsHelper.ValidateOperationsInput(ASelectedAccounts, LWalletKeys, LNode, AErrorMessage) then
    Exit(False);

  LOperationsHashTree := TOperationsHashTree.Create;
  try
    LTotalSignerFee := 0;
    LNoOfOperations := 0;
    LOperationsTxt := '';
    LOperationToString := '';

    for LAccountIdx := Low(ASelectedAccounts) to High(ASelectedAccounts) do
    begin
      LPCOperation := nil; // reset LPCOperation to Nil
      LCurrentAccount := ASelectedAccounts[LAccountIdx];

      if Length(ASelectedAccounts) = 1 then
      begin
        LSignerAccount := ASignerAccounts[0];

        LNewName := ANewName;

        if not TBaseType.Equals(LNewName, LCurrentAccount.Name) then
        begin
          LChangeName := True;
          if LNewName <> nil then
          begin
            if (not TPCSafeBox.ValidAccountName(LNewName, AErrorMessage)) then
            begin
              AErrorMessage := Format('New name "%s" is not a valid name: %s ', [LNewName.ToPrintable, AErrorMessage]);
              Exit(False);
            end;
            LAccNumberIndex := (TNode.Node.Bank.SafeBox.FindAccountByName(LNewName));
            if (LAccNumberIndex >= 0) then
            begin
              AErrorMessage := Format('Name "%s" is used by account %s ', [LNewName.ToPrintable, TAccountComp.AccountNumberToAccountTxtNumber(LAccNumberIndex)]);
              Exit(False);
            end;
          end;
        end;

        if (TBaseType.Equals(LNewName, LCurrentAccount.Name)) and (ANewType = LCurrentAccount.account_type) then
        begin
          AErrorMessage := 'New account name and type are same as former.';
          Exit(False);
        end;

      end
      else
      begin
        LSignerAccount := LCurrentAccount;
      end;

      LChangeType := ANewType <> LCurrentAccount.account_type;

      if not TWIZOperationsHelper.IsOwnerOfWallet(LCurrentAccount, LWalletKeys, LWalletKey, AErrorMessage) then
        Exit(False);

      if (TAccountComp.IsAccountLocked(LCurrentAccount.accountInfo, LNode.Bank.BlocksCount)) then
      begin
        AErrorMessage := Format('Target Account "%s" Is Locked Until Block %u', [LCurrentAccount.AccountString, LCurrentAccount.accountInfo.locked_until_block]);
        Exit(False);
      end;

      if (TAccountComp.IsAccountLocked(LSignerAccount.accountInfo, LNode.Bank.BlocksCount)) then
      begin
        AErrorMessage := Format('Signer Account "%s" Is Locked Until Block %u', [LSignerAccount.AccountString, LSignerAccount.accountInfo.locked_until_block]);
        Exit(False);
      end;

      if (not TAccountComp.EqualAccountKeys(LSignerAccount.accountInfo.accountKey, LCurrentAccount.accountInfo.accountKey)) then
      begin
        AErrorMessage := Format('Signer Account %s Is Not The Owner Of Target Account %s', [LSignerAccount.AccountString, LCurrentAccount.AccountString]);
        Exit(False);
      end;

      if (LNode.Bank.SafeBox.CurrentProtocol = CT_PROTOCOL_1) then
      begin
        AErrorMessage := 'This Operation Needs PROTOCOL 2 Active';
        Exit(False);
      end;

      if LSignerAccount.balance > UInt64(AFee) then
        LFee := AFee
      else
        LFee := LSignerAccount.balance;

      if not UpdatePayload(LCurrentAccount.accountInfo.accountKey, LSignerAccount.accountInfo.accountKey, APayloadEncryptionMode, APayloadContent, LPayloadEncodedBytes, APayloadEncryptionPassword, AErrorMessage) then
      begin
        AErrorMessage := Format('Error Encoding Payload Of Selected Account "%s. ", Specific Error Is "%s"', [LCurrentAccount.AccountString, AErrorMessage]);
        Exit(False);
      end;


      LPCOperation := TOpChangeAccountInfo.CreateChangeAccountInfo(LNode.Bank.Safebox.CurrentProtocol,
        LSignerAccount.account, LSignerAccount.n_operation + 1, LCurrentAccount.account, LWalletKey.PrivateKey, False, CT_TECDSA_Public_Nul,
        LChangeName, LNewName, LChangeType, ANewType, LFee, LPayloadEncodedBytes);

      try
        if (LChangeName) and (LChangeType) then
        begin
          LTemp := Format('%d. Change Account %s Name and Type from [%s, %d] To [%s, %d] %s', [LNoOfOperations + 1, LCurrentAccount.DisplayString, LCurrentAccount.Name.ToPrintable, LCurrentAccount.account_type, LNewName.ToPrintable, ANewType, sLineBreak]);
        end
        else if LChangeName then
        begin
          LTemp := Format('%d. Change Account %s Name from [%s] To [%s] %s', [LNoOfOperations + 1, LCurrentAccount.DisplayString, LCurrentAccount.Name.ToPrintable, LNewName.ToPrintable, sLineBreak]);
        end
        else if LChangeType then
        begin
          LTemp := Format('%d. Change Account %s Type from [%d] To [%d] %s', [LNoOfOperations + 1, LCurrentAccount.DisplayString, LCurrentAccount.account_type, ANewType, sLineBreak]);
        end;

        if LOperationsTxt <> '' then
          LOperationsTxt := LOperationsTxt + LTemp + sLineBreak
        else
          LOperationsTxt := sLineBreak + LTemp;

        if Assigned(LPCOperation) then
        begin
          LOperationsHashTree.AddOperationToHashTree(LPCOperation);
          Inc(LNoOfOperations);
          Inc(LTotalSignerFee, LFee);
          if LOperationToString <> '' then
            LOperationToString := LOperationToString + #10;
          LOperationToString := LOperationToString + LPCOperation.ToString;
        end;
      finally
        FreeAndNil(LPCOperation);
      end;

    end;

    if (LOperationsHashTree.OperationsCount = 0) then
    begin
      AErrorMessage := 'No Valid Operation to Execute';
      Exit(False);
    end;

    Exit(TWIZOperationsHelper.OthersFinalizeAndDisplayMessage(LOperationsTxt, LOperationToString, LNoOfOperations, LTotalSignerFee, LOperationsHashTree, AErrorMessage));

  finally
    LOperationsHashTree.Free;
  end;
end;

class function TWIZOperationsHelper.ExecuteBuyAccount(const ASelectedAccounts: TArray<TAccount>; const AAccountToBuy: TAccount; AFee: int64; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; const AAmount: int64; const ANewOwnerPublicKey: TAccountKey; var AErrorMessage: string): boolean;
var
  LWalletKey: TWalletKey;
  LWalletKeys: TWalletKeys;
  LNode: TNode;
  LPCOperation: TPCOperation;
  LOperationsHashTree: TOperationsHashTree;
  LTotalSignerFee, LFee: int64;
  LOperationsTxt, LOperationToString, LTemp: string;
  LAccountIdx, LNoOfOperations, LAccNumberIndex: integer;
  LCurrentAccount, LSignerAccount: TAccount;
  LPayloadEncodedBytes, LNewName: TRawBytes;
begin

  LWalletKeys := TWallet.Keys;
  LNode := TNode.Node;

  if not TWIZOperationsHelper.ValidateOperationsInput(ASelectedAccounts, LWalletKeys, LNode, AErrorMessage) then
    Exit(False);

  LOperationsHashTree := TOperationsHashTree.Create;
  try
    LTotalSignerFee := 0;
    LNoOfOperations := 0;
    LOperationsTxt := '';
    LOperationToString := '';
    // although buyaccount does not support multioperation for now, this loop was intentionally put here
    for LAccountIdx := Low(ASelectedAccounts) to High(ASelectedAccounts) do
    begin
      LPCOperation := nil; // reset LPCOperation to Nil
      LCurrentAccount := ASelectedAccounts[LAccountIdx];
      LSignerAccount := LCurrentAccount;

      if Length(ASelectedAccounts) <> 1 then
      begin
        AErrorMessage := 'Cannot Buy Accounts With MultiOperations. Use Only 1 Account';
        Exit(False);
      end;

      if (LNode.Bank.SafeBox.CurrentProtocol = CT_PROTOCOL_1) then
      begin
        AErrorMessage := 'This Operation Needs PROTOCOL 2 Active';
        Exit(False);
      end;

      if not TAccountComp.IsAccountForSale(AAccountToBuy.accountInfo) then
      begin
        AErrorMessage := Format('Account "%s" is not enlisted for sale', [AAccountToBuy.AccountString]);
        Exit(False);
      end;

      if LSignerAccount.balance > UInt64(AFee) then
        LFee := AFee
      else
        LFee := LSignerAccount.balance;


      if ((AAmount + LFee) > LCurrentAccount.balance) then
      begin
        AErrorMessage := Format('Insufficient Funds in Account "%s" to buy Account "%s"', [LCurrentAccount.AccountString, AAccountToBuy.AccountString]);
        Exit(False);
      end;

      if not UpdatePayload(LCurrentAccount.accountInfo.accountKey, LSignerAccount.accountInfo.accountKey, APayloadEncryptionMode, APayloadContent, LPayloadEncodedBytes, APayloadEncryptionPassword, AErrorMessage) then
      begin
        AErrorMessage := Format('Error Encoding Payload Of Selected Account "%s. ", Specific Error Is "%s"', [LCurrentAccount.AccountString, AErrorMessage]);
        Exit(False);
      end;

      LPCOperation := TOpBuyAccount.CreateBuy(LNode.Bank.Safebox.CurrentProtocol, LCurrentAccount.account, LCurrentAccount.n_operation + 1, AAccountToBuy.account, AAccountToBuy.accountInfo.account_to_pay,
        AAccountToBuy.accountInfo.price, AAmount, LFee, ANewOwnerPublicKey, LWalletKey.PrivateKey, LPayloadEncodedBytes);

      try
      LTemp := Format('%d. Buy Account %s for %s PASC %s', [LNoOfOperations + 1, AAccountToBuy.AccountString, TAccountComp.FormatMoney(AAmount), sLineBreak]);

        if LOperationsTxt <> '' then
          LOperationsTxt := LOperationsTxt + LTemp + sLineBreak
        else
          LOperationsTxt := sLineBreak + LTemp;

        if Assigned(LPCOperation) then
        begin
          LOperationsHashTree.AddOperationToHashTree(LPCOperation);
          Inc(LNoOfOperations);
          Inc(LTotalSignerFee, LFee);
          if LOperationToString <> '' then
            LOperationToString := LOperationToString + #10;
          LOperationToString := LOperationToString + LPCOperation.ToString;
        end;
      finally
        FreeAndNil(LPCOperation);
      end;

    end;

    if (LOperationsHashTree.OperationsCount = 0) then
    begin
      AErrorMessage := 'No Valid Operation to Execute';
      Exit(False);
    end;

    Exit(TWIZOperationsHelper.OthersFinalizeAndDisplayMessage(LOperationsTxt, LOperationToString, LNoOfOperations, LTotalSignerFee, LOperationsHashTree, AErrorMessage));

  finally
    LOperationsHashTree.Free;
  end;
end;



end.
