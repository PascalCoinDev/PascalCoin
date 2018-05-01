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
  private
    class function GetBalanceInternal(const AKeys: array of TAccountKey; IncludePending: boolean = False): TBalanceSummary;
  public
    class function GetSignerCandidates(ANumOps: integer; ASingleOperationFee: int64; const ACandidates: array of TAccount): TArray<TAccount>; static;
    class function GetOperationShortText(const OpType, OpSubType: DWord): ansistring; static; inline;
    class function GetUserBalance(IncludePending: boolean = False): TBalanceSummary;
    class function GetUserAccounts(IncludePending: boolean = False): TArray<TAccount>;
    class function GetUserAccountNumbers: TArray<cardinal>;
    class function GetModifiedAccounts(const AAccounts: array of TAccount; IncludePending : boolean = false): TArray<TAccount>;
    class function GetBalance(const AKey: TAccountKey; IncludePending: boolean = False): TBalanceSummary; overload;
    class function GetBalance(const AKeys: array of TAccountKey; IncludePending: boolean = False): TBalanceSummary; overload;
  end;

  { TNodeHelper }

  TNodeHelper = class helper for TNode
   function HasBestKnownBlockchainTip: boolean;
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
  UMemory, UConst, UWallet, UECIES, UAES;

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

class function TCoreTool.GetModifiedAccounts(const AAccounts: array of TAccount; IncludePending : boolean = false): TArray<TAccount>;
var
  i: integer;
  LChanged: TList<TAccount>;
  LAcc: TAccount;
  GC: TDisposables;
begin
  LChanged := GC.AddObject(TList<TAccount>.Create) as TList<TAccount>;
  for i := Low(AAccounts) to High(AAccounts) do
  begin
    if IncludePending then
      LAcc := TNode.Node.Operations.SafeBoxTransaction.Account(AAccounts[i].account)
    else begin
      TNode.Node.Bank.SafeBox.StartThreadSafe;
      LAcc := TNode.Node.Bank.SafeBox.Account(AAccounts[i].account);
      TNode.Node.Bank.SafeBox.EndThreadSave;
    end;
    if (LAcc.n_Operation <> AAccounts[i].n_operation) or (LAcc.Balance <> AAccounts[i].balance) then
      LChanged.Add(LAcc);
  end;
  Result := LChanged.ToArray;
end;

class function TCoreTool.GetBalance(const AKey: TAccountKey; IncludePending: boolean = False): TBalanceSummary;
begin
  Result := GetBalance([AKey], IncludePending);
end;

class function TCoreTool.GetBalance(const AKeys: array of TAccountKey; IncludePending: boolean = False): TBalanceSummary;
begin
  Result := GetBalanceInternal(AKeys, IncludePending);
end;

class function TCoreTool.GetBalanceInternal(const AKeys: array of TAccountKey; IncludePending: boolean = False): TBalanceSummary;
var
  i: integer;
  LAcc: TAccount;
  LAccs: THashSet<TAccount>;
  LKeys: THashSet<TAccountKey>;
  GC: TDisposables;
begin
  // Setup local collections
  LAccs := GC.AddObject(THashSet<TAccount>.Create(TAccountEqualityComparer.Create)) as THashSet<TAccount>;
  LKeys := GC.AddObject(THashSet<TAccountKey>.Create(TAccountKeyEqualityComparer.Create)) as THashSet<TAccountKey>;

  // Gather all keys into hashset
  for i := Low(AKeys) to High(AKeys) do
    LKeys.Add(AKeys[i]);

  // Gather all referenced accounts
  for i := 0 to TNode.Node.Bank.SafeBox.AccountsCount - 1 do
  begin
    if IncludePending then
      LAcc := TNode.Node.Operations.SafeBoxTransaction.Account(i)
    else begin
      TNode.Node.Bank.SafeBox.StartThreadSafe;
      LAcc := TNode.Node.Bank.SafeBox.Account(i);
      TNode.Node.Bank.SafeBox.EndThreadSave;
    end;
    if LKeys.Contains(LAcc.accountInfo.accountKey) then
      LAccs.Add(LAcc);
  end;

  // Build the results
  Result := CT_BalanceSummary_Nil;
  for LAcc in LAccs do
  begin
    Inc(Result.TotalPASA);
    Inc(Result.TotalPASC, LAcc.Balance);
  end;
end;

class function TCoreTool.GetUserBalance(IncludePending: boolean = False): TBalanceSummary;
var
  i: integer;
  LAccs: TArray<TAccount>;
begin
  Result := CT_BalanceSummary_Nil;
  LAccs := TCoreTool.GetUserAccounts(IncludePending);
  for i := Low(LAccs) to High(LAccs) do
  begin
    Inc(Result.TotalPASA);
    Inc(Result.TotalPASC, LAccs[i].Balance);
  end;
end;

class function TCoreTool.GetUserAccounts(IncludePending: boolean = False): TArray<TAccount>;
var
  i, j: integer;
  LAccs: TList<TAccount>;
  LAcc: TAccount;
  LList: TOrderedCardinalList;
  Disposables: TDisposables;
begin
  LAccs := Disposables.AddObject(TList<TAccount>.Create) as TList<TAccount>;
  for i := 0 to TWallet.Keys.Count - 1 do begin
    LList := TWallet.Keys.AccountsKeyList.AccountKeyList[i];
    for j := 0 to LList.Count - 1 do begin
      if IncludePending then
        LAcc := TNode.Node.Operations.SafeBoxTransaction.Account(LList.Get(j))
      else begin
        TNode.Node.Bank.SafeBox.StartThreadSafe;
        LAcc := TNode.Node.Bank.SafeBox.Account(LList.Get(j));
        TNode.Node.Bank.SafeBox.EndThreadSave;
      end;
      LAccs.Add(LAcc);
    end;
  end;
  Result := LAccs.ToArray;
  TArrayHelper<TAccount>.Sort(Result, TAccountComparer.Create);
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
      LAccs.Add(j);
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
