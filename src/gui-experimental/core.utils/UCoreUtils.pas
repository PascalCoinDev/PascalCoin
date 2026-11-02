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

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}


{$IFDEF FPC}
{$modeswitch nestedprocvars}
{$ENDIF}

{ Provides account/key comparisons, wallet summaries and bounded
  operation-history queries. Prepares, validates and executes operation
  batches using supplied accounts and keys. }

{ Public types in this unit

  Type                        | Purpose
  ----------------------------+-------------------------------------------------------------------------
  TAccountComparer            | Orders accounts for wallet collections.
  TAccountEqualityComparer    | Compares and hashes account identities.
  TAccountHelper              | Formats account identifiers, display text and detailed information.
  TAccountKeyComparer         | Orders account public keys.
  TAccountKeyEqualityComparer | Compares and hashes account public keys.
  TCoreTool                   | Collects wallet accounts, balances, signer candidates and pending history.
  TNodeHelper                 | Reads account snapshots and bounded pending/stored history.
  TOperationResumeHelper      | Formats operation hashes and operation-detail text.
  TTimeSpanHelper             | Converts a time span to its expected block count.
  TWalletOperationRequest     | Carries the accounts, fees, keys and payload for operation preparation.
  TWIZOperationsHelper        | Prepares, validates, signs and submits wallet operation batches.
}

interface

uses
  {$IFNDEF FPC}Winapi.Windows,{$ENDIF}
  Classes, SysUtils, Forms, Dialogs, {$IFDEF FPC}LCLType,{$ENDIF} UAccounts, UBlockChain, UNode, UWallet,
  UBaseTypes, UCommon, UCoreObjects, UCommon.Collections, Generics.Defaults, UPCDataTypes;

type

  {$IFDEF FPC}TAccountHashCode = UInt32;{$ELSE}TAccountHashCode = Integer;{$ENDIF}

  { TAccountComparer }

  TAccountComparer = class(TComparer<TAccount>)
    function Compare({$IFDEF FPC}constref{$ELSE}const{$ENDIF} ALeft, ARight: TAccount): integer; override;
    class function DoCompare({$IFDEF FPC}constref{$ELSE}const{$ENDIF} ALeft, ARight: TAccount): integer; inline;
  end;

  { TAccountEqualityComparer }

  TAccountEqualityComparer = class(TEqualityComparer<TAccount>)
  public
    function Equals({$IFDEF FPC}constref{$ELSE}const{$ENDIF} ALeft, ARight: TAccount): boolean; override;
    function GetHashCode({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AValue: TAccount): TAccountHashCode; override;
    class function AreEqual({$IFDEF FPC}constref{$ELSE}const{$ENDIF} ALeft, ARight: TAccount): boolean;
    class function CalcHashCode({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AValue: TAccount): UInt32;
  end;

  { TAccountKeyComparer }

  TAccountKeyComparer = class(TComparer<TAccountKey>)
    function Compare({$IFDEF FPC}constref{$ELSE}const{$ENDIF} ALeft, ARight: TAccountKey): integer; override;
    class function DoCompare({$IFDEF FPC}constref{$ELSE}const{$ENDIF} ALeft, ARight: TAccountKey): integer; inline;
  end;

  { TAccountKeyEqualityComparer }

  TAccountKeyEqualityComparer = class(TEqualityComparer<TAccountKey>)
  public
    function Equals({$IFDEF FPC}constref{$ELSE}const{$ENDIF} ALeft, ARight: TAccountKey): boolean; override;
    function GetHashCode({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AValue: TAccountKey): TAccountHashCode; override;
    class function AreEqual({$IFDEF FPC}constref{$ELSE}const{$ENDIF} ALeft, ARight: TAccountKey): boolean;
    class function CalcHashCode({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AValue: TAccountKey): UInt32;
  end;

  { TCoreTool }

  TCoreTool = class
  public
    class function GetSignerCandidates(ANumOps: integer; ASingleOperationFee: int64; const ACandidates: array of TAccount): TArray<TAccount>; static;
    class function GetOperationShortText(const OpType, OpSubType: DWord): String; static; inline;
    class function GetUserBalance(IncludePending: boolean = False): TBalanceSummary;
    class function GetUserAccounts(IncludePending: boolean = False): TArray<TAccount>; overload;
    class function GetUserAccounts(out Balance: TBalanceSummary; IncludePending: boolean = False): TArray<TAccount>; overload;
    class function GetUserAccountNumbers: TArray<cardinal>;
    class function AreAccountBalancesGreaterThan(const ACandidates: array of TAccount; AAmount: int64; var AFaultyAccount: TAccount): boolean; static;
    // The caller retains the pending-state lock and owns both inputs. Build the
    // account histories with one operation-tree traversal, then apply paging.
    class function GetPendingOperationsAffectingAccounts(AOperations: TOperationsHashTree;
      ASnapshot: TPCSafeBoxTransaction; ABlock: Cardinal;
      const AAccountNumbers: array of Cardinal; ASkipCount, ATakeCount: Integer): TArray<TOperationResume>; static;
  end;

  { TNodeHelper }

  TNodeHelper = class helper for TNode
    function HasBestKnownBlockchainTip: boolean;
    function BlockTip: cardinal;
    function GetAccount(AAccountNumber: cardinal; AIncludePending: boolean = True): TAccount;
    function GetAccounts(const AAccountNumbers: array of cardinal; AIncludePending: boolean = True): TArray<TAccount>;
    function GetPendingOperationsAffectingAccounts(const AAccountNumbers: array of cardinal; ASkipCount, ATakeCount: integer): TArray<TOperationResume>;
    function GetStoredOperationsAffectingAccounts(const AAccountNumbers: array of cardinal; ABlockDepth: Cardinal; ASkipCount, ATakeCount: integer): TArray<TOperationResume>;
  end;

  { TAccountHelper }

  TAccountHelper = record helper for TAccount
    function GetAccountString : String;
    function GetDisplayString : String;
    function GetInfoText(const ABank : TPCBank) : String; overload;
    function GetInfoText(ABlocksCount : Cardinal) : String; overload;
    property AccountString : String read GetAccountString;
    property DisplayString : String read GetDisplayString;
  end;

  { TOperationResumeHelper }

  TOperationResumeHelper = record helper for TOperationResume
    function GetPrintableOPHASH : String;
    function GetInfoText(const ABank : TPCBank) : String; overload;
    function GetInfoText(ABlocksCount : Cardinal; const ANode: TNode = nil;
      const AWalletKeys: TWalletKeys = nil) : String; overload;
  end;

  { TTimeSpanHelper }

  TTimeSpanHelper = record helper for TTimeSpan
    function TotalBlockCount : Integer;
  end;

  { TWalletOperationRequest }

  // Account snapshots and keys are supplied explicitly so preparation never opens
  // a wallet, contacts a node or transmits an operation. The caller owns the keys.
  TWalletOperationRequest = record
    OperationType: TExecuteOperationType;
    Accounts, SignerAccounts: TArray<TAccount>;
    DestinationAccount, SellerAccount: TAccount;
    PublicKey: TAccountKey;
    Amount, Fee, SalePrice: Int64;
    LockedUntilBlock: Cardinal;
    SendMode: TSendPASCMode;
    SaleMode: TAccountSaleMode;
    SigningMode: TOperationSigningMode;
    PayloadMode: TPayloadEncryptionMode;
    PayloadContent, PayloadPassword: string;
    NewName: TRawBytes;
    NewType: Word;
    // Nonempty for a reviewed wizard batch. Refreshed targets must still match
    // these account IDs before preparation may charge fees or sign operations.
    ApprovedChangeInfoTargets: TArray<TAccount>;
  end;

  { TWIZOperationsHelper }

  TWIZOperationsHelper = class
  private
    class function IsOwnerOfWallet(AAccount: TAccount; AWalletKeys: TWalletKeys; out AWalletKey: TWalletKey; var AErrorMessage: string): boolean; static;
    class function ValidateOperationsInput(const ASelectedAccounts: TArray<TAccount>; AWalletKeys: TWalletKeys; ANode: TNode; var AErrorMessage: string): boolean; static;
    class function UpdatePayload(const ASenderPublicKey, ADestinationPublicKey: TAccountKey; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent: string; var AEncodedPayloadBytes: TRawBytes; const APayloadEncryptionPassword: string; var AErrorMessage: string): boolean; overload; static;
    class function UpdatePayload(const ASenderPublicKey, ADestinationPublicKey: TAccountKey; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent: string; var AOperationPayload: TOperationPayload; const APayloadEncryptionPassword: string; var AErrorMessage: string): boolean; overload; static;
    class function SendPASCFinalizeAndDisplayMessage(const AOperationsTxt, AOperationToString: string; ANoOfOperations: integer; ATotalAmount, ATotalFee: int64; AOperationsHashTree: TOperationsHashTree; var AErrorMessage: string): boolean; static;
    class function OthersFinalizeAndDisplayMessage(const AOperationsTxt, AOperationToString: string; ANoOfOperations: integer; ATotalFee: int64; AOperationsHashTree: TOperationsHashTree; var AErrorMessage: string): boolean; static;
    class function ExecuteOperations(var ARequest: TWalletOperationRequest; var AErrorMessage: string): Boolean; static;
  public
    // Batch type changes leave matching accounts untouched. A single-account
    // request retains its account so the caller can still change its name.
    class function GetChangeAccountInfoTargets(const AAccounts: TArray<TAccount>;
      ANewType: Word): TArray<TAccount>; static;
    // On success the caller owns AOperations. On failure it is nil. The entire
    // batch is prepared and signed before confirmation or node submission.
    class function PrepareOperations(const ARequest: TWalletOperationRequest; AProtocol: Word; ABlocksCount: Cardinal; AWalletKeys: TWalletKeys; out AOperations: TOperationsHashTree; var AErrorMessage: string): Boolean; static;
    // Validates a private copy while the caller retains its pending-state lock.
    class function ValidatePreparedOperations(AOperations: TOperationsHashTree; ASnapshot: TPCSafeBoxTransaction; var AErrorMessage: String): Boolean; static;
    class function ExecuteSendPASC(const ASelectedAccounts: TArray<TAccount>; const ADestinationAccount: TAccount; AAmount, AFee: int64; const ASendPASCMode: TSendPASCMode; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string): boolean; static;
    class function ExecuteChangeKey(const ASelectedAccounts: TArray<TAccount>; const ASignerAccount: TAccount; APublicKey: TAccountKey; AFee: int64; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string; AOperationSigningMode: TOperationSigningMode = akaPrimary): boolean; static;
    class function ExecuteEnlistAccountForSale(const ASelectedAccounts: TArray<TAccount>; const ASignerAccount, ASellerAccount: TAccount; const APublicKey: TAccountKey; AFee, ASalePrice: int64; ALockedUntilBlock: UInt32; const AAccountSaleMode: TAccountSaleMode; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string; AOperationSigningMode: TOperationSigningMode = akaPrimary): boolean; static;
    class function ExecuteDelistAccountFromSale(const ASelectedAccounts: TArray<TAccount>; const ASignerAccount: TAccount; AFee: int64; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string; AOperationSigningMode: TOperationSigningMode = akaPrimary): boolean; static;
    class function ExecuteChangeAccountInfo(const ASelectedAccounts, ASignerAccounts: TArray<TAccount>; AFee: int64; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; const ANewName: TRawBytes; const ANewType: word; var AErrorMessage: string; AOperationSigningMode: TOperationSigningMode = akaPrimary; const AApprovedTargets: TArray<TAccount> = nil): boolean; static;
    class function ExecuteBuyAccount(const ASelectedAccounts: TArray<TAccount>; const AAccountToBuy: TAccount; AFee: int64; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; const AAmount: int64; const ANewOwnerPublicKey: TAccountKey; var AErrorMessage: string): boolean; static;
  end;


implementation

uses
  ULog,
  UPCEncryption,
  UConst,
  UCrypto,
  UMemory,
  UNetProtocol,
  UOpTransaction,
  URPC, UJSONFunctions,
  UPCOrderedLists,
  Generics.Collections, UOrderedList, UFRMMemoText;

procedure CheckWalletQueryCancelled; inline;
begin
  // UI calls remain synchronous. Grid workers are terminated before their
  // owner joins them, so do not continue reading a discarded history snapshot.
  if TThread.CurrentThread.ThreadID = MainThreadID then Exit;
  if TThread.CheckTerminated then Abort;
end;

function LockQueryMempool(ANode: TNode): TPCOperationsComp;
begin
  if TThread.CurrentThread.ThreadID = MainThreadID then Exit(ANode.LockMempoolRead);
  // Attempt the read mode directly so readers can overlap and closing stays cancellable.
  while not ANode.TryLockMempoolRead(0, Result) do begin
    CheckWalletQueryCancelled;
    Sleep(10);
  end;
  try
    CheckWalletQueryCancelled;
  except
    ANode.UnlockMempoolRead;
    raise;
  end;
end;

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

class function TCoreTool.GetOperationShortText(const OpType, OpSubType: DWord): String;
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
  LNumbers: TList<Cardinal>;
  LIndex: TOrderedAccountKeysList;
  LList: TAccountsNumbersList;
  LNode: TNode;
  LMemPool: TPCOperationsComp;
  Disposables: TDisposables;
begin
  CheckWalletQueryCancelled;
  Balance := CT_BalanceSummary_Nil;
  LAccs := Disposables.AddObject(TList<TAccount>.Create) as TList<TAccount>;
  LNumbers := Disposables.AddObject(TList<Cardinal>.Create) as TList<Cardinal>;
  LNode := TNode.Node;
  LIndex := TWallet.Keys.AccountsKeyList;
  LMemPool := nil;
  // Core operation application takes mempool before safebox. Confirmed reads
  // need only the safebox; pending reads keep the same lock order as the core.
  if IncludePending then LMemPool := LockQueryMempool(LNode);
  try
    LNode.Bank.SafeBox.StartThreadSafe;
    try
      LIndex.Lock;
      try
        for i := 0 to LIndex.Count-1 do begin
          CheckWalletQueryCancelled;
          LList := LIndex.AccountKeyList[i];
          LList.FillList(0,LList.Count,LNumbers);
          for j := 0 to LNumbers.Count-1 do begin
            CheckWalletQueryCancelled;
            if IncludePending then
              LAcc := LMemPool.SafeBoxTransaction.Account(LNumbers[j])
            else
              LAcc := LNode.Bank.SafeBox.Account(LNumbers[j]);
            LAccs.Add(LAcc);
            Inc(Balance.TotalPASA);
            Inc(Balance.TotalPASC,LAcc.Balance);
          end;
        end;
      finally
        LIndex.Unlock;
      end;
    finally
      LNode.Bank.SafeBox.EndThreadSave;
    end;
  finally
    if IncludePending then LNode.UnlockMempoolRead;
  end;
  CheckWalletQueryCancelled;
  LAccs.Sort(TAccountComparer.Create);
  CheckWalletQueryCancelled;
  Result := LAccs.ToArray;
end;

class function TCoreTool.GetUserAccountNumbers: TArray<cardinal>;
var
  i, LUniqueCount: integer;
  LAccs, LNumbers: TList<Cardinal>;
  LIndex: TOrderedAccountKeysList;
  LSafeBox: TPCSafeBox;
  LList: TAccountsNumbersList;
  Disposables: TDisposables;
begin
  CheckWalletQueryCancelled;
  LAccs := Disposables.AddObject(TList<Cardinal>.Create) as TList<Cardinal>;
  LNumbers := Disposables.AddObject(TList<Cardinal>.Create) as TList<Cardinal>;
  LIndex := TWallet.Keys.AccountsKeyList;
  LSafeBox := LIndex.SafeBox;
  if Assigned(LSafeBox) then LSafeBox.StartThreadSafe;
  try
    LIndex.Lock;
    try
      for i := 0 to LIndex.Count-1 do begin
        CheckWalletQueryCancelled;
        LList := LIndex.AccountKeyList[i];
        LList.FillList(0,LList.Count,LNumbers);
        LAccs.AddRange(LNumbers);
      end;
    finally
      LIndex.Unlock;
    end;
  finally
    if Assigned(LSafeBox) then LSafeBox.EndThreadSave;
  end;
  // Append each key's snapshot, then sort once. Inserting every account into
  // a sorted list would introduce another quadratic pass across many keys.
  CheckWalletQueryCancelled;
  LAccs.Sort;
  SetLength(Result,LAccs.Count);
  LUniqueCount := 0;
  for i := 0 to LAccs.Count-1 do begin
    CheckWalletQueryCancelled;
    if (LUniqueCount=0) or (LAccs[i]<>Result[LUniqueCount-1]) then begin
      Result[LUniqueCount] := LAccs[i];
      Inc(LUniqueCount);
    end;
  end;
  SetLength(Result,LUniqueCount);
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
  LMsg: string;
  LProgress: TProgressInfo;
  LDestBlock: cardinal;
begin
  LReady := Self.Bank.IsReady(LMsg);
  if LReady and TNetData.NetData.IsGettingNewBlockChainFromClient(LProgress) then
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
  LMemPool : TPCOperationsComp;
begin
  CheckWalletQueryCancelled;
  SetLength(Result, Length(AAccountNumbers));
  if AIncludePending then begin
    LMemPool := LockQueryMempool(Self);
    try
      for i := Low(AAccountNumbers) to High(AAccountNumbers) do begin
        CheckWalletQueryCancelled;
        Result[i] := LMemPool.SafeBoxTransaction.Account(AAccountNumbers[i]);
      end;
    finally
      Self.UnlockMempoolRead;
    end;
  end
  else
    for i := Low(AAccountNumbers) to High(AAccountNumbers) do begin
      CheckWalletQueryCancelled;
      Result[i] := Self.Bank.SafeBox.Account(AAccountNumbers[i]);
    end;
end;

class function TCoreTool.GetPendingOperationsAffectingAccounts(AOperations: TOperationsHashTree;
  ASnapshot: TPCSafeBoxTransaction; ABlock: Cardinal;
  const AAccountNumbers: array of Cardinal; ASkipCount, ATakeCount: Integer): TArray<TOperationResume>;
type
  TOperationIndexes = TList<Cardinal>;
var
  LByAccount: TObjectDictionary<Cardinal, TOperationIndexes>;
  LOperations: TList<TPCOperation>;
  LAffected: TOrderedList<Cardinal>;
  LIndexes: TOperationIndexes;
  LOps: TList<TOperationResume>;
  LOpResume: TOperationResume;
  LAccNo: Cardinal;
  LNumOps, LLastWanted, LBalance: Int64;
  i, j, LOperationIndex: Integer;
  LHasBalance: Boolean;
  GC: TDisposables;
begin
  CheckWalletQueryCancelled;
  Result := nil;
  if (Length(AAccountNumbers) = 0) or (ATakeCount <= 0) then Exit;
  LLastWanted := Int64(ASkipCount) + Int64(ATakeCount);
  if LLastWanted <= 0 then Exit;
  if not Assigned(AOperations) then
    raise EArgumentException.Create('Pending operations are required');
  if AOperations.OperationsCount = 0 then Exit;
  if not Assigned(ASnapshot) then
    raise EArgumentException.Create('A pending account snapshot is required');

  LByAccount := GC.AddObject(TObjectDictionary<Cardinal, TOperationIndexes>.Create([doOwnsValues])) as TObjectDictionary<Cardinal, TOperationIndexes>;
  LOperations := GC.AddObject(TList<TPCOperation>.Create) as TList<TPCOperation>;
  LAffected := GC.AddObject(TOrderedList<Cardinal>.Create(False, TComparison_Cardinal)) as TOrderedList<Cardinal>;
  LOps := GC.AddObject(TList<TOperationResume>.Create) as TList<TOperationResume>;
  for LAccNo in AAccountNumbers do begin
    CheckWalletQueryCancelled;
    if not LByAccount.ContainsKey(LAccNo) then
      LByAccount.Add(LAccNo, nil);
  end;

  AOperations.GetOperationsList(LOperations, False);
  // AffectedAccounts is evaluated once per operation, independently of the
  // number of wallet accounts. Store only matching account/index pairs.
  for i := 0 to LOperations.Count - 1 do begin
    CheckWalletQueryCancelled;
    LAffected.Clear;
    LOperations[i].AffectedAccounts(LAffected);
    for j := 0 to LAffected.Count - 1 do begin
      CheckWalletQueryCancelled;
      if LByAccount.TryGetValue(LAffected[j], LIndexes) then begin
        if not Assigned(LIndexes) then begin
          LIndexes := TOperationIndexes.Create;
          try
            LByAccount.AddOrSetValue(LAffected[j], LIndexes);
          except
            LIndexes.Free;
            raise;
          end;
        end;
        LIndexes.Add(Cardinal(i));
      end;
    end;
  end;

  LNumOps := 0;
  // Preserve the existing account-major order and per-account newest-first
  // order, including duplicate requested accounts and account-specific balances.
  for LAccNo in AAccountNumbers do begin
    CheckWalletQueryCancelled;
    LIndexes := LByAccount[LAccNo];
    LHasBalance := False;
    if Assigned(LIndexes) then
      for i := LIndexes.Count - 1 downto 0 do begin
        CheckWalletQueryCancelled;
        Inc(LNumOps);
        if (LNumOps > ASkipCount) and (LNumOps <= LLastWanted) then begin
          LOperationIndex := LIndexes[i];
          if TPCOperation.OperationToOperationResume(0, LOperations[LOperationIndex], False, LAccNo, LOpResume) then begin
            if not LHasBalance then begin
              LBalance := ASnapshot.Account(LAccNo).balance;
              LHasBalance := True;
            end;
            LOpResume.NOpInsideBlock := LOperationIndex;
            LOpResume.Block := ABlock;
            LOpResume.Balance := LBalance;
            LOps.Add(LOpResume);
          end;
        end;
        if LNumOps >= LLastWanted then begin
          CheckWalletQueryCancelled;
          Exit(LOps.ToArray);
        end;
      end;
  end;
  CheckWalletQueryCancelled;
  Result := LOps.ToArray;
end;

function TNodeHelper.GetPendingOperationsAffectingAccounts(const AAccountNumbers: array of Cardinal; ASkipCount, ATakeCount: Integer): TArray<TOperationResume>;
var
  LMemPool: TPCOperationsComp;
  LAccounts: TArray<Cardinal>;
  I, LCount: Integer;
begin
  Result := nil;
  if (Length(AAccountNumbers) = 0) or (ATakeCount <= 0) then
    Exit;
  LMemPool := LockQueryMempool(Self);
  try
    // Successful live admissions already record every affected account here.
    // Use that index before walking the pool, under the same pending-state lock.
    // Arbitrary operation trees still use the unfiltered TCoreTool helper.
    SetLength(LAccounts, Length(AAccountNumbers));
    LCount := 0;
    for I := 0 to High(AAccountNumbers) do
    begin
      CheckWalletQueryCancelled;
      if LMemPool.PreviousUpdatedBlocks.IndexOfAccount(AAccountNumbers[I]) >= 0 then
      begin
        LAccounts[LCount] := AAccountNumbers[I];
        Inc(LCount);
      end;
    end;
    if LCount = 0 then
      Exit;
    SetLength(LAccounts, LCount);
    Result := TCoreTool.GetPendingOperationsAffectingAccounts(LMemPool.OperationsHashTree,
      LMemPool.SafeBoxTransaction, LMemPool.OperationBlock.block,
      LAccounts, ASkipCount, ATakeCount);
  finally
    Self.UnlockMempoolRead;
  end;
end;

function TNodeHelper.GetStoredOperationsAffectingAccounts(const AAccountNumbers: array of cardinal; ABlockDepth: Cardinal; ASkipCount, ATakeCount: integer): TArray<TOperationResume>;
type
  __TList_Cardinal = TList<cardinal>;
var
  i: integer;
  LBlock, LBlockTip, LBlockEnd: Cardinal;
  LRelevantBlockOps: TList<Cardinal>;
  LOp: TPCOperation;
  LOpResume: TOperationResume;
  LFoundOps: TList<TOperationResume>;
  LOpsComp: TPCOperationsComp;
  LAccountBalances: TDictionary<cardinal, Int64>;
  LAccounts: TArray<TAccount>;
  LDisposables: TDisposables;
  LNumOps, LLastWanted: Int64;
  {$IFDEF FPC}
  LBlockTraversal: TSortedHashSet<cardinal>;
  {$ELSE}
  LBlockTraversal: TOrderedCardinalList;
  {$ENDIF}
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
    CheckWalletQueryCancelled;
    LOpsComp := LDisposables.AddObject(TPCOperationsComp.Create(nil)) as TPCOperationsComp;
    LRelevantBlockOps := LDisposables.AddObject(TList<Cardinal>.Create) as TList<Cardinal>;

    // load block
    if not Bank.Storage.LoadBlockChainBlock(LOpsComp, ABlockNum) then
    begin
      TLog.NewLog(ltdebug, ClassName, 'Block ' + IntToStr(ABlockNum) + ' not found. Cannot read operations');
      exit;
    end;

    // scan for each account
    for LAccNo in LAccountsToScanAtBlock[ABlockNum] do
    begin
      CheckWalletQueryCancelled;
      LRelevantBlockOps.Clear;
      LOpsComp.OperationsHashTree.GetOperationsAffectingAccount(LAccNo, LRelevantBlockOps);
      for i := LRelevantBlockOps.Count - 1 downto 0 do
      begin
        CheckWalletQueryCancelled;
        LOp := LOpsComp.Operation[LRelevantBlockOps.Items[i]];
        // Conversion encodes the block into operation hashes as well as Block.
        // Pass the stored height, never this account's local operation index.
        if TPCOperation.OperationToOperationResume(ABlockNum, LOp, False, LAccNo, LOpResume) then
        begin
          LOpResume.NOpInsideBlock := LRelevantBlockOps.Items[i];
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
          if (LNumOps > ASkipCount) and (LNumOps <= LLastWanted) then
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

  function GetAccountLastUpdateBlock({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AAccount: TAccount): cardinal;
  begin
    Result := AAccount.GetLastUpdatedBlock;
  end;

begin
  CheckWalletQueryCancelled;
  Result := nil;
  if (Length(AAccountNumbers) = 0) or (ATakeCount <= 0) then
    Exit;
  LLastWanted := Int64(ASkipCount) + Int64(ATakeCount);
  if LLastWanted <= 0 then
    Exit;
  // Init
  LNumOps := 0;
  {$IFDEF FPC}
  LBlockTraversal := LDisposables.AddObject(TSortedHashSet<cardinal>.Create(TComparerTool<cardinal>.Inverted(TComparer<cardinal>.Default))) as TSortedHashSet<cardinal>;
  {$ELSE}
  LBlockTraversal := LDisposables.AddObject(TOrderedCardinalList.Create) as TOrderedCardinalList;
  {$ENDIF}
  LAccountsToScanAtBlock := LDisposables.AddObject(TObjectDictionary<cardinal, __TList_Cardinal>.Create([doOwnsValues])) as TObjectDictionary<cardinal, __TList_Cardinal>;
  LFoundOps := LDisposables.AddObject(TList<TOperationResume>.Create) as TList<TOperationResume>;
  // Money and the undetermined (-1) history sentinel use signed 64-bit values,
  // matching TOperationResume.Balance while walking older operations backwards.
  LAccountBalances := LDisposables.AddObject(TDictionary<cardinal, Int64>.Create) as TDictionary<cardinal, Int64>;
  LBlockTip := Self.BlockTip;
  // Saturate before subtracting unsigned heights. A full-history request can
  // exceed the chain length; it must include genesis rather than wrap around.
  if ABlockDepth = 0 then
    LBlockEnd := LBlockTip
  else if ABlockDepth >= LBlockTip then
    LBlockEnd := 0
  else
    LBlockEnd := LBlockTip - ABlockDepth;
  // First get all accounts, their balances and initial traversal set
  LAccounts := Self.GetAccounts(AAccountNumbers, False);
  for i := Low(LAccounts) to High(LAccounts) do
  begin
    CheckWalletQueryCancelled;
    // if account is modified in block-tip
    LAcc := LAccounts[i];
    // Accounts older than this window cannot produce a matching linked block.
    // Exclude them before building balance/frontier dictionaries, rather than
    // allocating and sorting entries which the traversal will only discard.
    if LAcc.GetLastUpdatedBlock < LBlockEnd then
      Continue;
    LAccountBalances.AddOrSetValue(LAcc.account, LAcc.Balance);  // track account balances
    LBlockTraversal.Add(LAcc.GetLastUpdatedBlock);
    MarkAccountAsScannableAtBlock(LAcc.account, LAcc.GetLastUpdatedBlock);
  end;

  // Traverse the set of "last updated" blocks in DESCENDING order
  while LBlockTraversal.Count > 0 do
  begin
    CheckWalletQueryCancelled;
    {$IFDEF FPC}
    LBlock := TSortedHashSetTool<cardinal>.Pop(LBlockTraversal);
    {$ELSE}
    LBlock := LBlockTraversal.Get(LBlockTraversal.Count - 1);
    LBlockTraversal.Remove(LBlock);
    {$ENDIF}
    if LBlock < LBlockEnd then
      continue;
    ScanBlock(LBlock);   // note: this will update LBlockTraversals with prev updated blocks, so loops until finished
    if LFoundOps.Count >= ATakeCount then
    begin
      // Return a completed finite page before releasing its scoped list.
      Result := LFoundOps.ToArray;
      Exit;
    end;
  end;

  // return array result
  CheckWalletQueryCancelled;
  Result := LFoundOps.ToArray;
end;

{ TAccountComparer }

function TAccountComparer.Compare({$IFDEF FPC}constref{$ELSE}const{$ENDIF} ALeft, ARight: TAccount): integer;
begin
  Result := TAccountComparer.DoCompare(ALeft, ARight);
end;

class function TAccountComparer.DoCompare({$IFDEF FPC}constref{$ELSE}const{$ENDIF} ALeft, ARight: TAccount): integer;
begin
  {$IFDEF FPC}
  Result := TCompare.UInt64(ALeft.account, ARight.account);
  {$ELSE}
  Result := TComparer<Cardinal>.Default.Compare(ALeft.account, ARight.account);
  {$ENDIF}
end;

{ TAccountEqualityComparer }

function TAccountEqualityComparer.Equals({$IFDEF FPC}constref{$ELSE}const{$ENDIF} ALeft, ARight: TAccount): boolean;
begin
  Result := TAccountEqualityComparer.AreEqual(ALeft, ARight);
end;

function TAccountEqualityComparer.GetHashCode({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AValue: TAccount): TAccountHashCode;
begin
  {$IFDEF FPC}
  Result := TAccountEqualityComparer.CalcHashCode(AValue);
  {$ELSE}
  Result := Integer(TAccountEqualityComparer.CalcHashCode(AValue));
  {$ENDIF}
end;

class function TAccountEqualityComparer.AreEqual({$IFDEF FPC}constref{$ELSE}const{$ENDIF} ALeft, ARight: TAccount): boolean;
begin
  Result :=
    (ALeft.account = ARight.account) and
    (ALeft.balance = ARight.balance) and
    (ALeft.GetLastUpdatedBlock = ARight.GetLastUpdatedBlock) and
    (ALeft.n_operation = ARight.n_operation) and
    TAccountKeyEqualityComparer.AreEqual(ALeft.accountInfo.accountKey, ARight.accountInfo.accountKey);
end;

class function TAccountEqualityComparer.CalcHashCode({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AValue: TAccount): UInt32;
begin
  Result := AValue.account;
end;

{ TAccountKeyComparer }

function TAccountKeyComparer.Compare({$IFDEF FPC}constref{$ELSE}const{$ENDIF} ALeft, ARight: TAccountKey): integer;
begin
  Result := TAccountKeyComparer.DoCompare(ALeft, ARight);
end;

class function TAccountKeyComparer.DoCompare({$IFDEF FPC}constref{$ELSE}const{$ENDIF} ALeft, ARight: TAccountKey): integer;
begin
  Result := BytesCompare(ALeft.x, ARight.x);
  if Result = 0 then
    Result := BytesCompare(ALeft.y, ARight.y);
end;

{ TAccountKeyEqualityComparer }

function TAccountKeyEqualityComparer.Equals({$IFDEF FPC}constref{$ELSE}const{$ENDIF} ALeft, ARight: TAccountKey): boolean;
begin
  Result := TAccountKeyEqualityComparer.AreEqual(ALeft, ARight);
end;

function TAccountKeyEqualityComparer.GetHashCode({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AValue: TAccountKey): TAccountHashCode;
begin
  {$IFDEF FPC}
  Result := TAccountKeyEqualityComparer.CalcHashCode(AValue);
  {$ELSE}
  Result := Integer(TAccountKeyEqualityComparer.CalcHashCode(AValue));
  {$ENDIF}
end;

class function TAccountKeyEqualityComparer.AreEqual({$IFDEF FPC}constref{$ELSE}const{$ENDIF} ALeft, ARight: TAccountKey): boolean;
begin
  Result := TAccountKeyComparer.DoCompare(ALeft, ARight) = 0;
end;

class function TAccountKeyEqualityComparer.CalcHashCode({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AValue: TAccountKey): UInt32;
begin
  // Delphi string hashes are signed; retain their bits in the unsigned contract.
  Result := UInt32(TEqualityComparer<String>.Default.GetHashCode(IntToStr(AValue.EC_OpenSSL_NID) + AValue.x.ToString + AValue.y.ToString));
end;

{ TAccountHelper }

function TAccountHelper.GetAccountString: String;
begin
  Result := TAccountComp.AccountNumberToAccountTxtNumber(Self.account);
end;

function TAccountHelper.GetDisplayString: String;
begin
  Result := GetAccountString;
  if Self.Name <> nil then
    Result := Result + ': ' + Self.Name.ToString;
end;

function TAccountHelper.GetInfoText(const ABank: TPCBank): String;
begin
  Result := GetInfoText(ABank.BlocksCount);
end;

function TAccountHelper.GetInfoText(ABlocksCount: Cardinal): String;
var
  builder: TStrings;
  GC: TDisposables;
  json: TPCJSONObject;
begin
  builder := GC.AddObject(TStringList.Create) as TStrings;
  builder.Append(Format('Account: %s %s Type:%d', [TAccountComp.AccountNumberToAccountTxtNumber(self.account), IIF(Self.Name <> nil, 'Name: ' + Self.Name.ToString, ''), Self.account_type]));
  builder.Append('');
  builder.Append(Format('Current balance: %s', [TAccountComp.FormatMoney(Self.balance)]));
  builder.Append('');
  builder.Append(Format('Passively updated on block: %d  (%d blocks ago)', [Self.updated_on_block_passive_mode, Int64(ABlocksCount) - Self.updated_on_block_passive_mode]));
  builder.Append(Format('Actively updated on block: %d  (%d blocks ago)', [Self.updated_on_block_active_mode, Int64(ABlocksCount) - Self.updated_on_block_active_mode]));
  builder.Append(Format('Public key type: %s', [TAccountComp.GetECInfoTxt(Self.accountInfo.accountKey.EC_OpenSSL_NID)]));
  builder.Append(Format('Base58 Public key: %s', [TAccountComp.AccountPublicKeyExport(Self.accountInfo.accountKey)]));
  if Length(Self.account_data) > 0 then
    builder.Append('Account Data: ' + Self.account_data.ToHexaString)
  else
    builder.Append('Account Data: (No data)');
  builder.Append('Account Seal: ' + Self.account_seal.ToHexaString);
  if TAccountComp.IsAccountForSale(Self.accountInfo) then
  begin
    builder.Append('');
    builder.Append('** Account is for sale: **');
    builder.Append(Format('Price: %s', [TAccountComp.FormatMoney(Self.accountInfo.price)]));
    builder.Append(Format('Seller account (where to pay): %s', [TAccountComp.AccountNumberToAccountTxtNumber(Self.accountInfo.account_to_pay)]));
    if TAccountComp.IsAccountForPrivateSale(Self.accountInfo) then
    begin
      builder.Append('');
      builder.Append('** Private sale **');
      builder.Append(Format('New Base58 Public key: %s', [TAccountComp.AccountPublicKeyExport(Self.accountInfo.new_publicKey)]));
      builder.Append('');
      if TAccountComp.IsAccountLocked(Self.accountInfo, ABlocksCount) then
        builder.Append(Format('PURCHASE IS SECURE UNTIL BLOCK %d (current %d, remains %d)',
          [Self.accountInfo.locked_until_block, ABlocksCount, Self.accountInfo.locked_until_block - ABlocksCount]))
      else
        builder.Append(Format('PURCHASE IS NOT SECURE (Expired on block %d, current %d)',
          [Self.accountInfo.locked_until_block, ABlocksCount]));
    end;
  end;
  if TAccountComp.IsAccountForSwap(Self.accountInfo) then begin
    builder.Append('');
    if TAccountComp.IsAccountForAccountSwap(Self.accountInfo) then begin
      builder.Append('** Account is for Atomic Account Swap: **');
      builder.Append('New Base58 Public key: ' + TAccountComp.AccountPublicKeyExport(Self.accountInfo.new_publicKey));
    end else begin
      builder.Append('** Account is for Atomic Coin Swap: **');
      builder.Append('Amount to swap: ' + TAccountComp.FormatMoney(Self.accountInfo.price));
      builder.Append('Counterparty account: ' + TAccountComp.AccountNumberToAccountTxtNumber(Self.accountInfo.account_to_pay));
    end;
    builder.Append('Public secret to find: ' + Self.accountInfo.hashed_secret.ToHexaString);
    builder.Append(Format('Locked until block: %d (current %d)', [Self.accountInfo.locked_until_block, ABlocksCount]));
  end;
  // Reuse the same canonical account JSON as the classic wallet's info dialog.
  json := GC.AddObject(TPCJSONObject.Create) as TPCJSONObject;
  TPascalCoinJSONComp.FillAccountObject(Self, json);
  builder.Append('ACCOUNT JSON:');
  builder.Append(json.ToJSON(false));
  Result := builder.Text;
end;

{ TOperationResumeHelper }

function TOperationResumeHelper.GetPrintableOPHASH: String;
begin
  Result := TCrypto.ToHexaString(Self.OperationHash);
end;

function TOperationResumeHelper.GetInfoText(const ABank: TPCBank): String;
begin
  Result := GetInfoText(ABank.BlocksCount);
end;

function TOperationResumeHelper.GetInfoText(ABlocksCount: Cardinal;
  const ANode: TNode; const AWalletKeys: TWalletKeys): String;
var
  builder: TStrings;
  GC: TDisposables;
  json: TPCJSONObject;
begin
  if (not Self.valid) then
    exit;
  builder := GC.AddObject(TStringList.Create) as TStrings;
  if Self.Block < ABlocksCount then
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
  if Self.isMultiOperation then
    builder.Add(Format('Multioperation: %d senders, %d receivers, %d account changes',
      [Length(Self.Senders), Length(Self.Receivers), Length(Self.Changers)]));
  builder.Add(Format('Operation Hash (ophash): %s', [TCrypto.ToHexaString(Self.OperationHash)]));
  if (Self.OperationHash_OLD <> nil) then
    builder.Add(Format('Old Operation Hash (old_ophash): %s', [TCrypto.ToHexaString(Self.OperationHash_OLD)]));
  if (Self.OriginalPayload.payload_raw <> nil) then
  begin
    builder.Add(Format('Payload length:%d', [length(Self.OriginalPayload.payload_raw)]));
    if Self.PrintablePayload <> '' then
      builder.Add(Format('Payload (human): %s', [Self.PrintablePayload]));
    builder.Add(Format('Payload (Hexadecimal): %s', [TCrypto.ToHexaString(Self.OriginalPayload.payload_raw)]));
  end;
  if Self.Balance >= 0 then
    builder.Add(Format('Final balance: %s', [TAccountComp.FormatMoney(Self.Balance)]));
  // Context-free callers stay offline; the UI can supply its loaded node/keys
  // for the same EPASA/payload information shown by the classic wallet.
  json := GC.AddObject(TPCJSONObject.Create) as TPCJSONObject;
  TPascalCoinJSONComp.FillOperationObject(Self, ABlocksCount, ANode, AWalletKeys, nil, json);
  builder.Add('OPERATION JSON:');
  builder.Add(json.ToJSON(false));
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
  LAuxs, LOperationsTxt, LPrompt, LDetails: string;
  i: integer;
begin
  Result := False;
  AErrorMessage := '';
  LAuxs := 'Total amount that dest will receive: ' +
    TAccountComp.FormatMoney(ATotalAmount) + #10 +
    'Total fee: ' + TAccountComp.FormatMoney(ATotalFee);
  if (ANoOfOperations > 1) then
  begin
    LPrompt := 'Execute ' + IntToStr(ANoOfOperations) + ' operations?';
    LDetails := 'Operation: ' + AOperationsTxt;
    if AOperationToString <> AOperationsTxt then
      LDetails := LDetails + #10 + #10 + AOperationToString;
  end
  else begin
    LPrompt := 'Execute this operation:' + #10 +
      'Number of operations: ' + IntToStr(ANoOfOperations);
    LDetails := AOperationToString;
  end;
  LPrompt := LPrompt + #10 + LAuxs + #10 + #10 +
    'Note: This operation will be transmitted to the network!';
  // Keep totals and the network notice visible while the complete list scrolls.
  if not ConfirmMemoQuery(Screen.ActiveForm, Application.Title, LPrompt, LDetails) then
    Exit;
  Result := True;
  i := TNode.Node.AddOperations(nil, AOperationsHashTree, nil, AErrorMessage);
  if (i = AOperationsHashTree.OperationsCount) then
  begin
    LOperationsTxt := 'Successfully executed ' + IntToStr(i) +
      ' operations!' + #10 + LAuxs;
    ShowResultMemo(Screen.ActiveForm, Application.Title, LOperationsTxt,
      AOperationToString);
  end
  else if (i > 0) then
  begin
    LOperationsTxt := 'One or more of your operations has not been executed:' + #10 +
      'Total successfully executed operations: ' + IntToStr(i);
    ShowResultMemo(Screen.ActiveForm, Application.Title, LOperationsTxt,
      'Errors:' + #10 + AErrorMessage);
  end
  else
    Result := False;
end;

class function TWIZOperationsHelper.UpdatePayload(const ASenderPublicKey, ADestinationPublicKey: TAccountKey; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent: string; var AEncodedPayloadBytes: TRawBytes; const APayloadEncryptionPassword: string; var AErrorMessage: string): boolean;
var
  LValid: boolean;
begin
  AEncodedPayloadBytes := nil;
  AErrorMessage := '';
  if (APayloadContent = '') then
    Exit(True);

  LValid := False;
  AErrorMessage := 'An Error Occured During Payload Encryption.';

  try

    case APayloadEncryptionMode of

      pemEncryptWithSender:
      begin
        // Use sender public key
        TPCEncryption.DoPascalCoinECIESEncrypt(ASenderPublicKey, TEncoding.ANSI.GetBytes(APayloadContent), AEncodedPayloadBytes);
        LValid := AEncodedPayloadBytes <> nil;
      end;

      pemEncryptWithRecipient:
      begin
        // With destination public key
        TPCEncryption.DoPascalCoinECIESEncrypt(ADestinationPublicKey, TEncoding.ANSI.GetBytes(APayloadContent), AEncodedPayloadBytes);
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
        AEncodedPayloadBytes := TPCEncryption.DoPascalCoinAESEncrypt(
          TEncoding.ANSI.GetBytes(APayloadContent),
          TEncoding.ANSI.GetBytes(APayloadEncryptionPassword)
        );
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
    if LValid then
      AErrorMessage := ''
    else
      AEncodedPayloadBytes := nil;
    Result := LValid;
  end;
end;

class function TWIZOperationsHelper.UpdatePayload(const ASenderPublicKey, ADestinationPublicKey: TAccountKey; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent: string; var AOperationPayload: TOperationPayload; const APayloadEncryptionPassword: string; var AErrorMessage: string): boolean;
begin
  AOperationPayload := CT_TOperationPayload_NUL;
  result := UpdatePayload( ASenderPublicKey, ADestinationPublicKey, APayloadEncryptionMode, APayloadContent, AOperationPayload.payload_raw, APayloadEncryptionPassword, AErrorMessage );
end;

class function TWIZOperationsHelper.OthersFinalizeAndDisplayMessage(const AOperationsTxt, AOperationToString: string; ANoOfOperations: integer; ATotalFee: int64; AOperationsHashTree: TOperationsHashTree; var AErrorMessage: string): boolean;
var
  LAuxs, LOperationsTxt, LPrompt, LDetails: string;
  i: integer;
begin
  Result := False;
  AErrorMessage := '';
  LAuxs := 'Total fee: ' + TAccountComp.FormatMoney(ATotalFee);
  if (ANoOfOperations > 1) then
  begin
    LPrompt := 'Execute ' + IntToStr(ANoOfOperations) + ' operations?';
    LDetails := 'Operation: ' + AOperationsTxt;
    if AOperationToString <> AOperationsTxt then
      LDetails := LDetails + #10 + #10 + AOperationToString;
  end
  else begin
    LPrompt := 'Execute this operation:' + #10 +
      'Number of operations: ' + IntToStr(ANoOfOperations);
    LDetails := AOperationToString;
  end;
  LPrompt := LPrompt + #10 + LAuxs + #10 + #10 +
    'Note: This operation will be transmitted to the network!';
  // Use the same bounded presentation for one operation and large batches.
  if not ConfirmMemoQuery(Screen.ActiveForm, Application.Title, LPrompt, LDetails) then
    Exit;
  Result := True;
  i := TNode.Node.AddOperations(nil, AOperationsHashTree, nil, AErrorMessage);
  if (i = AOperationsHashTree.OperationsCount) then
  begin
    LOperationsTxt := 'Successfully executed ' + IntToStr(i) +
      ' operations!' + #10 + LAuxs;
    ShowResultMemo(Screen.ActiveForm, Application.Title, LOperationsTxt,
      AOperationToString);
  end
  else if (i > 0) then
  begin
    LOperationsTxt := 'One or more of your operations has not been executed:' + #10 +
      'Total successfully executed operations: ' + IntToStr(i);
    ShowResultMemo(Screen.ActiveForm, Application.Title, LOperationsTxt,
      'Errors:' + #10 + AErrorMessage);
  end
  else
    Result := False;
end;

class function TWIZOperationsHelper.GetChangeAccountInfoTargets(
  const AAccounts: TArray<TAccount>; ANewType: Word): TArray<TAccount>;
var
  I, LCount: Integer;
begin
  if Length(AAccounts) <= 1 then
    Exit(TArrayTool<TAccount>.Copy(AAccounts));
  SetLength(Result, Length(AAccounts));
  LCount := 0;
  for I := 0 to High(AAccounts) do
    if AAccounts[I].account_type <> ANewType then
    begin
      Result[LCount] := AAccounts[I];
      Inc(LCount);
    end;
  SetLength(Result, LCount);
end;

class function TWIZOperationsHelper.PrepareOperations(const ARequest: TWalletOperationRequest; AProtocol: Word; ABlocksCount: Cardinal; AWalletKeys: TWalletKeys; out AOperations: TOperationsHashTree; var AErrorMessage: string): Boolean;
var
  LAccounts, LChangeInfoTargets: TArray<TAccount>;
  LStates: TDictionary<Cardinal, TAccount>;
  LDisposables: TDisposables;
  LTree: TOperationsHashTree;
  LOperation: TPCOperation;
  LAccount, LSigner, LDestination: TAccount;
  LWalletKey: TWalletKey;
  LPayload: TOperationPayload;
  LNewKey: TAccountKey;
  LNewName: TRawBytes;
  LAmount: Int64;
  LLock: Cardinal;
  LChangeName, LChangeType: Boolean;
  i, j: Integer;

  function Fail(const AMessage: string): Boolean;
  begin
    AErrorMessage := AMessage;
    Result := False;
  end;

  function IsUsable(const AAccount: TAccount): Boolean;
  begin
    Result := False;
    if AAccount.balance > CT_MaxWalletAmount then
      Exit(Fail('The account balance exceeds the protocol limit'));
    if TAccountComp.IsAccountBlockedByProtocol(AAccount.account, ABlocksCount) then
      Exit(Fail(Format('Account %s is blocked by the current protocol', [AAccount.AccountString])));
    if TAccountComp.IsAccountLocked(AAccount.accountInfo, ABlocksCount) then
      Exit(Fail(Format('Account %s is locked until block %u', [AAccount.AccountString, AAccount.accountInfo.locked_until_block])));
    Result := True;
  end;

  procedure Remember(const AAccount: TAccount);
  begin
    // A repeated signer must use the state resulting from earlier operations.
    if not LStates.ContainsKey(AAccount.account) then
      LStates.Add(AAccount.account, AAccount);
  end;

  function AppendOperation(AOperation: TPCOperation): Boolean;
  var
    LOperationDisposables: TDisposables;
  begin
    LOperationDisposables.AddObject(AOperation);
    Result := LTree.AddOperationToHashTree(AOperation);
    if not Result then
      Fail('The prepared operation conflicts with another operation in the batch');
  end;

begin
  Result := False;
  AOperations := nil;
  AErrorMessage := '';
  if Length(ARequest.Accounts) = 0 then
    Exit(Fail('No selected account found'));
  if not Assigned(AWalletKeys) then
    Exit(Fail('No wallet keys found'));
  if not (ARequest.OperationType in [omtSendPasc, omtChangeKey, omtEnlistAccountForSale, omtDelistAccountFromSale, omtChangeInfo, omtBuyAccount]) then
    Exit(Fail('Unsupported wallet operation'));
  if (ARequest.Fee < 0) or (ARequest.Fee > CT_MaxTransactionFee) then
    Exit(Fail('Invalid operation fee'));
  if (ARequest.OperationType <> omtSendPasc) and (AProtocol < CT_PROTOCOL_2) and
    (ARequest.OperationType <> omtChangeKey) then
    Exit(Fail('This operation requires protocol 2'));
  if (ARequest.OperationType in [omtSendPasc, omtBuyAccount]) and
    ((ARequest.Amount < 0) or (ARequest.Amount > CT_MaxTransactionAmount)) then
    Exit(Fail('Invalid operation amount'));
  if (ARequest.OperationType = omtSendPasc) and (ARequest.SendMode = akaSpecifiedAmount) and (ARequest.Amount = 0) then
    Exit(Fail('The amount to send must be greater than zero'));
  if (ARequest.OperationType = omtBuyAccount) and (Length(ARequest.Accounts) <> 1) then
    Exit(Fail('Buying an account requires exactly one funding account'));
  if (ARequest.OperationType in [omtChangeKey, omtEnlistAccountForSale, omtDelistAccountFromSale, omtChangeInfo]) and
    (Length(ARequest.SignerAccounts) <> 1) and (Length(ARequest.SignerAccounts) <> Length(ARequest.Accounts)) then
    Exit(Fail('A signer is required for every selected account'));
  if (ARequest.OperationType in [omtChangeKey, omtEnlistAccountForSale, omtDelistAccountFromSale]) and
    (Length(ARequest.SignerAccounts) <> 1) then
    Exit(Fail('Choose one common signer for this operation'));
  if (ARequest.OperationType = omtEnlistAccountForSale) and
    ((ARequest.SalePrice <= 0) or (ARequest.SalePrice > CT_MaxTransactionAmount)) then
    Exit(Fail('The sale price must be greater than zero and within the protocol limit'));

  if (ARequest.OperationType = omtChangeInfo) and
    (Length(ARequest.ApprovedChangeInfoTargets) > 0) then
  begin
    // Finish refreshes pending accounts. Do not silently change the reviewed
    // skip set if another operation changed an account while the wizard waited.
    LChangeInfoTargets := GetChangeAccountInfoTargets(ARequest.Accounts, ARequest.NewType);
    if Length(LChangeInfoTargets) <> Length(ARequest.ApprovedChangeInfoTargets) then
      Exit(Fail('The accounts to change have changed. Return to Details to review the skipped accounts again.'));
    for i := 0 to High(LChangeInfoTargets) do
      if LChangeInfoTargets[i].account <> ARequest.ApprovedChangeInfoTargets[i].account then
        Exit(Fail('The accounts to change have changed. Return to Details to review the skipped accounts again.'));
  end;

  LAccounts := TArrayTool<TAccount>.Copy(ARequest.Accounts);
  // Changing or locking a common signer's own account must be the last action.
  // Work on a copy: the caller's selected-account order belongs to the UI.
  if (Length(ARequest.SignerAccounts) = 1) and
    (ARequest.OperationType in [omtChangeKey, omtEnlistAccountForSale]) then
    for i := 0 to High(LAccounts) - 1 do
      if LAccounts[i].account = ARequest.SignerAccounts[0].account then
      begin
        TArrayTool<TAccount>.Swap(LAccounts, i, High(LAccounts));
        Break;
      end;

  LStates := LDisposables.AddObject(TDictionary<Cardinal, TAccount>.Create) as TDictionary<Cardinal, TAccount>;
  LTree := LDisposables.AddObject(TOperationsHashTree.Create) as TOperationsHashTree;
  for i := 0 to High(LAccounts) do
  begin
    if LStates.ContainsKey(LAccounts[i].account) then
      Exit(Fail('A selected account occurs more than once'));
    LStates.Add(LAccounts[i].account, LAccounts[i]);
  end;
  for i := 0 to High(ARequest.SignerAccounts) do
    Remember(ARequest.SignerAccounts[i]);
  Remember(ARequest.DestinationAccount);

  for i := 0 to High(LAccounts) do
  begin
    LAccount := LStates[LAccounts[i].account];
    // Skipping this target creates no operation, fee or nonce use of its own.
    // It may still be the chosen payer for another target. Preserve batch name rules.
    if (ARequest.OperationType = omtChangeInfo) and (Length(LAccounts) > 1) and
      (LAccount.account_type = ARequest.NewType) then
      Continue;
    if not IsUsable(LAccount) then
      Exit;
    LSigner := LAccount;
    if ARequest.OperationType in [omtChangeKey, omtEnlistAccountForSale, omtDelistAccountFromSale, omtChangeInfo] then
    begin
      if Length(ARequest.SignerAccounts) = 1 then
        LSigner := LStates[ARequest.SignerAccounts[0].account]
      else
      begin
        // Resolve by the original target index after any signer-last reorder.
        for j := 0 to High(ARequest.Accounts) do
          if ARequest.Accounts[j].account = LAccount.account then
            LSigner := LStates[ARequest.SignerAccounts[j].account];
      end;
      if (ARequest.SigningMode = akaSecondary) and (LAccount.balance >= UInt64(ARequest.Fee)) then
        LSigner := LAccount;
      if (AProtocol < CT_PROTOCOL_2) and (ARequest.OperationType = omtChangeKey) then
        LSigner := LAccount;
      if not IsUsable(LSigner) then
        Exit;
      if not TAccountComp.EqualAccountKeys(LAccount.accountInfo.accountKey, LSigner.accountInfo.accountKey) then
        Exit(Fail(Format('Signer %s does not own account %s', [LSigner.AccountString, LAccount.AccountString])));
    end;
    if not IsOwnerOfWallet(LSigner, AWalletKeys, LWalletKey, AErrorMessage) then
      Exit;
    if LSigner.n_operation = High(Cardinal) then
      Exit(Fail('The signer operation counter is exhausted'));
    if LSigner.balance < UInt64(ARequest.Fee) then
      Exit(Fail(Format('Insufficient funds in signer %s for all operation fees', [LSigner.AccountString])));

    LDestination := LSigner;
    LAmount := 0;
    LNewKey := ARequest.PublicKey;
    LLock := ARequest.LockedUntilBlock;
    LChangeName := False;
    LChangeType := False;
    LNewName := nil;

    case ARequest.OperationType of
      omtSendPasc:
        begin
          LDestination := LStates[ARequest.DestinationAccount.account];
          if LAccount.account = LDestination.account then
            Exit(Fail('Sender and destination accounts are the same'));
          if TAccountComp.IsAccountBlockedByProtocol(LDestination.account, ABlocksCount) then
            Exit(Fail('The destination account is blocked by the current protocol'));
          case ARequest.SendMode of
            akaSpecifiedAmount: LAmount := ARequest.Amount;
            akaAllBalance:
              begin
                if LAccount.balance = 0 then
                  Continue;
                if LAccount.balance <= UInt64(ARequest.Fee) then
                  Exit(Fail('The sender balance does not cover the fee and a positive amount'));
                LAmount := LAccount.balance - UInt64(ARequest.Fee);
              end;
          else
            Exit(Fail('Invalid send mode'));
          end;
          if (LAmount > CT_MaxTransactionAmount) or (UInt64(LAmount) > LAccount.balance - UInt64(ARequest.Fee)) then
            Exit(Fail(Format('Insufficient funds in account %s for the amount and fee', [LAccount.AccountString])));
          if (LDestination.balance > CT_MaxWalletAmount) or (UInt64(LAmount) > CT_MaxWalletAmount - LDestination.balance) then
            Exit(Fail('The destination balance would exceed the protocol limit'));
        end;
      omtChangeKey:
        begin
          if not TAccountComp.IsValidAccountKey(LNewKey, AProtocol, AErrorMessage) then
            Exit;
          if TAccountComp.EqualAccountKeys(LNewKey, LAccount.accountInfo.accountKey) then
            Exit(Fail('The new public key is the same as the current key'));
        end;
      omtEnlistAccountForSale:
        begin
          if TAccountComp.IsAccountForSaleOrSwap(LAccount.accountInfo) then
            Exit(Fail('The account is already listed for sale or swap'));
          if ARequest.SellerAccount.account = LAccount.account then
            Exit(Fail('The seller account cannot be the account being sold'));
          if TAccountComp.IsAccountBlockedByProtocol(ARequest.SellerAccount.account, ABlocksCount) then
            Exit(Fail('The seller account is blocked by the current protocol'));
          case ARequest.SaleMode of
            akaPublicSale:
              begin
                LNewKey := CT_TECDSA_Public_Nul;
                LLock := 0;
              end;
            akaPrivateSale:
              begin
                if not TAccountComp.IsValidAccountKey(LNewKey, AProtocol, AErrorMessage) then
                  Exit;
                if TAccountComp.EqualAccountKeys(LNewKey, LAccount.accountInfo.accountKey) then
                  Exit(Fail('The buyer public key is the current owner key'));
                if (LLock <= ABlocksCount) or (UInt64(LLock) > UInt64(ABlocksCount) + CT_MaxFutureBlocksLockedAccount) then
                  Exit(Fail('The private sale requires a valid future locking block'));
              end;
          else
            Exit(Fail('Invalid sale mode'));
          end;
        end;
      omtDelistAccountFromSale:
        if not TAccountComp.IsAccountForSale(LAccount.accountInfo) then
          Exit(Fail('The account is not listed for sale'));
      omtChangeInfo:
        begin
          if Length(LAccounts) = 1 then
          begin
            LNewName := ARequest.NewName;
            LChangeName := not TBaseType.Equals(LNewName, LAccount.name);
            if LChangeName and (Length(LNewName) > 0) and
              not TPascalCoinProtocol.IsValidAccountName(AProtocol, LNewName, AErrorMessage) then
              Exit;
          end;
          LChangeType := ARequest.NewType <> LAccount.account_type;
          if not LChangeName and not LChangeType then
            Exit(Fail('The account name and type have not changed'));
        end;
      omtBuyAccount:
        begin
          LDestination := ARequest.DestinationAccount;
          if LAccount.account = LDestination.account then
            Exit(Fail('Choose a separate funding account for this purchase'));
          if not TAccountComp.IsAccountForSale(LDestination.accountInfo) then
            Exit(Fail('The account to buy is not listed for sale'));
          if TAccountComp.IsAccountBlockedByProtocol(LDestination.account, ABlocksCount) then
            Exit(Fail('The account to buy is blocked by the current protocol'));
          if (AProtocol >= CT_PROTOCOL_5) and not TAccountComp.IsAccountForPublicSale(LDestination.accountInfo) and
            not TAccountComp.IsAccountLocked(LDestination.accountInfo, ABlocksCount) then
            Exit(Fail('The private sale locking period has expired'));
          if not TAccountComp.IsValidAccountKey(LNewKey, AProtocol, AErrorMessage) or
            not TAccountComp.IsValidNewAccountKey(LDestination.accountInfo, LNewKey, AProtocol) then
            Exit(Fail('The new owner public key does not satisfy the sale'));
          LAmount := ARequest.Amount;
          if (LAmount <= 0) or (UInt64(LAmount) > LAccount.balance - UInt64(ARequest.Fee)) then
            Exit(Fail('Insufficient funds for the purchase amount and fee'));
          // The purchased account may already hold part of its sale price.
          if (LDestination.balance > CT_MaxWalletAmount) or (UInt64(LAmount) > CT_MaxWalletAmount - LDestination.balance) then
            Exit(Fail('The purchase would exceed the account balance limit'));
          if LDestination.balance + UInt64(LAmount) < LDestination.accountInfo.price then
            Exit(Fail('The purchase amount and existing account balance do not cover its price'));
          // The recipient in the purchase wizard is the new owner. Encrypting
          // to the former owner would disclose its private recipient payload.
          LDestination.accountInfo.accountKey := LNewKey;
        end;
    end;

    if ARequest.OperationType = omtChangeKey then
      LDestination.accountInfo.accountKey := LNewKey;
    if not UpdatePayload(LSigner.accountInfo.accountKey, LDestination.accountInfo.accountKey,
      ARequest.PayloadMode, ARequest.PayloadContent, LPayload, ARequest.PayloadPassword, AErrorMessage) then
      Exit;

    LOperation := nil;
    case ARequest.OperationType of
      omtSendPasc:
        LOperation := TOpTransaction.CreateTransaction(AProtocol, LSigner.account, LSigner.n_operation + 1,
          LDestination.account, LWalletKey.PrivateKey, LAmount, ARequest.Fee, LPayload);
      omtChangeKey:
        if AProtocol >= CT_PROTOCOL_2 then
          LOperation := TOpChangeKeySigned.Create(AProtocol, LSigner.account, LSigner.n_operation + 1,
            LAccount.account, LWalletKey.PrivateKey, LNewKey, ARequest.Fee, LPayload)
        else
          LOperation := TOpChangeKey.Create(AProtocol, LSigner.account, LSigner.n_operation + 1,
            LAccount.account, LWalletKey.PrivateKey, LNewKey, ARequest.Fee, LPayload);
      omtEnlistAccountForSale:
        LOperation := TOpListAccountForSaleOrSwap.CreateListAccountForSaleOrSwap(AProtocol, as_ForSale,
          LSigner.account, LSigner.n_operation + 1, LAccount.account, ARequest.SalePrice, ARequest.Fee,
          ARequest.SellerAccount.account, LNewKey, LLock, LWalletKey.PrivateKey, CT_HashLock_NUL, LPayload);
      omtDelistAccountFromSale:
        LOperation := TOpDelistAccountForSale.CreateDelistAccountForSale(AProtocol, LSigner.account,
          LSigner.n_operation + 1, LAccount.account, ARequest.Fee, LWalletKey.PrivateKey, LPayload);
      omtChangeInfo:
        LOperation := TOpChangeAccountInfo.CreateChangeAccountInfo(AProtocol, LSigner.account,
          LSigner.n_operation + 1, LAccount.account, LWalletKey.PrivateKey, False, CT_TECDSA_Public_Nul,
          LChangeName, LNewName, LChangeType, ARequest.NewType, False, nil, ARequest.Fee, LPayload);
      omtBuyAccount:
        LOperation := TOpBuyAccount.CreateBuy(AProtocol, LSigner.account, LSigner.n_operation + 1,
          ARequest.DestinationAccount.account, ARequest.DestinationAccount.accountInfo.account_to_pay,
          ARequest.DestinationAccount.accountInfo.price, LAmount, ARequest.Fee, LNewKey, LWalletKey.PrivateKey, LPayload);
    end;
    if not AppendOperation(LOperation) then
      Exit;
    Dec(LSigner.balance, UInt64(ARequest.Fee) + UInt64(LAmount));
    Inc(LSigner.n_operation);
    LStates.AddOrSetValue(LSigner.account, LSigner);
    if ARequest.OperationType = omtSendPasc then
    begin
      Inc(LDestination.balance, UInt64(LAmount));
      LStates.AddOrSetValue(LDestination.account, LDestination);
    end;
  end;
  if LTree.OperationsCount = 0 then
  begin
    if (ARequest.OperationType = omtChangeInfo) and (Length(LAccounts) > 1) then
      Exit(Fail(Format('All selected accounts already have type %u. No changes or fees are needed.', [ARequest.NewType])));
    Exit(Fail('No valid operation to execute'));
  end;
  LDisposables.RemoveObject(LTree);
  AOperations := LTree;
  Result := True;
end;

class function TWIZOperationsHelper.ValidatePreparedOperations(AOperations: TOperationsHashTree; ASnapshot: TPCSafeBoxTransaction; var AErrorMessage: String): Boolean;
var
  LDisposables: TDisposables;
  LValidation: TPCSafeBoxTransaction;
  LPrevious: TAccountPreviousBlockInfo;
  i: Integer;
begin
  Result := False;
  AErrorMessage := '';
  if not Assigned(AOperations) or not Assigned(ASnapshot) then
  begin
    AErrorMessage := 'Operation validation requires a prepared batch and account snapshot.';
    Exit;
  end;
  LValidation := LDisposables.AddObject(TPCSafeBoxTransaction.Create(ASnapshot.FreezedSafeBox)) as TPCSafeBoxTransaction;
  LValidation.CopyFrom(ASnapshot);
  LPrevious := LDisposables.AddObject(TAccountPreviousBlockInfo.Create) as TAccountPreviousBlockInfo;
  for i := 0 to AOperations.OperationsCount - 1 do
    if not AOperations.GetOperation(i).DoOperation(LPrevious, LValidation, AErrorMessage) then
      Exit;
  Result := AOperations.OperationsCount > 0;
  if not Result then AErrorMessage := 'No operation is available to validate.';
end;

class function TWIZOperationsHelper.ExecuteOperations(var ARequest: TWalletOperationRequest; var AErrorMessage: string): Boolean;
var
  LNode: TNode;
  LTree: TOperationsHashTree;
  LDisposables: TDisposables;
  LDescription: string;
  LNameAccount, i: Integer;
  LMempool: TPCOperationsComp;

  function Refresh(var AAccount: TAccount): Boolean;
  begin
    Result := AAccount.account < Cardinal(LNode.Bank.AccountsCount);
    if Result then
      AAccount := LNode.GetAccount(AAccount.account)
    else
      AErrorMessage := Format('Account %u no longer exists in the current blockchain', [AAccount.account]);
  end;

begin
  Result := False;
  AErrorMessage := '';
  LNode := TNode.Node;
  if not ValidateOperationsInput(ARequest.Accounts, TWallet.Keys, LNode, AErrorMessage) then
    Exit;
  // Re-read the pending state at Finish. A wizard can stay open while new
  // operations change balances, owners, prices or operation counters.
  for i := 0 to High(ARequest.Accounts) do
    if not Refresh(ARequest.Accounts[i]) then
      Exit;
  for i := 0 to High(ARequest.SignerAccounts) do
    if not Refresh(ARequest.SignerAccounts[i]) then
      Exit;
  if ARequest.OperationType in [omtSendPasc, omtBuyAccount] then
    if not Refresh(ARequest.DestinationAccount) then
      Exit;
  if ARequest.OperationType = omtEnlistAccountForSale then
    if not Refresh(ARequest.SellerAccount) then
      Exit;
  if (ARequest.OperationType = omtBuyAccount) and
    (ARequest.DestinationAccount.accountInfo.account_to_pay >= Cardinal(LNode.Bank.AccountsCount)) then
  begin
    AErrorMessage := 'The sale proceeds account no longer exists';
    Exit;
  end;
  if (ARequest.OperationType = omtChangeInfo) and (Length(ARequest.Accounts) = 1) and
    (Length(ARequest.NewName) > 0) then
  begin
    LNameAccount := LNode.Bank.SafeBox.FindAccountByName(ARequest.NewName);
    if (LNameAccount >= 0) and (Cardinal(LNameAccount) <> ARequest.Accounts[0].account) then
    begin
      AErrorMessage := 'The account name is already in use';
      Exit;
    end;
  end;
  if not PrepareOperations(ARequest, LNode.Bank.SafeBox.CurrentProtocol, LNode.Bank.BlocksCount, TWallet.Keys, LTree, AErrorMessage) then
    Exit;
  LDisposables.AddObject(LTree);
  LMempool := LNode.LockMempoolRead;
  try
    if not ValidatePreparedOperations(LTree, LMempool.SafeBoxTransaction, AErrorMessage) then Exit;
  finally
    LNode.UnlockMempoolRead;
  end;
  LDescription := '';
  for i := 0 to LTree.OperationsCount - 1 do
  begin
    if LDescription <> '' then
      LDescription := LDescription + sLineBreak;
    LDescription := LDescription + LTree.GetOperation(i).ToString;
  end;
  if ARequest.OperationType = omtSendPasc then
    Result := SendPASCFinalizeAndDisplayMessage(LDescription, LDescription, LTree.OperationsCount,
      LTree.TotalAmount, LTree.TotalFee, LTree, AErrorMessage)
  else
    Result := OthersFinalizeAndDisplayMessage(LDescription, LDescription, LTree.OperationsCount,
      LTree.TotalFee, LTree, AErrorMessage);
end;

class function TWIZOperationsHelper.ExecuteSendPASC(const ASelectedAccounts: TArray<TAccount>; const ADestinationAccount: TAccount; AAmount, AFee: int64; const ASendPASCMode: TSendPASCMode; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string): boolean;
var
  LRequest: TWalletOperationRequest;
begin
  LRequest := Default(TWalletOperationRequest);
  LRequest.OperationType := omtSendPasc;
  LRequest.Accounts := TArrayTool<TAccount>.Copy(ASelectedAccounts);
  LRequest.DestinationAccount := ADestinationAccount;
  LRequest.Amount := AAmount;
  LRequest.Fee := AFee;
  LRequest.SendMode := ASendPASCMode;
  LRequest.PayloadMode := APayloadEncryptionMode;
  LRequest.PayloadContent := APayloadContent;
  LRequest.PayloadPassword := APayloadEncryptionPassword;
  Result := ExecuteOperations(LRequest, AErrorMessage);
end;

class function TWIZOperationsHelper.ExecuteChangeKey(const ASelectedAccounts: TArray<TAccount>; const ASignerAccount: TAccount; APublicKey: TAccountKey; AFee: int64; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string; AOperationSigningMode: TOperationSigningMode): boolean;
var
  LRequest: TWalletOperationRequest;
begin
  LRequest := Default(TWalletOperationRequest);
  LRequest.OperationType := omtChangeKey;
  LRequest.Accounts := TArrayTool<TAccount>.Copy(ASelectedAccounts);
  LRequest.SignerAccounts := TArray<TAccount>.Create(ASignerAccount);
  LRequest.PublicKey := APublicKey;
  LRequest.Fee := AFee;
  LRequest.SigningMode := AOperationSigningMode;
  LRequest.PayloadMode := APayloadEncryptionMode;
  LRequest.PayloadContent := APayloadContent;
  LRequest.PayloadPassword := APayloadEncryptionPassword;
  Result := ExecuteOperations(LRequest, AErrorMessage);
end;

class function TWIZOperationsHelper.ExecuteEnlistAccountForSale(const ASelectedAccounts: TArray<TAccount>; const ASignerAccount, ASellerAccount: TAccount; const APublicKey: TAccountKey; AFee, ASalePrice: int64; ALockedUntilBlock: UInt32; const AAccountSaleMode: TAccountSaleMode; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string; AOperationSigningMode: TOperationSigningMode): boolean;
var
  LRequest: TWalletOperationRequest;
begin
  LRequest := Default(TWalletOperationRequest);
  LRequest.OperationType := omtEnlistAccountForSale;
  LRequest.Accounts := TArrayTool<TAccount>.Copy(ASelectedAccounts);
  LRequest.SignerAccounts := TArray<TAccount>.Create(ASignerAccount);
  LRequest.SellerAccount := ASellerAccount;
  LRequest.PublicKey := APublicKey;
  LRequest.Fee := AFee;
  LRequest.SigningMode := AOperationSigningMode;
  LRequest.SalePrice := ASalePrice;
  LRequest.LockedUntilBlock := ALockedUntilBlock;
  LRequest.SaleMode := AAccountSaleMode;
  LRequest.PayloadMode := APayloadEncryptionMode;
  LRequest.PayloadContent := APayloadContent;
  LRequest.PayloadPassword := APayloadEncryptionPassword;
  Result := ExecuteOperations(LRequest, AErrorMessage);
end;

class function TWIZOperationsHelper.ExecuteDelistAccountFromSale(const ASelectedAccounts: TArray<TAccount>; const ASignerAccount: TAccount; AFee: int64; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; var AErrorMessage: string; AOperationSigningMode: TOperationSigningMode): boolean;
var
  LRequest: TWalletOperationRequest;
begin
  LRequest := Default(TWalletOperationRequest);
  LRequest.OperationType := omtDelistAccountFromSale;
  LRequest.Accounts := TArrayTool<TAccount>.Copy(ASelectedAccounts);
  LRequest.SignerAccounts := TArray<TAccount>.Create(ASignerAccount);
  LRequest.Fee := AFee;
  LRequest.SigningMode := AOperationSigningMode;
  LRequest.PayloadMode := APayloadEncryptionMode;
  LRequest.PayloadContent := APayloadContent;
  LRequest.PayloadPassword := APayloadEncryptionPassword;
  Result := ExecuteOperations(LRequest, AErrorMessage);
end;

class function TWIZOperationsHelper.ExecuteChangeAccountInfo(const ASelectedAccounts, ASignerAccounts: TArray<TAccount>; AFee: int64; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; const ANewName: TRawBytes; const ANewType: word; var AErrorMessage: string; AOperationSigningMode: TOperationSigningMode; const AApprovedTargets: TArray<TAccount>): boolean;
var
  LRequest: TWalletOperationRequest;
begin
  LRequest := Default(TWalletOperationRequest);
  LRequest.OperationType := omtChangeInfo;
  LRequest.Accounts := TArrayTool<TAccount>.Copy(ASelectedAccounts);
  LRequest.SignerAccounts := TArrayTool<TAccount>.Copy(ASignerAccounts);
  LRequest.Fee := AFee;
  LRequest.SigningMode := AOperationSigningMode;
  LRequest.NewName := ANewName;
  LRequest.NewType := ANewType;
  LRequest.ApprovedChangeInfoTargets := TArrayTool<TAccount>.Copy(AApprovedTargets);
  LRequest.PayloadMode := APayloadEncryptionMode;
  LRequest.PayloadContent := APayloadContent;
  LRequest.PayloadPassword := APayloadEncryptionPassword;
  Result := ExecuteOperations(LRequest, AErrorMessage);
end;

class function TWIZOperationsHelper.ExecuteBuyAccount(const ASelectedAccounts: TArray<TAccount>; const AAccountToBuy: TAccount; AFee: int64; const APayloadEncryptionMode: TPayloadEncryptionMode; const APayloadContent, APayloadEncryptionPassword: string; const AAmount: int64; const ANewOwnerPublicKey: TAccountKey; var AErrorMessage: string): boolean;
var
  LRequest: TWalletOperationRequest;
begin
  LRequest := Default(TWalletOperationRequest);
  LRequest.OperationType := omtBuyAccount;
  LRequest.Accounts := TArrayTool<TAccount>.Copy(ASelectedAccounts);
  LRequest.DestinationAccount := AAccountToBuy;
  LRequest.PublicKey := ANewOwnerPublicKey;
  LRequest.Amount := AAmount;
  LRequest.Fee := AFee;
  LRequest.PayloadMode := APayloadEncryptionMode;
  LRequest.PayloadContent := APayloadContent;
  LRequest.PayloadPassword := APayloadEncryptionPassword;
  Result := ExecuteOperations(LRequest, AErrorMessage);
end;

end.
