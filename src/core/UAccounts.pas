unit UAccounts;

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

{$I ./../config.inc}

uses
  Classes, SysUtils, UConst, UCrypto, SyncObjs, UThread, UBaseTypes,
  UPCOrderedLists, UPCDataTypes, UPCSafeBoxRootHash,
  UPCHardcodedRandomHashTable, UJSONFunctions,
  {$IFDEF USE_ABSTRACTMEM}
  UPCAbstractMem, UPCAbstractMemAccountKeys,
  {$ELSE}
  {$ENDIF}
  UPCAccountsOrdenations,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF};

Type
  { TPascalCoinProtocol }

  TPascalCoinProtocol = Class
  public
    FPCHardcodedRandomHashTable : TPCHardcodedRandomHashTable;
  public
    constructor Create;
    destructor Destroy; override;
    Class Function MinimumTarget(protocol_version : Integer): Cardinal;
    Class Function ResetTarget(current_target : Cardinal; protocol_version : Integer): Cardinal;
    Class Function GetRewardForNewLine(line_index: Cardinal): UInt64;
    Class Function CalcTotalBalance(ABlockCount : Cardinal): Int64;
    Class Function TargetToCompact(target: TRawBytes; protocol_version : Integer): Cardinal;
    Class Function TargetFromCompact(encoded: Cardinal; protocol_version : Integer): TRawBytes;
    Class Function GetNewTarget(vteorical, vreal: Cardinal; protocol_version : Integer; isSlowMovement : Boolean; Const actualTarget: TRawBytes): TRawBytes;
    Class Procedure CalcProofOfWork_Part1(const operationBlock : TOperationBlock; out Part1 : TRawBytes);
    Class Procedure CalcProofOfWork_Part3(const operationBlock : TOperationBlock; out Part3 : TRawBytes);
    Class Procedure CalcProofOfWork(const operationBlock : TOperationBlock; out PoW : TRawBytes);
    Class Function IsValidMinerBlockPayload(const newBlockPayload : TRawBytes) : Boolean;
    class procedure GetRewardDistributionForNewBlock(const OperationBlock : TOperationBlock; out acc_0_miner_reward, acc_4_dev_reward : Int64; out acc_4_for_dev : Boolean);
    class Function CalcSafeBoxHash(ABlocksHashBuffer : TBytesBuffer; protocol_version : Integer) : TRawBytes;
    class Function AllowUseHardcodedRandomHashTable(const AHardcodedFileName : String; const AHardcodedSha256Value : TRawBytes) : Boolean;
  end;

  TAccount_Helper = record helper for TAccount
     procedure SerializeAccount(AStream : TStream; current_protocol : Word);
  end;

  { TAccountComp }

  TAccountComp = Class
  private
  public
    Class Function IsValidAccountKey(const AAccountInfo: TAccountKey; ACurrentProtocol : Word; var errors : String): Boolean;
    Class function IsNullAccountKey(const AAccountInfo : TAccountKey) : Boolean;
    Class function IsValidNewAccountKey(const AAccountInfo : TAccountInfo; const ANewKey : TAccountKey; AProtocolVersion : Integer) : Boolean;
    Class Function IsValidAccountInfo(const AAccountInfo: TAccountInfo; ACurrentProtocol : Word; var errors : String): Boolean;
    Class Function IsValidAccountInfoHashLockKey(const AAccountInfo : TAccountInfo; const AKey : TRawBytes) : Boolean;
    Class Function IsValidHashLockKey(const AKey : TRawBytes; out AError : String) : Boolean;
    Class Function CalculateHashLock(const AKey : TRawBytes) : T32Bytes;
    Class Function IsAccountForSale(const AAccountInfo: TAccountInfo) : Boolean;
    Class function IsAccountForPrivateSale(const AAccountInfo: TAccountInfo): Boolean;
    Class function IsAccountForPublicSale(const AAccountInfo: TAccountInfo): Boolean;
    Class Function IsAccountForSwap(const AAccountInfo: TAccountInfo) : Boolean;
    Class function IsAccountForCoinSwap(const AAccountInfo: TAccountInfo) : Boolean;
    Class function IsAccountForAccountSwap(const AAccountInfo: TAccountInfo) : Boolean;
    Class Function IsAccountForSaleOrSwap(const AAccountInfo: TAccountInfo) : Boolean;
    Class Function IsAccountForSaleOrSwapAcceptingTransactions(const AAccount: TAccount; ACurrentBlock : Integer; ACurrentProtocol : Word; const APayload : TRawBytes) : Boolean;
    Class Function IsOperationRecipientSignable(const ASender, ATarget : TAccount; ACurrentBlock : Integer; ACurrentProtocol : Word) : Boolean;
    Class Function GetECInfoTxt(Const EC_OpenSSL_NID: Word) : String;
    Class Function IsValidEC_OpenSSL_NID(ANID : Word) : Boolean;
    Class Procedure ValidsEC_OpenSSL_NID(list : TList<Word>);
    Class Function AccountKey2RawString(const account: TAccountKey): TRawBytes; overload;
    Class procedure AccountKey2RawString(const account: TAccountKey; var dest: TRawBytes); overload;
    Class Function RawString2Accountkey(const rawaccstr: TRawBytes): TAccountKey; overload;
    Class procedure RawString2Accountkey(const rawaccstr: TRawBytes; var dest: TAccountKey); overload;
    Class Function PrivateToAccountkey(key: TECPrivateKey): TAccountKey;
    Class Function IsAccountBlockedByProtocol(account_number, blocks_count : Cardinal) : Boolean;
    Class Function EqualAccountInfos(const accountInfo1,accountInfo2 : TAccountInfo) : Boolean;
    Class Function EqualAccountKeys(const account1,account2 : TAccountKey) : Boolean;
    Class Function EqualAccounts(const account1,account2 : TAccount) : Boolean;
    Class Function EqualOperationBlocks(const opBlock1,opBlock2 : TOperationBlock) : Boolean;
    Class Function EqualBlockAccounts(const blockAccount1,blockAccount2 : TBlockAccount) : Boolean;
    Class Function AccountNumberToAccountTxtNumber(account_number : Cardinal) : String;
    Class function AccountTxtNumberToAccountNumber(Const account_txt_number : String; var account_number : Cardinal) : Boolean;
    Class function FormatMoney(Money : Int64) : String;
    Class function FormatMoneyDecimal(Money : Int64) : Currency;
    Class Function TxtToMoney(Const moneytxt : String; var money : Int64) : Boolean;
    Class Function AccountKeyFromImport(Const HumanReadable : String; var account : TAccountKey; var errors : String) : Boolean;
    Class Function AccountPublicKeyExport(Const account : TAccountKey) : String;
    Class Function AccountPublicKeyImport(Const HumanReadable : String; var account : TAccountKey; var errors : String) : Boolean;
    Class Function AccountBlock(Const account_number : Cardinal) : Cardinal;
    Class Function AccountInfo2RawString(const AccountInfo : TAccountInfo) : TRawBytes; overload;
    Class procedure AccountInfo2RawString(const AccountInfo : TAccountInfo; var dest : TRawBytes); overload;
    Class procedure SaveAccountToAStream(Stream: TStream; const Account : TAccount; current_protocol : Word);
    Class function LoadAccountFromStream(Stream: TStream; var Account : TAccount) : Boolean;
    Class Function RawString2AccountInfo(const rawaccstr: TRawBytes): TAccountInfo; overload;
    Class procedure RawString2AccountInfo(const rawaccstr: TRawBytes; var dest : TAccountInfo); overload;
    Class Function IsAccountLocked(const AccountInfo : TAccountInfo; blocks_count : Cardinal) : Boolean;
    Class procedure SaveTOperationBlockToStream(const stream : TStream; const operationBlock:TOperationBlock);
    Class Function LoadTOperationBlockFromStream(const stream : TStream; var operationBlock:TOperationBlock) : Boolean;
    Class Function AccountToTxt(const Account : TAccount) : String;
    Class Function AccountCanRecover(const Account: TAccount; currentBlockCount: Cardinal) : Boolean;
  End;

  TPCSafeBox = Class;

  TAccountKeyArray = array of TAccountKey;

  TAccountList = TList<TAccount>;

  // This is a class to quickly find accountkeys and their respective account number/s

  { TOrderedAccountKeysList }

  {$IFDEF USE_ABSTRACTMEM}
  TAccountsNumbersList = TAccountsUsingThisKey;
  {$ELSE}
  TAccountsNumbersList = TOrderedCardinalList;
  {$ENDIF}

  TOrderedAccountKeysList = Class
  Private
    FAutoAddAll : Boolean;
    FAccountList : TPCSafeBox;
    FOrderedAccountKeysList : TPCThreadList<Pointer>; // An ordered list of pointers to quickly find account keys in account list
    FTotalChanges : Integer;
    Function Find(lockedList : TList<Pointer>; Const AccountKey: TAccountKey; var Index: Integer): Boolean;
    function GetAccountKeyChanges(index : Integer): Integer;
    function GetAccountKeyList(index: Integer): TAccountsNumbersList;
    function GetAccountKey(index: Integer): TAccountKey;
  protected
    Procedure ClearAccounts(RemoveAccountList : Boolean);
  public
    Constructor Create(AccountList : TPCSafeBox; AutoAddAll : Boolean);
    Destructor Destroy; override;
    Procedure AddAccountKey(Const AccountKey : TAccountKey);
    Procedure AddAccountKeys(Const AccountKeys : array of TAccountKey);
    Procedure RemoveAccountKey(Const AccountKey : TAccountKey);
    {$IFnDEF USE_ABSTRACTMEM}
    Procedure AddAccounts(Const AccountKey : TAccountKey; const accounts : Array of Cardinal);
    Procedure RemoveAccounts(Const AccountKey : TAccountKey; const accounts : Array of Cardinal);
    {$ENDIF}
    Function IndexOfAccountKey(Const AccountKey : TAccountKey) : Integer;
    Property AccountKeyList[index : Integer] : TAccountsNumbersList read GetAccountKeyList;
    Property AccountKey[index : Integer] : TAccountKey read GetAccountKey;
    Property AccountKeyChanges[index : Integer] : Integer read GetAccountKeyChanges;
    procedure ClearAccountKeyChanges;
    Function Count : Integer;
    Property SafeBox : TPCSafeBox read FAccountList;
    Procedure Clear;
    function ToArray : TAccountKeyArray;
    function Lock : TList<Pointer>;
    procedure Unlock;
    function HasAccountKeyChanged : Boolean;
    procedure CopyFrom(const source : TOrderedAccountKeysList);
    function GetAccountsUsingThisKey(Const AAccountKey : TAccountKey) : TAccountsNumbersList;
  End;

  // SafeBox is a box that only can be updated using SafeBoxTransaction, and this
  // happens only when a new BlockChain is included. After this, a new "SafeBoxHash"
  // is created, so each SafeBox has a unique SafeBoxHash

  TPCSafeBoxTransaction = Class;
  TOrderedAccountList = Class;
  TOrderedBlockAccountList = Class;

  { TProgressNotify }

  TProgressNotify = procedure(sender : TObject; const message : String; curPos, totalCount : Int64) of object;

  { TProgressNotifyMany }

  TProgressNotifyMany = TArray<TProgressNotify>;

  { TProgressNotifyManyHelper }

  TProgressNotifyManyHelper = record helper for TProgressNotifyMany
    procedure Add(listener : TProgressNotify);
    procedure Remove(listener : TProgressNotify);
    procedure Invoke(sender : TObject; const message : String; curPos, totalCount : Int64);
  end;


  {$IFDEF USE_ABSTRACTMEM}
  TSafeboxPubKeysAndAccounts = TPCAbstractMemAccountKeys;
  {$ELSE}
  TSafeboxPubKeysAndAccounts = TOrderedAccountKeysList;
  {$ENDIF}

  { TPCSafeBox }

  TAccountUpdateStyle = (aus_transaction_commit, aus_rollback, aus_commiting_from_otherchain);

  TPCSafeBox = Class
  private
    {$IFDEF USE_ABSTRACTMEM}
    FPCAbstractMem : TPCAbstractMem;
    {$ELSE}
    FBlockAccountsList : TList<Pointer>; // Used when has no PreviousSafebox
    FBufferBlocksHash: TBytesBuffer32Safebox;
    FAggregatedHashrate : TBigNum;
    FOrderedByName : TOrderedRawList;
    // OrderedAccountKeysList (Added after Build 3.0.1) allows an indexed search of public keys in the safebox with mem optimization
    FOrderedAccountKeysList : TSafeboxPubKeysAndAccounts;
    FAccountsOrderedByUpdatedBlock : TAccountsOrderedByUpdatedBlock;
    FAccountsOrderedBySalePrice : TAccountsOrderedBySalePrice;
    {$ENDIF}
    FModifiedBlocksSeparatedChain : TOrderedBlockAccountList; // Used when has PreviousSafebox (Used if we are on a Separated chain)
    //
    FListOfOrderedAccountKeysList : TList<TOrderedAccountKeysList>;
    FTotalBalance: Int64;
    FSafeBoxHash : TRawBytes;
    FLock: TPCCriticalSection; // Thread safe
    FWorkSum : UInt64;
    FCurrentProtocol: Integer;
    // Snapshots utility new on V3
    FSnapshots : TList<Pointer>; // Will save a Snapshots lists in order to rollback Safebox to a previous block state
    FMaxSafeboxSnapshots : Integer;
    // To be added to next snapshot
    FModifiedBlocksPreviousState : TOrderedBlockAccountList;
    FModifiedBlocksFinalState : TOrderedBlockAccountList;
    FAddedNamesSincePreviousSafebox : TOrderedRawList;
    FDeletedNamesSincePreviousSafebox : TOrderedRawList;
    // Is capturing data from a snapshot?
    FPreviousSafeBox : TPCSafeBox;  // PreviousSafebox is the Safebox with snpashots where this safebox searches
    FPreviousSafeboxOriginBlock : Integer;
    // Has chains based on this Safebox?
    FSubChains : TList<TPCSafeBox>; // Will link to subchains (other safebox) based on a current snapshot of this safebox
    //
    Procedure AccountKeyListAddAccounts(Const AccountKey : TAccountKey; const accounts : Array of Cardinal);
    Procedure AccountKeyListRemoveAccount(Const AccountKey : TAccountKey; const accounts : Array of Cardinal);
    // V3
    procedure SearchBlockWhenOnSeparatedChain(blockNumber : Cardinal; out blockAccount : TBlockAccount);
    function GetAggregatedHashrate: TBigNum;
    function GetOrderedAccountKeysList: TSafeboxPubKeysAndAccounts;
    function GetAccount(AAccountNumber : Integer; var AAccount : TAccount) : Boolean;
  protected
    FTotalFee: Int64;
    Procedure UpdateAccount(account_number : Cardinal; const newAccountInfo: TAccountInfo; const newName : TRawBytes; newType : Word;
         newBalance: UInt64; newN_operation: Cardinal;
         const newAccountData, newAccountSeal : TRawBytes;
         accountUpdateStyle : TAccountUpdateStyle; newUpdated_block_pasive_mode, newUpdated_block_active_mode : Cardinal;
         AHasBenUpdatedOnActiveMode, AHasBenUpdatedOnPasiveMode : Boolean);
    Function AddNew(Const blockChain : TOperationBlock) : TBlockAccount;
    function DoUpgradeToProtocol2 : Boolean;
    function DoUpgradeToProtocol3 : Boolean;
    function DoUpgradeToProtocol4 : Boolean;
    function DoUpgradeToProtocol5 : Boolean;
    function DoUpgradeToProtocol6 : Boolean;
    function BufferBlocksHash : TBytesBuffer32Safebox;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure SetToPrevious(APreviousSafeBox : TPCSafeBox; StartBlock : Cardinal);
    procedure CommitToPrevious;
    procedure RollBackToSnapshot(snapshotBlock : Cardinal);
    function AccountsCount: Integer;
    Function BlocksCount : Integer;
    Procedure CopyFrom(accounts : TPCSafeBox);
    Class Function CalcBlockHash(const block : TBlockAccount; current_protocol : Word):TRawBytes;
    Class Function BlockAccountToText(Const block : TBlockAccount):String;
    Function LoadSafeBoxChunkFromStream(Stream : TStream; checkAll : Boolean; checkSafeboxHash : TRawBytes; progressNotify : TProgressNotify; previousCheckedSafebox : TPCSafebox; var ALastReadBlock : TBlockAccount; var errors : String) : Boolean;
    Function LoadSafeBoxFromStream(Stream : TStream; checkAll : Boolean; var LastReadBlock : TBlockAccount; var errors : String) : Boolean; overload;
    Function LoadSafeBoxFromStream(Stream : TStream; checkAll : Boolean; checkSafeboxHash : TRawBytes; progressNotify : TProgressNotify; previousCheckedSafebox : TPCSafebox; var ALastReadBlock : TBlockAccount; var errors : String) : Boolean; overload;
    Class Function LoadSafeBoxStreamHeader(Stream : TStream; var sbHeader : TPCSafeBoxHeader) : Boolean;
    Class Function SaveSafeBoxStreamHeader(Stream : TStream; protocol : Word; OffsetStartBlock, OffsetEndBlock, CurrentSafeBoxBlocksCount : Cardinal) : Boolean;
    Class Function MustSafeBoxBeSaved(BlocksCount : Cardinal) : Boolean;
    Class Function InitialSafeboxHash : TRawBytes;
    Class Procedure SaveSafeBoxBlockToAStream(ADestStream : TStream; ACurrentProtocol : Integer; const ABlock : TBlockAccount);
    Procedure SaveSafeBoxToAStream(Stream : TStream; FromBlock, ToBlock : Cardinal);
    class Function CopySafeBoxStream(Source,Dest : TStream; FromBlock, ToBlock : Cardinal; var errors : String) : Boolean;
    class Function ConcatSafeBoxStream(Source1, Source2, Dest : TStream; var errors : String) : Boolean;
    class function ValidAccountName(const new_name : TRawBytes; var errors : String) : Boolean;

    Function IsValidNewOperationsBlock(Const newOperationBlock : TOperationBlock; checkSafeBoxHash, checkValidOperationsBlock : Boolean; var errors : String) : Boolean;
    class Function IsValidOperationBlock(Const newOperationBlock : TOperationBlock; var errors : String) : Boolean;
    Function GetActualTargetHash(protocolVersion : Word): TRawBytes;
    Function GetActualCompactTargetHash(protocolVersion : Word): Cardinal;
    Function FindAccountByName(const aName : String) : Integer; overload;
    Function FindAccountByName(const aName : TRawBytes) : Integer; overload;
    Function FindAccountsStartingByName(const AStartName : TRawBytes; const ARawList : TOrderedRawList; const AMax : Integer = 0) : Integer;
    
    Procedure Clear;
    Function Account(account_number : Cardinal) : TAccount;
    Function GetBlock(block_number : Cardinal) : TBlockAccount;
    Function GetBlockInfo(ABlockNumber : Cardinal) : TOperationBlock;

    Function CalcSafeBoxHash : TRawBytes;
    Function CalcBlockHashRateInKhs(block_number : Cardinal; Previous_blocks_average : Cardinal) : Int64;
    Function CalcBlockHashRateInHs(block_number : Cardinal; Previous_blocks_average : Cardinal) : TBigNum;
    Property TotalBalance : Int64 read FTotalBalance;
    Procedure StartThreadSafe;
    Procedure EndThreadSave;
    Property SafeBoxHash : TRawBytes read FSafeBoxHash;
    Property WorkSum : UInt64 read FWorkSum;
    Property CurrentProtocol : Integer read FCurrentProtocol;
    function CanUpgradeToProtocol(newProtocolVersion : Word) : Boolean;
    procedure CheckMemory;
    Property PreviousSafeboxOriginBlock : Integer Read FPreviousSafeboxOriginBlock;
    Function GetMinimumAvailableSnapshotBlock : Integer;
    Function HasSnapshotForBlock(block_number : Cardinal) : Boolean;
    Property OrderedAccountKeysList : TSafeboxPubKeysAndAccounts read GetOrderedAccountKeysList;

    Property AggregatedHashrate : TBigNum read GetAggregatedHashrate;
    procedure GetAggregatedHashrateOnBlock(ABlockNumber : Cardinal; const AAggregatedHashrate : TBigNum);
    {$IFDEF USE_ABSTRACTMEM}
    procedure SetSafeboxFileName(ASafeboxFileName : String);
    procedure SaveCheckpointing(ACheckpointingSafeboxFileName : String);
    procedure UpdateSafeboxFileName(const ANewSafeboxFileName : String);
    procedure ClearSafeboxfile;
    class Function CopyAbstractMemToSafeBoxStream(ASource : TPCAbstractMem; ADestStream : TStream; AFromBlock, AToBlock : Cardinal; var AErrors : String) : Boolean;
    property PCAbstractMem : TPCAbstractMem read FPCAbstractMem;
    {$ENDIF}
    Function AccountsOrderedByUpdatedBlock : TAccountsOrderedByUpdatedBlock;
    Function AccountsOrderedBySalePrice : TAccountsOrderedBySalePrice;
  End;


  { TOrderedBlockAccountList }

  TOrderedBlockAccountList = Class
  private
    FMaxBlockNumber : Integer;
    FList : TList<Pointer>;
    Function Find(const block_number: Cardinal; out Index: Integer): Boolean;
    Function SaveBlockAccount(Const blockAccount : TBlockAccount; UpdateIfFound : Boolean) : Integer;
  public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Clear;
    Function AddIfNotExists(Const blockAccount : TBlockAccount) : Integer;
    Function Add(Const blockAccount : TBlockAccount) : Integer;
    Function Count : Integer;
    Function Get(index : Integer) : TBlockAccount;
    Function MaxBlockNumber : Integer;
  End;

  TOrderedAccountList = Class
  private
    FList : TList<Pointer>;
    Function Find(const account_number: Cardinal; var Index: Integer): Boolean;
  public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Clear;
    Function Add(Const account : TAccount) : Integer;
    Function Count : Integer;
    Function Get(index : Integer) : TAccount;
    Function IndexOf(account_number: Cardinal) : Integer;
  End;

  TAccountPreviousBlockInfoData = Record
    Account : Cardinal;
    Previous_updated_block : Cardinal;
  end;

  { TAccountPreviousBlockInfo }

  TAccountPreviousBlockInfo = Class
  private
    FList : TList<Pointer>;
    Function FindAccount(const account: Cardinal; var Index: Integer): Boolean;
    function GetData(index : Integer): TAccountPreviousBlockInfoData;
  public
    Constructor Create;
    Destructor Destroy; override;
    Procedure UpdateIfLower(account, previous_updated_block : Cardinal);
    Function Add(account, previous_updated_block : Cardinal) : Integer;
    Procedure Remove(account : Cardinal);
    Procedure Clear;
    Procedure CopyFrom(Sender : TAccountPreviousBlockInfo);
    Function IndexOfAccount(account : Cardinal) : Integer;
    Property Data[index : Integer] : TAccountPreviousBlockInfoData read GetData;
    Function GetPreviousUpdatedBlock(account : Cardinal; defaultValue : Cardinal) : Cardinal;
    Function Count : Integer;
    procedure SaveToStream(stream : TStream);
    function LoadFromStream(stream : TStream) : Boolean;
  end;

  { TPCSafeBoxTransaction }

  TPCSafeBoxTransaction = Class
  private
  type
    TSealedAccount = Record
      LatestOpIDUsedForSeal : TRawBytes;
      AccountSealed : PAccount;
      SealChangesCounter : Integer;
      UsedAsPasiveMode : Boolean;
      UsedAsActiveMode : Boolean;
    End;
    PSealedAccount = ^TSealedAccount;
    {TSealedAccountList}
    TSealedAccountList = Class
    private
      FSafeBoxTransaction : TPCSafeBoxTransaction;
      FList : TList<Pointer>;
      Function Find(const account_number: Cardinal; var Index: Integer): Boolean;
    public
      Constructor Create(ASafeBoxTransaction : TPCSafeBoxTransaction);
      Destructor Destroy; Override;
      Procedure Clear;
      function Count : Integer;
      function GetAccount_Whitout_Sealing(account_number: Cardinal) : PSealedAccount;
      procedure DoUpdateSealIfNeeded(APtrSealedAccount : PSealedAccount; const AOpID : TRawBytes);
      procedure CopyFrom(ASource : TSealedAccountList);
    End;
  private
    FOrderedList : TSealedAccountList;
    FFreezedAccounts : TPCSafeBox;
    FTotalBalance: Int64;
    FTotalFee: Int64;
    FOldSafeBoxHash : TRawBytes;
    FAccountNames_Deleted : TOrderedRawList;
    FAccountNames_Added : TOrderedRawList;
    Function Origin_BlocksCount : Cardinal;
    Function Origin_SafeboxHash : TRawBytes;
    Function Origin_TotalBalance : Int64;
    Function Origin_TotalFee : Int64;
    Function Origin_FindAccountByName(const account_name : TRawBytes) : Integer;
  protected
    Function GetInternalAccount(account_number : Cardinal; var APtrSealedAccount : PSealedAccount) : PAccount;
    procedure UpdateSealAndActiveModeFlag(APtrSealedAccount : PSealedAccount; AOpID : TRawBytes; ASetUsedAsActiveMode : Boolean);
  public
    Constructor Create(SafeBox : TPCSafeBox);
    Destructor Destroy; override;
    Function TransferAmount(previous : TAccountPreviousBlockInfo; const AOpID : TRawBytes; sender,signer,target : Cardinal; n_operation : Cardinal; amount, fee : UInt64; var errors : String) : Boolean;
    Function TransferAmounts(previous : TAccountPreviousBlockInfo; const AOpID : TRawBytes; const senders, n_operations : Array of Cardinal; const sender_amounts : Array of UInt64; const receivers : Array of Cardinal; const receivers_amounts : Array of UInt64; var errors : String) : Boolean;
    Function UpdateAccountInfo(previous : TAccountPreviousBlockInfo; const AOpID : TRawBytes; signer_account, signer_n_operation, target_account: Cardinal; const accountInfo: TAccountInfo; const newName, newData : TRawBytes; newType : Word; fee: UInt64; var errors : String) : Boolean;
    Function BuyAccount(APrevious : TAccountPreviousBlockInfo; const AOpID : TRawBytes; ABuyer,AAccountToBuy,ASeller: Cardinal; ANOperation : Cardinal; AAmount, AAccountPrice, AFee : UInt64; const ANewAccountKey : TAccountKey; const AHashLockKey : TRawBytes; ARecipientSigned : Boolean; var AErrors : String) : Boolean;
    Function Commit(Const operationBlock : TOperationBlock; var errors : String) : Boolean;
    Function Account(account_number : Cardinal) : TAccount;
    Procedure Rollback;
    Function CheckIntegrity : Boolean;
    Property FreezedSafeBox : TPCSafeBox read FFreezedAccounts;
    Property TotalFee : Int64 read FTotalFee;
    Property TotalBalance : Int64 read FTotalBalance;
    Procedure CopyFrom(transaction : TPCSafeBoxTransaction);
    Procedure CleanTransaction;
    Function FindAccountByNameInTransaction(const findName : TRawBytes; out isAddedInThisTransaction, isDeletedInThisTransaction : Boolean) : Integer;
  End;

  { TStreamOp }

  TStreamOp = Class
  public
    class Function WriteAnsiString(Stream: TStream; const value: TRawBytes): Integer; overload;
    class Function WriteAnsiString(Stream: TStream; const value: T32Bytes): Integer; overload;
    class Function WriteString(Stream: TStream; const value: String): Integer;
    class Function WriteTBytes(Stream: TStream; const value: TBytes): Integer;
    class Function ReadAnsiString(Stream: TStream; var value: TRawBytes; ACheckLength : Integer = 0) : Integer; overload;
    class Function ReadAnsiString(Stream: TStream; var value: T32Bytes): Integer; overload;
    class Function ReadString(Stream: TStream; var value: String): Integer;
    class Function ReadTBytes(Stream: TStream; var ABytes : TBytes; ACheckLength : Integer = 0): Integer;
    class Function WriteAccountKey(Stream: TStream; const value: TAccountKey): Integer;
    class Function ReadAccountKey(Stream: TStream; var value : TAccountKey): Integer;
    class Function SaveStreamToRaw(Stream: TStream) : TRawBytes;
    class procedure LoadStreamFromRaw(Stream: TStream; const raw : TRawBytes);
    class Function WriteGUID(AStream : TStream; const AGUID : TGUID) : Integer;
    class Function ReadGUID(AStream : TStream; var AGUID : TGUID) : Integer;
  End;



Const
  CT_OperationBlock_NUL : TOperationBlock = (block:0;account_key:(EC_OpenSSL_NID:0;x:Nil;y:Nil);reward:0;fee:0;protocol_version:0;
    protocol_available:0;timestamp:0;compact_target:0;nonce:0;block_payload:Nil;initial_safe_box_hash:Nil;operations_hash:Nil;proof_of_work:Nil;previous_proof_of_work:Nil);

  CT_SafeBoxChunkIdentificator = 'SafeBoxChunk';
  CT_HashLock_NUL : T32Bytes = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);

function Check_Safebox_Names_Consistency(sb : TPCSafeBox; const title :String; var errors : String) : Boolean;
Procedure Check_Safebox_Integrity(sb : TPCSafebox; title: String);

implementation

uses
  ULog, {$IFnDEF USE_ABSTRACTMEM} UAccountKeyStorage,{$ENDIF} math, UCommon, UPCOperationsBlockValidator, UPCTemporalFileStream, UEncoding;


{$IFDEF FPC}
  {$DEFINE USE_BIGBLOCKS_MEM_ON_DISK}
  // USE_BIGBLOCKS_MEM_ON_DISK directive is used in order to prevent a FreePascal issue with Heap allocation strategy that
  // reuses big blocks of disposed memory and fragments it, this causes that when a new big block of same size that previously
  // freeded mem is needed it will not reuse because has been fragmented...
  // Tested on FPC version 3.2.0 (2020-11-03) and order versions
  // Defragmention documented here: https://www.freepascal.org/docs-html/current/prog/progsu172.html
  // This issue is not detected on current Delphi memory manager (Tested on Delphi 10.3.2)
{$ENDIF}

{ This function is for testing purpose only.
  Will check if Account Names are well assigned and stored }
function Check_Safebox_Names_Consistency(sb : TPCSafeBox; const title :String; var errors : String) : Boolean;
Var i,j : Integer;
  acc : TAccount;
  auxs : TRawBytes;
  tc : TTickCount;
  LErrsString : TStrings;
Begin
  tc := TPlatform.GetTickCount;
  Try
    errors := '';
    Result := True;
    for i:=0 to sb.AccountsCount-1 do begin
      acc := sb.Account(i);
      If Length(acc.name)>0 then begin
        j := sb.FindAccountByName(acc.name);
        If j<>i then begin
          errors :=errors + Format(' > Account %d name:%s found at:%d<>Theorical:%d',[acc.account,acc.name.ToPrintable,j,i]);
        end;
      end;
    end;
    {$IFDEF USE_ABSTRACTMEM}
    LErrsString := TStringList.Create;
    try
      sb.FPCAbstractMem.CheckConsistency(LErrsString);
      if LErrsString.Count>0 then begin
        errors := errors + #10+ LErrsString.Text;
      end;
    finally
      LErrsString.Free;
    end;
    {$ELSE}
    // Reverse
    for i:=0 to sb.FOrderedByName.Count-1 do begin
      j := sb.FOrderedByName.GetTag(i);
      auxs := sb.FOrderedByName.Get(i);
      acc := sb.Account(j);
      If (auxs<>acc.name) then begin
        errors :=errors + Format(' > Name:%s at thorical account %d not valid (found %s)',[auxs.ToPrintable,j,acc.name.ToPrintable]);
      end;
    end;
    {$ENDIF}
    If (errors<>'') then begin
      errors := title+' '+errors;
      Result := False;
      TLog.NewLog(lterror,'Check_Safebox_Names_Consistency',errors);
    end;
  finally
    TLog.NewLog(ltDebug,'Check_Safebox_Names_Consistency','Used time '+IntToStr(TPlatform.GetElapsedMilliseconds(tc))+' milliseconds');
  end;
end;

{ This function is for testing purpose only.
  Will check if Accounts are Ok }
Procedure Check_Safebox_Integrity(sb : TPCSafebox; title: String);
var i,j,maxBlock : Integer;
  bl_my, bl_modified : TBlockAccount;
  auxH : TBytesBuffer;
Begin
  For i:=0 to sb.FModifiedBlocksFinalState.Count-1 do begin
    bl_modified := sb.FModifiedBlocksFinalState.Get(i);
    bl_my := sb.GetBlock(bl_modified.blockchainInfo.block);
    If Not TAccountComp.EqualBlockAccounts(bl_my,bl_modified) then begin
      Raise Exception.Create(Format('%s Integrity on modified (i)=%d for block number:%d',[title, i,bl_my.blockchainInfo.block]));
    end;
    If TBaseType.BinStrComp( sb.CalcBlockHash(bl_modified,sb.FCurrentProtocol), bl_modified.block_hash)<>0 then begin
      Raise Exception.Create(Format('%s Integrity on block hash (i)=%d for block number:%d',[title, i,bl_my.blockchainInfo.block]));
    end;
  end;
  auxH := TBytesBuffer.Create(1024);
  Try
    maxBlock := sb.BlocksCount;
    auxH.SetLength(sb.BlocksCount*32);
    for i:=0 to sb.BlocksCount-1 do begin
      bl_my := sb.GetBlock(i);
      for j:=Low(bl_my.accounts) to High(bl_my.accounts) do begin
        If (maxBlock < (bl_my.accounts[j].updated_on_block_passive_mode)) or (maxBlock < (bl_my.accounts[j].updated_on_block_active_mode)) then begin
          Raise Exception.Create(Format('%s Integrity on (i)=%d for block account:%d pasive updated on %d , active updated on %d ,maxBlock %d',[title, i,bl_my.accounts[j].account,bl_my.accounts[j].updated_on_block_passive_mode,bl_my.accounts[j].updated_on_block_active_mode,maxBlock]));
        end;
      end;
      auxH.Replace(i*32,bl_my.block_hash[Low(bl_my.block_hash)],Length(bl_my.block_hash));
    end;
    if (sb.BufferBlocksHash.Compare(auxH)<>0) then begin
      Raise Exception.Create(Format('%s Integrity different Buffer Block Hash',[title]));
    end;
  Finally
    auxH.Free;
  End;
end;


{ TPascalCoinProtocol }

var _INTERNAL_PascalCoinProtocol : TPascalCoinProtocol = Nil;

class function TPascalCoinProtocol.GetNewTarget(vteorical, vreal: Cardinal; protocol_version : Integer; isSlowMovement : Boolean; const actualTarget: TRawBytes): TRawBytes;
Var
  bnact, bnaux: TBigNum;
  tsTeorical, tsReal, factor, factorMin, factorMax, factorDivider: Int64;
begin
  { Given a teorical time in seconds (vteorical>0) and a real time in seconds (vreal>0)
    and an actual target, calculates a new target
    by % of difference of teorical vs real.

    Increment/decrement is adjusted to +-200% in a full CT_CalcNewTargetBlocksAverage round
    ...so each new target is a maximum +-(100% DIV (CT_CalcNewTargetBlocksAverage DIV 2)) of
    previous target. This makes target more stable.

    }
  tsTeorical := vteorical;
  tsReal := vreal;

  { On protocol 1,2 the increment was limited in a integer value between -10..20
    On protocol 3 we increase decimals, so increment could be a integer
    between -1000..2000, using 2 more decimals for percent. Also will introduce
    a "isSlowMovement" variable that will limit to a maximum +-0.5% increment}
  if (protocol_version<CT_PROTOCOL_3) then begin
    factorDivider := 1000;
    factor := (((tsTeorical - tsReal) * 1000) DIV (tsTeorical)) * (-1);

    { Important: Note that a -500 is the same that divide by 2 (-100%), and
      1000 is the same that multiply by 2 (+100%), so we limit increase
      in a limit [-500..+1000] for a complete (CT_CalcNewTargetBlocksAverage DIV 2) round }
    if CT_CalcNewTargetBlocksAverage>1 then begin
      factorMin := (-500) DIV (CT_CalcNewTargetBlocksAverage DIV 2);
      factorMax := (1000) DIV (CT_CalcNewTargetBlocksAverage DIV 2);
    end else begin
      factorMin := (-500);
      factorMax := (1000);
    end;
  end else begin
    // Protocol 3:
    factorDivider := 100000;
    If (isSlowMovement) then begin
      // Limit to 0.5% instead of 2% (When CT_CalcNewTargetBlocksAverage = 100)
      factorMin := (-50000) DIV (CT_CalcNewTargetBlocksAverage * 2);
      factorMax := (100000) DIV (CT_CalcNewTargetBlocksAverage * 2);
    end else begin
      if CT_CalcNewTargetBlocksAverage>1 then begin
        factorMin := (-50000) DIV (CT_CalcNewTargetBlocksAverage DIV 2);
        factorMax := (100000) DIV (CT_CalcNewTargetBlocksAverage DIV 2);
      end else begin
        factorMin := (-50000);
        factorMax := (100000);
      end;
    end;
  end;

  factor := (((tsTeorical - tsReal) * factorDivider) DIV (tsTeorical)) * (-1);

  if factor < factorMin then factor := factorMin
  else if factor > factorMax then factor := factorMax
  else if factor=0 then begin
    Result := actualTarget;
    exit;
  end;

  // Calc new target by increasing factor (-500 <= x <= 1000)
  bnact := TBigNum.Create(0);
  try
    bnact.RawValue := actualTarget;
    bnaux := bnact.Copy;
    try
      bnact.Multiply(factor).Divide(factorDivider).Add(bnaux);
    finally
      bnaux.Free;
    end;
    // Adjust to TargetCompact limitations:
    Result := TargetFromCompact(TargetToCompact(bnact.RawValue,protocol_version),protocol_version);
    //
  finally
    bnact.Free;
  end;
end;

class procedure TPascalCoinProtocol.CalcProofOfWork_Part1(const operationBlock: TOperationBlock; out Part1: TRawBytes);
var ms : TMemoryStream;
  accKeyRaw : TRawBytes;
begin
  ms := TMemoryStream.Create;
  try
    // Part 1
    ms.Write(operationBlock.block,Sizeof(operationBlock.block)); // Little endian
    accKeyRaw := TAccountComp.AccountKey2RawString(operationBlock.account_key);
    ms.WriteBuffer(accKeyRaw[Low(accKeyRaw)],Length(accKeyRaw));
    ms.Write(operationBlock.reward,Sizeof(operationBlock.reward)); // Little endian
    ms.Write(operationBlock.protocol_version,Sizeof(operationBlock.protocol_version)); // Little endian
    ms.Write(operationBlock.protocol_available,Sizeof(operationBlock.protocol_available)); // Little endian
    ms.Write(operationBlock.compact_target,Sizeof(operationBlock.compact_target)); // Little endian
    SetLength(Part1,ms.Size);
    ms.Position:=0;
    ms.Read(Part1[Low(Part1)],ms.Size);
  finally
    ms.Free;
  end;
end;

class procedure TPascalCoinProtocol.CalcProofOfWork_Part3(const operationBlock: TOperationBlock; out Part3: TRawBytes);
var ms : TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    ms.WriteBuffer(operationBlock.initial_safe_box_hash[Low(operationBlock.initial_safe_box_hash)],length(operationBlock.initial_safe_box_hash));
    ms.WriteBuffer(operationBlock.operations_hash[Low(operationBlock.operations_hash)],length(operationBlock.operations_hash));
    if operationBlock.protocol_version<CT_PROTOCOL_5 then begin
      // Note about fee: Fee is stored in 8 bytes, but only digest first 4 low bytes
      ms.Write(operationBlock.fee,4);
    end else begin
      // UPDATE PROTOCOL 5 - September 2019
      ms.Write(operationBlock.fee,SizeOf(operationBlock.fee)); // Changed from 4 to 8 bytes in Little endian

      // UPDATE PROTOCOL 5 - September 2019
      //
      // IMPORTANT SECURITY FIX:
      //
      // Since protocol 2 the Safebox can be checkpointed, this means that the
      // Safebox can be self-checked mantaining Proof-of-Work consistency.
      // (Introduced on PIP-0003 created by Herman Schoenfeld)
      //
      // The problem is that in order to protect "parallelization" of the PoW
      // process must ensure that cannot create a header for block N until N-1
      // has been created. On V4 version this protection is made using the
      // "initial_safe_box_hash" that is correct and protects parallelization
      // when adding a block on a previous safebox. The issue is that when
      // only computing PoW of the Safebox (without blockchain) then this N-1
      // value obtained on "initial_safe_box_hash" cannot be checked.
      //
      // In order to eliminate parallelization possibility for a hacked
      // new Safebox the best way is to include in N block info obtained
      // after N-1 block has been calculated (like blockchain does).
      //
      // The solution is to include the "previous PoW" obtained on N-1 on the
      // digest for next N block, like a traditional blockchain does
      //
      // This important issue and security fix was discovered by Herman:
      // Herman Schoenfeld <herman@sphere10.com>

      ms.WriteBuffer(operationBlock.previous_proof_of_work[Low(operationBlock.previous_proof_of_work)],Length(operationBlock.previous_proof_of_work));
    end;
    SetLength(Part3,ms.Size);
    ms.Position := 0;
    ms.ReadBuffer(Part3[Low(Part3)],ms.Size);
  finally
    ms.Free;
  end;
end;

class function TPascalCoinProtocol.CalcSafeBoxHash(ABlocksHashBuffer: TBytesBuffer; protocol_version: Integer): TRawBytes;
begin
  // If No buffer to hash is because it's fist block... so use Genesis: CT_Genesis_Magic_String_For_Old_Block_Hash
  if (ABlocksHashBuffer.Length=0) then Result := TPCSafebox.InitialSafeboxHash
  else begin

    // Protection
    Assert((ABlocksHashBuffer.Length MOD 32)=0,'ABlocksHashBuffer invalid length not modulo 32 = 0');
    Assert((ABlocksHashBuffer.Length>0),'ABlocksHashBuffer length = 0');

    if protocol_version<=CT_PROTOCOL_4 then begin
      // A single SHA256 over the full size is made
      Result := TCrypto.DoSha256(ABlocksHashBuffer.Memory,ABlocksHashBuffer.Length);
    end else begin
      // Implementation of PIP-0030, SafeboxHash will be calculated based on a MerkleTree obtaining a SafeboxRootHash
      if ABlocksHashBuffer is TBytesBuffer32Safebox then begin
        TBytesBuffer32Safebox(ABlocksHashBuffer).SafeBoxHashCalcType := sbh_Merkle_Root_Hash;
        Result := TBytesBuffer32Safebox(ABlocksHashBuffer).GetSafeBoxHash;
      end else begin
        Result := TPCSafeboxRootHash.CalcSafeBoxRootHash(ABlocksHashBuffer);
      end;
    end;
  end;
end;

class function TPascalCoinProtocol.CalcTotalBalance(ABlockCount: Cardinal): Int64;
var LCurrReward : Int64;
  LNextBlock : Integer;
begin
  LCurrReward := CT_FirstReward;
  LNextBlock := CT_NewLineRewardDecrease;
  Result := 0;
  while (LNextBlock < ABlockCount) do begin
    inc(Result, Int64(CT_NewLineRewardDecrease * LCurrReward));
    LCurrReward := LCurrReward DIV 2;
    inc(LNextBlock,CT_NewLineRewardDecrease);
  end;
  inc(Result, Int64(Int64(ABlockCount MOD CT_NewLineRewardDecrease) * LCurrReward));
end;

class function TPascalCoinProtocol.AllowUseHardcodedRandomHashTable(
  const AHardcodedFileName: String;
  const AHardcodedSha256Value: TRawBytes): Boolean;
var LTmp : TPCHardcodedRandomHashTable;
  LFileStream : TFileStream;
  LInternalHardcodedSha256 : TRawBytes;
begin
  Result := False;
  {$IFDEF ASSUME_VALID_POW_OLD_PROTOCOLS}
  // In this case will not use Hardcoded RandomHash Table
  Exit;
  {$ENDIF}
  If Not FileExists(AHardcodedFileName) then begin
    TLog.NewLog(ltdebug,ClassName,Format('Hardcoded RandomHash from file not found:%s',
      [AHardcodedFileName] ));
    Exit;
  end;
  LTmp := TPCHardcodedRandomHashTable.Create;
  try
    LFileStream := TFileStream.Create(AHardcodedFileName,fmOpenRead+fmShareDenyNone);
    try
      if LTmp.LoadFromStream(LFileStream,LInternalHardcodedSha256) then begin
        if TBaseType.Equals(LInternalHardcodedSha256, AHardcodedSha256Value) then begin
          if Not Assigned(_INTERNAL_PascalCoinProtocol) then begin
            _INTERNAL_PascalCoinProtocol := TPascalCoinProtocol.Create;
          end;
          _INTERNAL_PascalCoinProtocol.FPCHardcodedRandomHashTable.CopyFrom(LTmp);
          TLog.NewLog(ltinfo,ClassName,Format('Added %d (%d) Hardcoded RandomHash from file:%s (%s)',
            [LTmp.Count,_INTERNAL_PascalCoinProtocol.FPCHardcodedRandomHashTable.Count,
            AHardcodedFileName,AHardcodedSha256Value.ToHexaString] ));
          Result := True;
        end;
      end;
      if not Result then begin
         TLog.NewLog(lterror,ClassName,Format('Hardcoded RandomHash file invalid:%s (%s %s) %d',
           [AHardcodedFileName,AHardcodedSha256Value.ToHexaString,
            LInternalHardcodedSha256.ToHexaString,
            LTmp.Count] ));
      end;
    finally
      LFileStream.Free;
    end;
  finally
    LTmp.Free;
  end;
end;

constructor TPascalCoinProtocol.Create;
begin
  FPCHardcodedRandomHashTable := TPCHardcodedRandomHashTable.Create;
end;

destructor TPascalCoinProtocol.Destroy;
begin
  FreeAndNil(FPCHardcodedRandomHashTable);
  inherited;
end;

class procedure TPascalCoinProtocol.CalcProofOfWork(const operationBlock: TOperationBlock; out PoW: TRawBytes);
var ms : TMemoryStream;
  accKeyRaw : TRawBytes;
  LDigest : TRawBytes;
begin
  ms := TMemoryStream.Create;
  try
    // Part 1
    ms.Write(operationBlock.block,Sizeof(operationBlock.block)); // Little endian
    accKeyRaw := TAccountComp.AccountKey2RawString(operationBlock.account_key);
    ms.WriteBuffer(accKeyRaw[Low(accKeyRaw)],Length(accKeyRaw));
    ms.Write(operationBlock.reward,Sizeof(operationBlock.reward)); // Little endian
    ms.Write(operationBlock.protocol_version,Sizeof(operationBlock.protocol_version)); // Little endian
    ms.Write(operationBlock.protocol_available,Sizeof(operationBlock.protocol_available)); // Little endian
    ms.Write(operationBlock.compact_target,Sizeof(operationBlock.compact_target)); // Little endian
    // Part 2
    ms.WriteBuffer(operationBlock.block_payload[Low(operationBlock.block_payload)],Length(operationBlock.block_payload));
    // Part 3
    ms.WriteBuffer(operationBlock.initial_safe_box_hash[Low(operationBlock.initial_safe_box_hash)],length(operationBlock.initial_safe_box_hash));
    ms.WriteBuffer(operationBlock.operations_hash[Low(operationBlock.operations_hash)],length(operationBlock.operations_hash));
    if operationBlock.protocol_version<CT_PROTOCOL_5 then begin
      // Note about fee: Fee is stored in 8 bytes (Int64), but only digest first 4 low bytes
      ms.Write(operationBlock.fee,4);
    end else begin
      // UPDATE PROTOCOL 5 - September 2019
      ms.Write(operationBlock.fee,SizeOf(operationBlock.fee)); // Changed from 4 to 8 bytes in Little endian
      ms.WriteBuffer(operationBlock.previous_proof_of_work[Low(operationBlock.previous_proof_of_work)],Length(operationBlock.previous_proof_of_work));
    end;
    ms.Write(operationBlock.timestamp,4);
    ms.Write(operationBlock.nonce,4);
    if CT_ACTIVATE_RANDOMHASH_V4 AND (operationBlock.protocol_version >= CT_PROTOCOL_4) then begin
      if (operationBlock.protocol_version < CT_PROTOCOL_5) then begin
        if Assigned(_INTERNAL_PascalCoinProtocol) then begin
          SetLength(LDigest,ms.Size);
          Move(ms.Memory^,LDigest[0],ms.Size);
          if _INTERNAL_PascalCoinProtocol.FPCHardcodedRandomHashTable.FindRandomHashByDigest(LDigest,PoW) then Exit;
        end;
        TCrypto.DoRandomHash(ms.Memory,ms.Size,PoW);
      end else TCrypto.DoRandomHash2(ms.Memory,ms.Size,PoW);
    end else
      TCrypto.DoDoubleSha256(ms.Memory,ms.Size,PoW);
  finally
    ms.Free;
  end;
end;

class function TPascalCoinProtocol.IsValidMinerBlockPayload(const newBlockPayload: TRawBytes): Boolean;
var i : Integer;
begin
  Result := False;
  if Length(newBlockPayload)>CT_MaxPayloadSize then Exit;
  // Checking Miner Payload valid chars
  for i := Low(newBlockPayload) to High(newBlockPayload) do begin
    if Not (newBlockPayload[i] in [32..254]) then begin
      exit;
    end;
  end;
  Result := True;
end;

class procedure TPascalCoinProtocol.GetRewardDistributionForNewBlock(const OperationBlock : TOperationBlock; out acc_0_miner_reward, acc_4_dev_reward : Int64; out acc_4_for_dev : Boolean);
begin
  if OperationBlock.protocol_version<CT_PROTOCOL_3 then begin
    acc_0_miner_reward := OperationBlock.reward + OperationBlock.fee;
    acc_4_dev_reward := 0;
    acc_4_for_dev := False;
  end else begin
    acc_4_dev_reward := (OperationBlock.reward * CT_Protocol_v3_PIP11_Percent) DIV 100;
    acc_0_miner_reward := OperationBlock.reward + OperationBlock.fee - acc_4_dev_reward;
    acc_4_for_dev := True;
  end;
end;

class function TPascalCoinProtocol.GetRewardForNewLine(line_index: Cardinal): UInt64;
Var n, i : Cardinal;
begin
  {$IFDEF TESTNET}
  // TESTNET used (line_index +1), but PRODUCTION must use (line_index)
  n := (line_index + 1) DIV CT_NewLineRewardDecrease;  // TESTNET BAD USE (line_index + 1)
  {$ELSE}
  n := line_index DIV CT_NewLineRewardDecrease; // FOR PRODUCTION
  {$ENDIF}
  Result := CT_FirstReward;
  for i := 1 to n do begin
    Result := Result DIV 2;
  end;
  if (Result < CT_MinReward) then
    Result := CT_MinReward;
end;

class function TPascalCoinProtocol.MinimumTarget(protocol_version: Integer): Cardinal;
begin
  if protocol_version>=CT_PROTOCOL_5 then begin
    Result := CT_MinCompactTarget_v5
  end else if protocol_version>=CT_PROTOCOL_4 then begin
    Result := CT_MinCompactTarget_v4;
  end else begin
    Result := CT_MinCompactTarget_v1;
  end;
end;

class function TPascalCoinProtocol.ResetTarget(current_target: Cardinal; protocol_version: Integer): Cardinal;
begin
  {$IFDEF ACTIVATE_RANDOMHASH_V4}
  if protocol_version=CT_PROTOCOL_4 then begin
    Result := CT_CompactTarget_Reset_v4
  end else Result := current_target;
  {$ELSE}
  Result := current_target;
  {$ENDIF}
end;

class function TPascalCoinProtocol.TargetFromCompact(encoded: Cardinal; protocol_version : Integer): TRawBytes;
Var
  nbits, offset, i: Cardinal;
  bn: TBigNum;
  raw : TRawBytes;
begin
  {
    Compact Target is a 4 byte value that tells how many "0" must have the hash at left if presented in binay format.
    First byte indicates haw many "0 bits" are on left, so can be from 0x00 to 0xE7
    (Because 24 bits are reserved for 3 bytes, and 1 bit is implicit, max: 256-24-1=231=0xE7)
    Next 3 bytes indicates next value in XOR, stored in RAW format

    Example: If we want a hash lower than 0x0000 0000 0000 65A0 A2F4 +29 bytes
    Binary "0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0110 0101 1010 0000 1010 0010 1111 0100"
    That is 49 zeros on left before first 1. So first byte is 49 decimal = 0x31
    After we have "110 0101 1010 0000 1010 0010 1111 0100 1111 0100" but we only can accept first 3 bytes,
    also note that first "1" is implicit, so value is transformed in
    binary as "10 0101 1010 0000 1010 0010 11" that is 0x96828B
    But note that we must XOR this value, so result offset is: 0x697D74
    Compacted value is: 0x31697D74

    When translate compact target back to target: ( 0x31697D74 )
    0x31 = 49 bits at "0", then 1 bit at "1" followed by XOR 0x697D74 = 0x96828B
    49 "0" bits "0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0"
    0x96828B "1001 0110 1000 0010 1000 1011"
    Hash target = "0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0110 0101 1010 0000 1010 0010 11.. ...."
    Fill last "." with "1"
    Hash target = "0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0110 0101 1010 0000 1010 0010 1111 1111"
    Hash target = 0x00 00 00 00 00 00 65 A0 A2 FF + 29 bytes
    Note that is not exactly the same than expected due to compacted format
    }
  nbits := encoded shr 24;

  i := MinimumTarget(protocol_version) shr 24;
  if nbits < i then
    nbits := i; // min nbits
  if nbits > 231 then
    nbits := 231; // max nbits

  offset := (encoded shl 8) shr 8;
  // Make a XOR at offset and put a "1" on the left
  offset := ((offset XOR $00FFFFFF) OR ($01000000));

  bn := TBigNum.Create(offset);
  Try
    bn.LShift(256 - nbits - 25);
    raw := bn.RawValue;
    SetLength(Result,32);
    FillChar(Result[Low(Result)],32,0);
    for i:=Low(raw) to High(raw) do begin
      result[i+32-Length(raw)] := raw[i];
    end;
  Finally
    bn.Free;
  End;
end;

class function TPascalCoinProtocol.TargetToCompact(target: TRawBytes; protocol_version : Integer): Cardinal;
Var
  bn, bn2: TBigNum;
  i: Int64;
  nbits, min_compact_target: Cardinal;
  raw : TRawBytes;
  j : Integer;
begin
  { See instructions in explanation of TargetFromCompact }
  Result := 0;
  if length(target)>32 then begin
    raise Exception.Create('Invalid target to compact: '+TCrypto.ToHexaString(target)+' ('+inttostr(length(target))+')');
  end;
  SetLength(raw,32);
  FillChar(raw[Low(raw)],32,0);
  for j:=Low(target) to High(target) do begin
    raw[j+32-length(target)] := target[j];
  end;
  target := raw;

  bn := TBigNum.Create(0);
  bn2 := TBigNum.Create('8000000000000000000000000000000000000000000000000000000000000000'); // First bit 1 followed by 0
  try
    bn.RawValue := target;
    nbits := 0;
    while (bn.CompareTo(bn2) < 0) And (nbits < 231) do
    begin
      bn2.RShift(1);
      inc(nbits);
    end;

    min_compact_target := MinimumTarget(protocol_version);
    i := min_compact_target shr 24;
    if (nbits < i) then
    begin
      Result := min_compact_target;
      exit;
    end;

    bn.RShift((256 - 25) - nbits);
    Result := (nbits shl 24) + ((bn.value AND $00FFFFFF) XOR $00FFFFFF);
  finally
    bn.Free;
    bn2.Free;
  end;
end;

{ TStreamOp }

class function TStreamOp.ReadAccountKey(Stream: TStream; var value: TAccountKey): Integer;
begin
  if Stream.Size - Stream.Position < 2 then begin
    value := CT_TECDSA_Public_Nul;
    Result := -1;
    exit;
  end;
  stream.Read(value.EC_OpenSSL_NID,SizeOf(value.EC_OpenSSL_NID));
  if (ReadAnsiString(stream,value.x)<=0) then begin
    value := CT_TECDSA_Public_Nul;
    Result := -1;
    exit;
  end;
  if (ReadAnsiString(stream,value.y)<=0) then begin
    value := CT_TECDSA_Public_Nul;
    Result := -1;
    exit;
  end;
  Result := value.EC_OpenSSL_NID;
end;

class function TStreamOp.SaveStreamToRaw(Stream: TStream): TRawBytes;
begin
  SetLength(Result,Stream.Size);
  Stream.Position:=0;
  Stream.ReadBuffer(Result[Low(Result)],Stream.Size);
end;

class procedure TStreamOp.LoadStreamFromRaw(Stream: TStream; const raw: TRawBytes);
begin
  Stream.WriteBuffer(raw[Low(raw)],Length(raw));
end;

class function TStreamOp.ReadAnsiString(Stream: TStream; var value: TRawBytes; ACheckLength : Integer = 0): Integer;
Var
  w: Word;
begin
  if Stream.Size - Stream.Position < 2 then begin
    SetLength(value,0);
    Result := -1;
    Exit;
  end;
  Stream.Read(w, 2);
  if (Stream.Size - Stream.Position < w) OR ((ACheckLength > 0) AND (w <> ACheckLength)) then begin
    Stream.Position := Stream.Position - 2; // Go back!
    SetLength(value,0);
    Result := -1;
    Exit;
  end;
  SetLength(value, w);
  if (w>0) then begin
    Stream.ReadBuffer(value[Low(value)], w);
  end;
  Result := w+2;
end;

class function TStreamOp.ReadAnsiString(Stream: TStream;var value: T32Bytes): Integer;
var
  LBytes : TRawBytes;
   i : Integer;
begin
  Result := ReadAnsiString(Stream, LBytes, 32);
  if Result > 0 then
    value := TBaseType.To32Bytes(LBytes);
end;


class function TStreamOp.ReadGUID(AStream: TStream; var AGUID: TGUID): Integer;
var i : Integer;
begin
  if AStream.Size - AStream.Position < 16 then begin
    Result := 0; // Not enough space!
    Exit;
  end;
  AStream.Read(AGUID.D1,4);
  AStream.Read(AGUID.D2,2);
  AStream.Read(AGUID.D3,2);
  for i := 0 to 7 do begin
    AStream.Read(AGUID.D4[i],1);
  end;
  Result := 16; // GUID is 16 bytes
end;

class function TStreamOp.ReadString(Stream: TStream; var value: String): Integer;
var raw : TRawBytes;
begin
  Result := ReadAnsiString(Stream,raw);
  value := raw.ToString;
end;

class function TStreamOp.ReadTBytes(Stream: TStream;
  var ABytes: TBytes; ACheckLength : Integer = 0): Integer;
var LSize : Integer;
begin
  if Stream.Size - Stream.Position < 4 then begin
    SetLength(ABytes,0);
    Result := -1;
    Exit;
  end;
  LSize := 0;
  Stream.Read(LSize, 4);
  if (Stream.Size - Stream.Position < LSize) OR ((ACheckLength > 0) AND (LSize <> ACheckLength)) then begin
    Stream.Position := Stream.Position - 4; // Go back!
    SetLength(ABytes,0);
    Result := -1;
    Exit;
  end;
  SetLength(ABytes, LSize);
  if (LSize>0) then begin
    Stream.ReadBuffer(ABytes[Low(ABytes)], LSize);
  end;
  Result := LSize+4;
end;

class function TStreamOp.WriteAccountKey(Stream: TStream; const value: TAccountKey): Integer;
begin
  Result := stream.Write(value.EC_OpenSSL_NID, SizeOf(value.EC_OpenSSL_NID));
  Result := Result + WriteAnsiString(stream,value.x);
  Result := Result + WriteAnsiString(stream,value.y);
end;

class function TStreamOp.WriteAnsiString(Stream: TStream; const value: TRawBytes): Integer;
Var
  w: Word;
begin
  if (Length(value)>(256*256)) then begin
    TLog.NewLog(lterror,Classname,'Invalid stream size! '+Inttostr(Length(value)));
    raise Exception.Create('Invalid stream size! '+Inttostr(Length(value)));
  end;

  w := Length(value);
  Stream.Write(w, 2);
  if (w > 0) then
    Stream.WriteBuffer(value[Low(value)], Length(value));
  Result := w+2;
end;

class function TStreamOp.WriteAnsiString(Stream: TStream; const value: T32Bytes): Integer;
begin
  Result := WriteAnsiString(Stream, TBaseType.ToRawBytes(value));
end;


class function TStreamOp.WriteGUID(AStream: TStream; const AGUID: TGUID): Integer;
var i : Integer;
begin
  AStream.Write(AGUID.D1,4);
  AStream.Write(AGUID.D2,2);
  AStream.Write(AGUID.D3,2);
  for i := 0 to 7 do begin
    AStream.Write(AGUID.D4[i],1);
  end;
  Result := 16; // GUID is 16 bytes
end;

class function TStreamOp.WriteString(Stream: TStream;
  const value: String): Integer;
var LRaw : TRawBytes;
begin
  LRaw.FromString(value);
  Result := WriteAnsiString(Stream,LRaw);
end;

class function TStreamOp.WriteTBytes(Stream: TStream;
  const value: TBytes): Integer;
Var LSize : Integer;
begin
  if (Length(value)>MAXINT) then begin
    TLog.NewLog(lterror,Classname,'Invalid stream size! '+Inttostr(Length(value))+' '+MAXINT.ToString);
    raise Exception.Create('Invalid stream size! '+Inttostr(Length(value))+' '+MAXINT.ToString);
  end;
  LSize := Length(value);
  Stream.Write(LSize, 4);
  if (LSize > 0) then
    Stream.WriteBuffer(value[Low(value)], Length(value));
  Result := LSize+4;
end;

{ TAccountComp }
Const CT_Base58 : String = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz';

class function TAccountComp.AccountBlock(const account_number: Cardinal): Cardinal;
begin
  Result := account_number DIV CT_AccountsPerBlock;
end;

class function TAccountComp.AccountInfo2RawString(const AccountInfo: TAccountInfo): TRawBytes;
begin
  AccountInfo2RawString(AccountInfo,Result);
end;

class procedure TAccountComp.AccountInfo2RawString(const AccountInfo: TAccountInfo; var dest: TRawBytes);
Var ms : TMemoryStream;
  w : Word;
begin
  case AccountInfo.state of
    as_Normal: AccountKey2RawString(AccountInfo.accountKey,dest);
    as_ForSale, as_ForAtomicAccountSwap, as_ForAtomicCoinSwap: begin
      case AccountInfo.state of
        as_ForSale: w := CT_AccountInfo_ForSale;
        as_ForAtomicAccountSwap: w := CT_AccountInfo_ForAccountSwap;
        as_ForAtomicCoinSwap: w := CT_AccountInfo_ForCoinSwap;
      end;
      ms := TMemoryStream.Create;
      Try

        ms.Write(w,SizeOf(w));
        //
        TStreamOp.WriteAccountKey(ms,AccountInfo.accountKey);
        ms.Write(AccountInfo.locked_until_block,SizeOf(AccountInfo.locked_until_block));
        ms.Write(AccountInfo.price,SizeOf(AccountInfo.price));
        ms.Write(AccountInfo.account_to_pay,SizeOf(AccountInfo.account_to_pay));
        TStreamOp.WriteAccountKey(ms,AccountInfo.new_publicKey);
        // Adding Hashed_secret if Atomic Swap
        if AccountInfo.state in [as_ForAtomicAccountSwap,as_ForAtomicCoinSwap] then begin
          TStreamOp.WriteAnsiString(ms,AccountInfo.hashed_secret);
        end;
        SetLength(dest,ms.Size);
        ms.Position := 0;
        ms.Read(dest[Low(dest)],ms.Size);
      Finally
        ms.Free;
      end;
    end;
  else
    raise Exception.Create('DEVELOP ERROR 20170214-1');
  end;
end;

class procedure TAccountComp.SaveAccountToAStream(Stream: TStream; const Account: TAccount; current_protocol : Word);
var w : Word;
  LTmpSeal : T20Bytes;
  LTmpRaw : TRawBytes;
  LCardinal : Cardinal;
begin
  if current_protocol<CT_PROTOCOL_5 then
    w := CT_PROTOCOL_4
  else w := current_protocol;
  Stream.Write(w,SizeOf(w));
  Stream.Write(Account.account,Sizeof(Account.account));
  TStreamOp.WriteAnsiString(Stream,AccountInfo2RawString(Account.accountInfo));
  Stream.Write(Account.balance,Sizeof(Account.balance));
  if current_protocol>=CT_PROTOCOL_5 then begin
    Stream.Write(Account.updated_on_block_passive_mode,Sizeof(Account.updated_on_block_passive_mode));
    Stream.Write(Account.updated_on_block_active_mode,Sizeof(Account.updated_on_block_active_mode));
  end else begin
    LCardinal := Account.GetLastUpdatedBlock;
    Stream.Write(LCardinal,Sizeof(LCardinal));
  end;
  Stream.Write(Account.n_operation,Sizeof(Account.n_operation));
  TStreamOp.WriteAnsiString(Stream,Account.name);
  Stream.Write(Account.account_type,SizeOf(Account.account_type));
  if current_protocol>=CT_PROTOCOL_5 then begin
    TStreamOp.WriteAnsiString(Stream,Account.account_data);
    // Account Seal is allways a 20 bytes as described on PIP-0029
    LTmpSeal := TBaseType.To20Bytes(Account.account_seal);
    LTmpRaw := TBaseType.T20BytesToRawBytes(LTmpSeal);
    TStreamOp.WriteAnsiString(Stream,LTmpRaw);
  end;
end;

class function TAccountComp.LoadAccountFromStream(Stream: TStream; var Account: TAccount): Boolean;
var LSaved_protocol : Word;
  raw : TRawBytes;
begin
  Account := CT_Account_NUL;
  Result := False;
  if (Stream.Size - Stream.Position<8) then Exit;
  Stream.Read(LSaved_protocol,SizeOf(LSaved_protocol));
  if Not (LSaved_protocol in [CT_PROTOCOL_4..CT_PROTOCOL_MAX]) then Exit;
  Stream.Read(Account.account,Sizeof(Account.account));
  if TStreamOp.ReadAnsiString(Stream,raw) < 0 then Exit;
  TAccountComp.RawString2AccountInfo(raw,Account.accountInfo);
  if (Stream.Size - Stream.Position<20) then Exit;
  Stream.Read(Account.balance,Sizeof(Account.balance));
  Stream.Read(Account.updated_on_block_passive_mode,Sizeof(Account.updated_on_block_passive_mode));
  if LSaved_protocol>=CT_PROTOCOL_5 then begin
    Stream.Read(Account.updated_on_block_active_mode,Sizeof(Account.updated_on_block_active_mode));
  end else Account.updated_on_block_active_mode := Account.updated_on_block_passive_mode;
  Stream.Read(Account.n_operation,Sizeof(Account.n_operation));
  if TStreamOp.ReadAnsiString(Stream,Account.name)<0 then Exit;
  if Stream.Read(Account.account_type,SizeOf(Account.account_type)) <> 2 then Exit;
  if LSaved_protocol>=CT_PROTOCOL_5 then begin
    if TStreamOp.ReadAnsiString(Stream,Account.account_data)<0 then Exit;
    if TStreamOp.ReadAnsiString(Stream,Account.account_seal,20)<0 then Exit;
  end;
  Result := True;
end;

class function TAccountComp.AccountKey2RawString(const account: TAccountKey): TRawBytes;
begin
  AccountKey2RawString(account,Result);
end;

class procedure TAccountComp.AccountKey2RawString(const account: TAccountKey; var dest: TRawBytes);
Var s : TMemoryStream;
begin
  s := TMemoryStream.Create;
  try
    TStreamOp.WriteAccountKey(s,account);
    SetLength(dest,s.Size);
    s.Position := 0;
    s.Read(dest[Low(dest)],s.Size);
  finally
    s.Free;
  end;
end;

class function TAccountComp.AccountKeyFromImport(const HumanReadable: String; var account: TAccountKey; var errors : String): Boolean;
begin
  Result := AccountPublicKeyImport(HumanReadable,account,errors);
end;

class function TAccountComp.AccountNumberToAccountTxtNumber(account_number: Cardinal): String;
Var an : int64;
begin
  an := account_number; // Converting to int64 to prevent overflow when *101
  an := ((an * 101) MOD 89)+10;
  Result := IntToStr(account_number)+'-'+Inttostr(an);
end;

class function TAccountComp.AccountPublicKeyExport(const account: TAccountKey): String;
Var raw : TRawBytes;
  BN, BNMod, BNDiv : TBigNum;
begin
  Result := '';
  raw := AccountKey2RawString(account);
  BN := TBigNum.Create;
  BNMod := TBigNum.Create;
  BNDiv := TBigNum.Create(Length(CT_Base58));
  try
    BN.HexaValue := '01'+raw.ToHexaString+TCrypto.ToHexaString(Copy(TCrypto.DoSha256(raw),0,4));
    while (Not BN.IsZero) do begin
      BN.Divide(BNDiv,BNMod);
      If (BNMod.Value>=0) And (BNMod.Value<Length(CT_Base58)) then Result := CT_Base58.Chars[Byte(BNMod.Value)] + Result
      else raise Exception.Create('Error converting to Base 58');
    end;
  finally
    BN.Free;
    BNMod.Free;
    BNDiv.Free;
  end;
end;

class function TAccountComp.AccountPublicKeyImport(const HumanReadable: String; var account: TAccountKey; var errors: String): Boolean;
Var raw,rawPubKey,rawChecksum_Calculated,rawChecksum_Stored : TRawBytes;
  BN, BNAux, BNBase : TBigNum;
  i,j : Integer;
begin
  Result := false;
  account := CT_TECDSA_Public_Nul;
  if Length(HumanReadable)<10 then begin
    errors := 'Invalid length';
    Exit(False);
  end;
  BN := TBigNum.Create(0);
  BNAux := TBigNum.Create;
  BNBase := TBigNum.Create(1);
  try
    for i := Length(HumanReadable)-1 downto 0 do begin
      j := CT_Base58.IndexOf(HumanReadable.Chars[i]);
      if j<0 then begin
        errors := 'Invalid char "'+HumanReadable.Chars[i]+'" at pos '+inttostr(i+1)+'/'+inttostr(Length(HumanReadable));
        Exit(False);
      end;
      BNAux.Value := j;
      BNAux.Multiply(BNBase);
      BN.Add(BNAux);
      BNBase.Multiply(Length(CT_Base58));
    end;
    // Last 8 hexa chars are the checksum of others
    raw := TCrypto.HexaToRaw(BN.HexaValue);
    if (Length(raw)<5) then begin
      errors := 'Invalid decoded';
      Exit(False);
    end;
    if (raw[0]<>1) then begin
      errors := 'Invalid start value';
      Exit(False);
    end;
    rawPubKey := Copy(raw,1,Length(raw)-5);
    rawChecksum_Stored := Copy(raw,Length(raw)-4,4);
    rawChecksum_Calculated := Copy(TCrypto.DoSha256(rawPubKey),0,4);
    if (Not TBaseType.Equals(rawChecksum_Calculated,rawChecksum_Stored)) then begin
      // Invalid checksum
      errors := 'Invalid checksum';
      Exit(False);
    end;
    try
      account := TAccountComp.RawString2Accountkey(rawPubKey);
      errors := '';
      Result := True;
    except
      // Nothing to do... invalid
      errors := 'Error on conversion from Raw to Account key';
      Result := False;
    end;
  Finally
    BN.Free;
    BNBase.Free;
    BNAux.Free;
  end;
end;

class function TAccountComp.AccountTxtNumberToAccountNumber(const account_txt_number: String; var account_number: Cardinal): Boolean;
  // PascalCoin account number can be a simple number "123456" or a number with checksum separated by "-","." or "_"
  // The CHECKSUM is 2 digits between 10..88 (89 posibilities)
  // Calculation of checksum: Account number multiplied by 101, moduled by 89 and adding 10
  // Example:
  //   "1234" : Good, no checksum
  //   "1234-5" : Invalid. Correct checksum is 44  (((1234 * 101) MODULUS 89)+10)=44
  //   "1234-44" : Good, checksum for 1234 is 44
  //   "1234 44" : Invalid. Separator is not "-","." or "_"
Var i : Integer;
  an,rn,anaux : Int64;
begin
  if Length(Trim(account_txt_number))=0 then Exit(False);
  an := 0;
  i := 0;
  while (i<Length(account_txt_number)) do begin
    if account_txt_number.Chars[i] in ['0'..'9'] then begin
      an := (an * 10) + ord( account_txt_number.Chars[i] ) - ord('0');
    end else begin
      Break;
    end;
    inc(i);
  end;
  account_number := an;
  if (i>=Length(account_txt_number)) then begin
    Exit(True);
  end;
  if (account_txt_number.Chars[i] in ['-','.','_']) then inc(i);
  if i>=Length(account_txt_number) then Exit(False);  // Found invalid chars after account number
  rn := StrToIntDef(account_txt_number.Substring(i,Length(account_txt_number)),0);
  anaux := ((an * 101) MOD 89)+10;
  Result := rn = anaux;
end;

class function TAccountComp.EqualAccountInfos(const accountInfo1,accountInfo2 : TAccountInfo) : Boolean;
begin
  Result := (accountInfo1.state = accountInfo2.state) And (EqualAccountKeys(accountInfo1.accountKey,accountInfo2.accountKey))
    And (accountInfo1.locked_until_block = accountInfo2.locked_until_block) And (accountInfo1.price = accountInfo2.price)
    And (accountInfo1.account_to_pay = accountInfo2.account_to_pay) and (EqualAccountKeys(accountInfo1.new_publicKey,accountInfo2.new_publicKey));
end;

class function TAccountComp.EqualAccountKeys(const account1, account2: TAccountKey): Boolean;
begin
  Result := (account1.EC_OpenSSL_NID=account2.EC_OpenSSL_NID) And
    (TBaseType.Equals(account1.x,account2.x)) And (TBaseType.Equals(account1.y,account2.y));
end;

class function TAccountComp.EqualAccounts(const account1, account2: TAccount): Boolean;
begin
  Result := (account1.account = account2.account)
          And (EqualAccountInfos(account1.accountInfo,account2.accountInfo))
          And (account1.balance = account2.balance)
          And (account1.updated_on_block_passive_mode = account2.updated_on_block_passive_mode)
          And (account1.updated_on_block_active_mode = account2.updated_on_block_active_mode)
          And (account1.n_operation = account2.n_operation)
          And (TBaseType.Equals(account1.name,account2.name))
          And (account1.account_type = account2.account_type)
          And (TBaseType.Equals(account1.account_data,account2.account_data))
          And (TBaseType.Equals(account1.account_seal,account2.account_seal));
end;

class function TAccountComp.EqualOperationBlocks(const opBlock1, opBlock2: TOperationBlock): Boolean;
begin
  Result := (opBlock1.block = opBlock1.block)
          And (EqualAccountKeys(opBlock1.account_key,opBlock2.account_key))
          And (opBlock1.reward = opBlock2.reward)
          And (opBlock1.fee = opBlock2.fee)
          And (opBlock1.protocol_version = opBlock2.protocol_version)
          And (opBlock1.protocol_available = opBlock2.protocol_available)
          And (opBlock1.timestamp = opBlock2.timestamp)
          And (opBlock1.compact_target = opBlock2.compact_target)
          And (opBlock1.nonce = opBlock2.nonce)
          And (TBaseType.Equals(opBlock1.block_payload,opBlock2.block_payload))
          And (TBaseType.Equals(opBlock1.initial_safe_box_hash,opBlock2.initial_safe_box_hash))
          And (TBaseType.Equals(opBlock1.operations_hash,opBlock2.operations_hash))
          And (TBaseType.Equals(opBlock1.proof_of_work,opBlock2.proof_of_work))
          And ( (opBlock1.protocol_version < CT_PROTOCOL_5)
                OR
                (TBaseType.Equals(opBlock1.previous_proof_of_work,opBlock2.previous_proof_of_work))
              );
end;

class function TAccountComp.EqualBlockAccounts(const blockAccount1, blockAccount2: TBlockAccount): Boolean;
Var i : Integer;
begin
  Result := (EqualOperationBlocks(blockAccount1.blockchainInfo,blockAccount2.blockchainInfo))
          And (TBaseType.Equals(blockAccount1.block_hash,blockAccount2.block_hash))
          And (blockAccount1.accumulatedWork = blockAccount2.accumulatedWork);
  If Result then begin
    for i:=Low(blockAccount1.accounts) to High(blockAccount1.accounts) do begin
      Result := EqualAccounts(blockAccount1.accounts[i],blockAccount2.accounts[i]);
      If Not Result then Exit;
    end;
  end;
end;

// Deprecated
class function TAccountComp.FormatMoney(Money: Int64): String;
begin
  Result := TPASCEncoding.Encode(Money);
end;

class function TAccountComp.FormatMoneyDecimal(Money : Int64) : Currency;
var Ltmp : Double;
begin
  Ltmp := Money;
  Ltmp := Ltmp / 10000.0;
  Result := RoundTo( Ltmp , -4);
end;

class function TAccountComp.GetECInfoTxt(const EC_OpenSSL_NID: Word): String;
begin
  case EC_OpenSSL_NID of
    CT_NID_secp256k1 : begin
      Result := 'secp256k1';
    end;
    CT_NID_secp384r1 : begin
      Result := 'secp384r1';
    end;
    CT_NID_sect283k1 : Begin
      Result := 'secp283k1';
    End;
    CT_NID_secp521r1 : begin
      Result := 'secp521r1';
    end
  else Result := '(Unknown ID:'+inttostr(EC_OpenSSL_NID)+')';
  end;
end;

class function TAccountComp.IsAccountBlockedByProtocol(account_number, blocks_count: Cardinal): Boolean;
begin
  if blocks_count<CT_WaitNewBlocksBeforeTransaction then result := true
  else begin
    Result := ((blocks_count-CT_WaitNewBlocksBeforeTransaction) * CT_AccountsPerBlock) <= account_number;
  end;
end;

Class Function TAccountComp.IsValidAccountInfoHashLockKey(const AAccountInfo : TAccountInfo; const AKey : TRawBytes) : Boolean;
begin
  Result := BytesEqual( TBaseType.ToRawBytes( CalculateHashLock( AKey ) ), AAccountInfo.hashed_secret);
end;

Class Function TAccountComp.IsValidHashLockKey(const AKey : TRawBytes; out AError : String) : Boolean;
begin
  if (Length(AKey) < CT_HashLockKey_MinBytes) OR (Length(AKey) > CT_HashLockKey_MaxBytes) then begin
    AError := Format('Hash-lock key must be %d to %d bytes in length',[CT_HashLockKey_MinBytes,CT_HashLockKey_MaxBytes]);
    Exit(False);
  end;
  Result := true;
end;

Class Function TAccountComp.CalculateHashLock(const AKey : TRawBytes) : T32Bytes;
begin
  Result := TBaseType.To32Bytes( TCrypto.DoSha256(AKey) );
end;

class function TAccountComp.IsAccountForSale(const AAccountInfo: TAccountInfo): Boolean;
begin
  Result := IsAccountForPrivateSale(AAccountInfo) OR IsAccountForPublicSale(AAccountInfo);
end;

class function TAccountComp.IsAccountForPrivateSale(const AAccountInfo: TAccountInfo): Boolean;
begin
  Result := (AAccountInfo.state in [as_ForSale]) AND (NOT IsNullAccountKey(AAccountInfo.new_publicKey));
end;

class function TAccountComp.IsAccountForPublicSale(const AAccountInfo: TAccountInfo): Boolean;
begin
  Result := (AAccountInfo.state in [as_ForSale]) AND IsNullAccountKey(AAccountInfo.new_publicKey);
end;

class function TAccountComp.IsAccountForSwap(const AAccountInfo: TAccountInfo): Boolean;
begin
  Result := IsAccountForAccountSwap(AAccountInfo) OR IsAccountForCoinSwap(AAccountInfo);
end;

class function TAccountComp.IsAccountForAccountSwap(const AAccountInfo: TAccountInfo) : Boolean;
begin
  Result := (AAccountInfo.state in [as_ForAtomicAccountSwap]);
end;

class function TAccountComp.IsAccountForCoinSwap(const AAccountInfo: TAccountInfo) : Boolean;
begin
  Result := (AAccountInfo.state in [ as_ForAtomicCoinSwap]);
end;

class function TAccountComp.IsAccountForSaleOrSwap(const AAccountInfo: TAccountInfo): Boolean;
begin
  Result := IsAccountForSale(AAccountInfo) OR IsAccountForSwap(AAccountInfo);
end;

class function TAccountComp.IsAccountForSaleOrSwapAcceptingTransactions(const AAccount: TAccount; ACurrentBlock : Integer; ACurrentProtocol : Word; const APayload : TRawBytes): Boolean;
var errors : String;
begin
  Result := False;
  if Not IsAccountForSaleOrSwap(AAccount.accountInfo) then
    exit;

  if (ACurrentProtocol<CT_PROTOCOL_5) then begin
    // V4 and below only allows Private sales (No Swaps)
    if Not (IsAccountForPrivateSale(AAccount.accountInfo)) then Exit;
  end else begin
    // V5 only will allow PRIVATE SALES or SWAPS while locked
    if (IsAccountForPublicSale(AAccount.accountInfo)) Then Exit; // Public sales not allowed
    if (Not (IsAccountLocked(AAccount.accountInfo,ACurrentBlock))) then Exit; // Time lock expired
  end;

  if (AAccount.accountInfo.state in [as_ForSale, as_ForAtomicAccountSwap]) then begin
    if NOT IsValidAccountKey(AAccount.accountInfo.new_publicKey,ACurrentProtocol,errors) then
      exit;
  end;

  if (AAccount.accountInfo.state in [as_ForAtomicAccountSwap, as_ForAtomicCoinSwap]) then begin
    if NOT IsValidAccountInfoHashLockKey(AAccount.accountInfo, APayload) then
      exit;
  end;
  Result := True;
end;

Class Function TAccountComp.IsOperationRecipientSignable(const ASender, ATarget : TAccount; ACurrentBlock : Integer; ACurrentProtocol : Word) : Boolean;
begin
  // V5 - Allow recipient-signed operations under following conditions:
  //  - Sender Account = Target Account
  //  - Target Account is listed:
  //      - Listed for ACCOUNT SWAP
  //      or
  //      - Listed for PRIVATE SALE
  //  - Target Account is time-locked and time-lock is active
  //
  //  This allows following use-cases:
  //  - Private account sale where buyer does not have existing account to initiate transaction
  //  - Atomic account swap where counterparty does not have existing account to initiate transaction
  //
  // Note: this does not validate recipient signature, only determines if
  // it is recipient signable
  //
  Result := (ACurrentProtocol >= CT_PROTOCOL_5) AND
            (ASender.account = ATarget.account) AND
            TAccountComp.IsAccountLocked(ATarget.accountInfo, ACurrentBlock) AND
            (
              (TAccountComp.IsAccountForAccountSwap(ATarget.accountInfo))
              OR
              (TAccountComp.IsAccountForPrivateSale(ATarget.accountInfo))
            );
end;

class function TAccountComp.IsAccountLocked(const AccountInfo: TAccountInfo; blocks_count: Cardinal): Boolean;
begin
  Result := IsAccountForSaleOrSwap(accountInfo) And ((AccountInfo.locked_until_block)>=blocks_count);
end;

class procedure TAccountComp.SaveTOperationBlockToStream(const stream: TStream; const operationBlock: TOperationBlock);
begin
  stream.Write(operationBlock.block, Sizeof(operationBlock.block));
  TStreamOp.WriteAccountKey(stream,operationBlock.account_key);
  stream.Write(operationBlock.reward, Sizeof(operationBlock.reward));
  stream.Write(operationBlock.fee, Sizeof(operationBlock.fee));
  stream.Write(operationBlock.protocol_version, Sizeof(operationBlock.protocol_version));
  stream.Write(operationBlock.protocol_available, Sizeof(operationBlock.protocol_available));
  stream.Write(operationBlock.timestamp, Sizeof(operationBlock.timestamp));
  stream.Write(operationBlock.compact_target, Sizeof(operationBlock.compact_target));
  stream.Write(operationBlock.nonce, Sizeof(operationBlock.nonce));
  TStreamOp.WriteAnsiString(stream, operationBlock.block_payload);
  TStreamOp.WriteAnsiString(stream, operationBlock.initial_safe_box_hash);
  TStreamOp.WriteAnsiString(stream, operationBlock.operations_hash);
  TStreamOp.WriteAnsiString(stream, operationBlock.proof_of_work);
  if (operationBlock.protocol_version>=CT_PROTOCOL_5) then begin
    TStreamOp.WriteAnsiString(stream, operationBlock.previous_proof_of_work);
  end;
end;

class function TAccountComp.LoadTOperationBlockFromStream(const stream: TStream; var operationBlock: TOperationBlock): Boolean;
begin
  Result := False;
  operationBlock := CT_OperationBlock_NUL;
  If stream.Read(operationBlock.block, Sizeof(operationBlock.block))<Sizeof(operationBlock.block) then Exit;
  TStreamOp.ReadAccountKey(stream,operationBlock.account_key);
  stream.Read(operationBlock.reward, Sizeof(operationBlock.reward));
  stream.Read(operationBlock.fee, Sizeof(operationBlock.fee));
  stream.Read(operationBlock.protocol_version, Sizeof(operationBlock.protocol_version));
  stream.Read(operationBlock.protocol_available, Sizeof(operationBlock.protocol_available));
  stream.Read(operationBlock.timestamp, Sizeof(operationBlock.timestamp));
  stream.Read(operationBlock.compact_target, Sizeof(operationBlock.compact_target));
  stream.Read(operationBlock.nonce, Sizeof(operationBlock.nonce));
  if TStreamOp.ReadAnsiString(stream, operationBlock.block_payload) < 0 then Exit;
  if TStreamOp.ReadAnsiString(stream, operationBlock.initial_safe_box_hash) < 0 then Exit;
  if TStreamOp.ReadAnsiString(stream, operationBlock.operations_hash) < 0 then Exit;
  if TStreamOp.ReadAnsiString(stream, operationBlock.proof_of_work) < 0 then Exit;
  if operationBlock.protocol_version>=CT_PROTOCOL_5 then begin
    if TStreamOp.ReadAnsiString(stream, operationBlock.previous_proof_of_work) < 0 then Exit;
  end;
  Result := True;
end;

class function TAccountComp.AccountToTxt(const Account: TAccount): String;
begin
  Result := Format('%s Balance:%s N_Op:%d UpdB:%d UpdBA:%d Type:%d Name:%s PK:%s Data:%s Seal:%s',[AccountNumberToAccountTxtNumber(Account.account),
    FormatMoney(Account.balance),Account.n_operation,Account.updated_on_block_passive_mode,Account.updated_on_block_active_mode,Account.account_type,
      Account.name.ToPrintable,TCrypto.ToHexaString(TAccountComp.AccountInfo2RawString(Account.accountInfo)),
      Account.account_data.ToHexaString,Account.account_seal.ToHexaString ]);
end;

class function TAccountComp.AccountCanRecover(const Account: TAccount; currentBlockCount: Cardinal): Boolean;
begin
  Result := True;
  if TAccountComp.IsAccountBlockedByProtocol(Account.account, currentBlockCount) then begin
    Result := False; // 'account is blocked for protocol';
     Exit;
  end;
  if TAccountComp.IsAccountLocked(Account.accountInfo,currentBlockCount) then begin
    Result := False; // 'account is locked';
    Exit;
  end;
  // check boundary 1 gotten from TOpRecoverFounds.DoOperation
  if( Account.updated_on_block_active_mode + CT_RecoverFoundsWaitInactiveCount >= currentBlockCount ) then begin
    Result := False; // 'account is active';
    Exit;
  end;
  // check boundary 2 gotten from TOpRecoverFounds.DoOperation
  if( TAccountComp.AccountBlock(Account.account) + CT_RecoverFoundsWaitInactiveCount >= currentBlockCount ) then begin
    Result := False; // 'account block is active';
    Exit;
  end;
end;

class function TAccountComp.IsValidAccountInfo(const AAccountInfo: TAccountInfo; ACurrentProtocol : Word; var errors: String): Boolean;
Var s : String;
begin
  errors := '';
  case AAccountInfo.state of
    as_Unknown: begin
        errors := 'Account state is unknown';
        Result := false;
      end;
    as_Normal: begin
        Result := IsValidAccountKey(AAccountInfo.accountKey,ACurrentProtocol,errors);
      end;
    as_ForSale: begin
        Result := IsValidAccountKey(AAccountInfo.accountKey,ACurrentProtocol,errors);
        if (Result) And (IsAccountForPrivateSale(AAccountInfo)) then begin
          if Not IsValidAccountKey(AAccountInfo.new_publicKey,ACurrentProtocol,s) then begin
            Result := False;
            errors := 'Invalid new_publicKey: '+s;
          end;
        end;
      end;
    as_ForAtomicAccountSwap: begin
        Result := IsValidAccountKey(AAccountInfo.accountKey,ACurrentProtocol,errors);
        if (Result) And (Not IsValidAccountKey(AAccountInfo.new_publicKey,ACurrentProtocol,s)) then begin
          Result := False;
          errors := 'Invalid AccountSwap.new_publicKey: '+s;
        end;
      end;
    as_ForAtomicCoinSwap: begin
      Result := IsValidAccountKey(AAccountInfo.accountKey,ACurrentProtocol,errors);
    end
  else
    raise Exception.Create('DEVELOP ERROR 20170214-3');
  end;
end;

class function TAccountComp.IsValidAccountKey(const AAccountInfo: TAccountKey;  ACurrentProtocol : Word; var errors : String): Boolean;
begin
  errors := '';
  case AAccountInfo.EC_OpenSSL_NID of
    CT_NID_secp256k1,CT_NID_secp384r1,CT_NID_sect283k1,CT_NID_secp521r1 : begin
      Result := TECPrivateKey.IsValidPublicKey(AAccountInfo,ACurrentProtocol,errors);
    end;
  else
    errors := Format('Invalid AccountKey type:%d (Unknown type) - Length x:%d y:%d',[AAccountInfo.EC_OpenSSL_NID,length(AAccountInfo.x),length(AAccountInfo.y)]);
    Result := False;
  end;
end;

class function TAccountComp.IsValidEC_OpenSSL_NID(ANID: Word): Boolean;
begin
  Result := (ANID = CT_NID_secp256k1) or (ANID = CT_NID_secp384r1)
    or (ANID = CT_NID_sect283k1) or (ANID = CT_NID_secp521r1);
end;

class function TAccountComp.IsNullAccountKey(const AAccountInfo : TAccountKey) : Boolean;
begin
  Result := AAccountInfo.EC_OpenSSL_NID = CT_TECDSA_Public_Nul.EC_OpenSSL_NID;
end;

class function TAccountComp.IsValidNewAccountKey(const AAccountInfo : TAccountInfo; const ANewKey : TAccountKey; AProtocolVersion : Integer) : Boolean;
begin
  Result :=False;
  if AProtocolVersion < CT_PROTOCOL_5 then begin
    // V2 - V4 Rules
    // - Private Sale: non-null and must match stored new-key in account_info
    // - Public Sale: non-null
    // - Else: non-null  (used for change key)
    if IsAccountForPrivateSale(AAccountInfo) then
       Result := (NOT IsNullAccountKey(ANewKey)) AND TAccountComp.EqualAccountKeys(ANewKey, AAccountInfo.new_publicKey)
    else if IsAccountForPublicSale(AAccountInfo) then
       Result := NOT IsNullAccountKey(ANewKey)
    else
       Result := NOT IsNullAccountKey(ANewKey);
  end else begin
    // V5 New Key Rules:
    // - Private Sale, Atomic Account Swap: new key must match specified new-key in account_info
    // - Public Sale: non null
    // - Atomic Coin Swap: new key must equal existing account key, ignoring stored new-key if any
    // - else: non-null
    if TAccountComp.IsAccountForPrivateSale(AAccountInfo) then
      Result := TAccountComp.EqualAccountKeys(ANewKey, AAccountInfo.new_publicKey)
    else if TAccountComp.IsAccountForPublicSale(AAccountInfo) then
      Result := NOT IsNullAccountKey(ANewKey)
    else if TAccountComp.IsAccountForAccountSwap(AAccountInfo) then
      Result := TAccountComp.EqualAccountKeys(ANewKey, AAccountInfo.new_publicKey)
    else if TAccountComp.IsAccountForCoinSwap(AAccountInfo) then
      Result := TAccountComp.EqualAccountKeys(ANewKey, AAccountInfo.accountKey)
    else
      Result := NOT IsNullAccountKey(ANewKey);
  end;
end;

class function TAccountComp.PrivateToAccountkey(key: TECPrivateKey): TAccountKey;
begin
  Result := key.PublicKey;
end;

class function TAccountComp.RawString2AccountInfo(const rawaccstr: TRawBytes): TAccountInfo;
begin
  RawString2AccountInfo(rawaccstr,Result);
end;

class procedure TAccountComp.RawString2AccountInfo(const rawaccstr: TRawBytes; var dest: TAccountInfo);
Var ms : TMemoryStream;
  w : Word;
begin
  if Length(rawaccstr)=0 then begin
    dest := CT_AccountInfo_NUL;
    exit;
  end;
  ms := TMemoryStream.Create;
  Try
    ms.WriteBuffer(rawaccstr[Low(rawaccstr)],Length(rawaccstr));
    ms.Position := 0;
    If ms.Read(w,SizeOf(w))<>SizeOf(w) then exit;
    case w of
      CT_NID_secp256k1,CT_NID_secp384r1,CT_NID_sect283k1,CT_NID_secp521r1 : Begin
        dest.state := as_Normal;
        RawString2Accountkey(rawaccstr,dest.accountKey);
        dest.locked_until_block:=CT_AccountInfo_NUL.locked_until_block;
        dest.price:=CT_AccountInfo_NUL.price;
        dest.account_to_pay:=CT_AccountInfo_NUL.account_to_pay;
        dest.new_publicKey:=CT_AccountInfo_NUL.new_publicKey;
        dest.hashed_secret:=CT_AccountInfo_NUL.hashed_secret;
      End;
      CT_AccountInfo_ForSale, CT_AccountInfo_ForAccountSwap, CT_AccountInfo_ForCoinSwap : Begin
        TStreamOp.ReadAccountKey(ms,dest.accountKey);
        ms.Read(dest.locked_until_block,SizeOf(dest.locked_until_block));
        ms.Read(dest.price,SizeOf(dest.price));
        ms.Read(dest.account_to_pay,SizeOf(dest.account_to_pay));
        TStreamOp.ReadAccountKey(ms,dest.new_publicKey);
        case w of
          CT_AccountInfo_ForSale: dest.state := as_ForSale;
          CT_AccountInfo_ForAccountSwap: dest.state := as_ForAtomicAccountSwap;
          CT_AccountInfo_ForCoinSwap: dest.state := as_ForAtomicCoinSwap;
        end;
        if dest.state in [as_ForAtomicAccountSwap,as_ForAtomicCoinSwap] then begin
          TStreamOp.ReadAnsiString(ms,dest.hashed_secret);
        end else begin
          dest.hashed_secret:=CT_AccountInfo_NUL.hashed_secret;
        end;
      End;
    else
      raise Exception.Create('DEVELOP ERROR 20170214-2');
    end;
  Finally
    ms.Free;
  end;
end;

class function TAccountComp.RawString2Accountkey(const rawaccstr: TRawBytes): TAccountKey;
begin
  RawString2Accountkey(rawaccstr,Result);
end;

class procedure TAccountComp.RawString2Accountkey(const rawaccstr: TRawBytes; var dest: TAccountKey);
Var ms : TMemoryStream;
begin
  if length(rawaccstr)=0 then begin
    dest := CT_TECDSA_Public_Nul;
    exit;
  end;
  ms := TMemoryStream.Create;
  try
    ms.WriteBuffer(rawaccstr[Low(rawaccstr)],Length(rawaccstr));
    ms.Position := 0;
    TStreamOp.ReadAccountKey(ms,dest);
  finally
    ms.Free;
  end;
end;

{$IFNDEF VER210}
{$DEFINE DELPHIXE}
{$ENDIF}
// Deprecated
class function TAccountComp.TxtToMoney(const moneytxt: String;  var money: Int64): Boolean;
begin
  Result := TPASCEncoding.TryDecode(moneytxt, money);
end;

class procedure TAccountComp.ValidsEC_OpenSSL_NID(list: TList<Word>);
begin
  list.Clear;
  list.Add((CT_NID_secp256k1)); // = 714
  list.Add((CT_NID_secp384r1)); // = 715
  list.Add((CT_NID_sect283k1)); // = 729
  list.Add((CT_NID_secp521r1)); // = 716
end;

{ TProgressNotifyManyHelper }

procedure TProgressNotifyManyHelper.Add(listener : TProgressNotify);
begin
  if TArrayTool<TProgressNotify>.IndexOf(self, listener) = -1 then begin
    TArrayTool<TProgressNotify>.Add(self, listener);
  end;
end;

procedure TProgressNotifyManyHelper.Remove(listener : TProgressNotify);
begin
  TArrayTool<TProgressNotify>.Remove(self, listener);
end;

procedure TProgressNotifyManyHelper.Invoke(sender : TObject; const message : String; curPos, totalCount : Int64);
var i : Integer;
begin
  for i := low(self) to high(self) do
    self[i](sender, message, curPos, totalCount);
end;

{ TPCSafeBox }

{$IFDEF USE_ABSTRACTMEM}
  {$UNDEF uselowmem}
  {$UNDEF useAccountKeyStorage}
{$ELSE}
// New on version 2: To reduce mem usage
{$DEFINE uselowmem}
{$DEFINE useAccountKeyStorage}
{$ENDIF}

{$IFDEF uselowmem}
Type
  {$IFDEF useAccountKeyStorage}
  TAccountInfoKS = Record
    state : TAccountState;
    accountKeyKS: PAccountKey; // Change instead of TAccountKey
    locked_until_block : Cardinal;
    price : UInt64;
    account_to_pay : Cardinal;
    new_publicKeyKS : PAccountKey;
    hashed_secret: TRawBytes;
  end;
  {$ENDIF}

  { In order to store less memory on RAM, those types will be used
    to store in RAM memory (better than to use original ones)
    This will reduce 10-15% of memory usage.
    For future versions, will be a good solution to use those instead
    of originals, but}
  TMemAccount = Record // TAccount with less memory usage
    // account number is discarded (-4 bytes)
    {$IFDEF useAccountKeyStorage}
    accountInfoKS : TAccountInfoKS;
    {$ELSE}
    accountInfo : TDynRawBytes;
    {$ENDIF}
    balance: UInt64;
    updated_on_block_passive_mode: Cardinal;
    updated_on_block_active_mode: Cardinal;
    n_operation: Cardinal;
    name : TRawBytes;
    account_type : Word;
    account_data : TDynRawBytes;
    account_seal : T20Bytes;
  End;

  TMemOperationBlock = Record // TOperationBlock with less memory usage
    // block number is discarded (-4 bytes)
    {$IFDEF useAccountKeyStorage}
    account_keyKS: PAccountKey;
    {$ELSE}
    account_key: TDynRawBytes;
    {$ENDIF}
    reward: UInt64;
    fee: UInt64;
    protocol_version: Word;
    protocol_available: Word;
    timestamp: Cardinal;
    compact_target: Cardinal;
    nonce: Cardinal;
    block_payload : TDynRawBytes;
    initial_safe_box_hash: T32Bytes; // 32 direct bytes instead of use an AnsiString (-8 bytes)
    operations_hash: T32Bytes;       // 32 direct bytes instead of use an AnsiString (-8 bytes)
    proof_of_work: T32Bytes;         // 32 direct bytes instead of use an AnsiString (-8 bytes)
    previous_proof_of_work: T32Bytes;
  end;

  TMemBlockAccount = Record // TBlockAccount with less memory usage
    blockchainInfo : TMemOperationBlock;
    accounts : Array[0..CT_AccountsPerBlock-1] of TMemAccount;
    block_hash: T32Bytes;     // 32 direct bytes instead of use an AnsiString (-8 bytes)
    accumulatedWork : UInt64;
  end;


Type PBlockAccount = ^TMemBlockAccount;
{$ELSE}
Type
  {$IFDEF USE_ABSTRACTMEM}
  {$ELSE}
  PBlockAccount = ^TBlockAccount;
  {$ENDIF}
  TMemAccount = TAccount;
  TMemBlockAccount = TBlockAccount;
{$ENDIF}

procedure ToTMemAccount(Const source : TAccount; var dest : TMemAccount);
{$IFDEF uselowmem}
Var raw : TRawBytes;
{$ENDIF}
begin
  {$IFDEF uselowmem}
  {$IFDEF useAccountKeyStorage}
  dest.accountInfoKS.state:=source.accountInfo.state;
  dest.accountInfoKS.accountKeyKS:=TAccountKeyStorage.KS.AddAccountKey(source.accountInfo.accountKey);
  dest.accountInfoKS.locked_until_block:=source.accountInfo.locked_until_block;
  dest.accountInfoKS.price:=source.accountInfo.price;
  dest.accountInfoKS.account_to_pay:=source.accountInfo.account_to_pay;
  dest.accountInfoKS.new_publicKeyKS:=TAccountKeyStorage.KS.AddAccountKey(source.accountInfo.new_publicKey);
  dest.accountInfoKS.hashed_secret:=source.accountInfo.hashed_secret;
  {$ELSE}
  TAccountComp.AccountInfo2RawString(source.accountInfo,raw);
  TBaseType.To256RawBytes(raw,dest.accountInfo);
  {$ENDIF}
  dest.balance := source.balance;
  dest.updated_on_block_passive_mode:=source.updated_on_block_passive_mode;
  dest.updated_on_block_active_mode:=source.updated_on_block_active_mode;
  dest.n_operation:=source.n_operation;
  dest.name:=source.name;
  dest.account_type:=source.account_type;
  dest.account_data:=Copy(source.account_data);
  dest.account_seal:=TBaseType.To20Bytes(source.account_seal);
  {$ELSE}
  dest := source;
  {$ENDIF}
end;

procedure ToTAccount(const source : TMemAccount; account_number : Cardinal; var dest : TAccount);
{$IFDEF uselowmem}
var raw : TRawBytes;
{$ENDIF}
begin
  {$IFDEF uselowmem}
  dest.account:=account_number;
  {$IFDEF useAccountKeyStorage}
  dest.accountInfo.state:=source.accountInfoKS.state;
  dest.accountInfo.accountKey:=source.accountInfoKS.accountKeyKS^;
  dest.accountInfo.locked_until_block:=source.accountInfoKS.locked_until_block;
  dest.accountInfo.price:=source.accountInfoKS.price;
  dest.accountInfo.account_to_pay:=source.accountInfoKS.account_to_pay;
  dest.accountInfo.new_publicKey:=source.accountInfoKS.new_publicKeyKS^;
  dest.accountInfo.hashed_secret:=source.accountInfoKS.hashed_secret;
  {$ELSE}
  TBaseType.ToRawBytes(source.accountInfo,raw);
  TAccountComp.RawString2AccountInfo(raw,dest.accountInfo);
  {$ENDIF}
  dest.balance := source.balance;
  dest.updated_on_block_passive_mode:=source.updated_on_block_passive_mode;
  dest.updated_on_block_active_mode:=source.updated_on_block_active_mode;
  dest.n_operation:=source.n_operation;
  dest.name:=source.name;
  dest.account_type:=source.account_type;
  dest.account_data:=Copy(source.account_data);
  dest.account_seal:=TBaseType.T20BytesToRawBytes(source.account_seal);
  {$ELSE}
  dest := source;
  {$ENDIF}
end;

procedure ToTMemBlockAccount(const source : TBlockAccount; var dest : TMemBlockAccount);
{$IFDEF uselowmem}
var i : Integer;
var raw : TRawBytes;
{$ENDIF}
Begin
  {$IFDEF uselowmem}
  {$IFDEF useAccountKeyStorage}
  dest.blockchainInfo.account_keyKS:=TAccountKeyStorage.KS.AddAccountKey(source.blockchainInfo.account_key);
  {$ELSE}
  TAccountComp.AccountKey2RawString(source.blockchainInfo.account_key,raw);
  TBaseType.To256RawBytes(raw,dest.blockchainInfo.account_key);
  {$ENDIF}
  dest.blockchainInfo.reward:=source.blockchainInfo.reward;
  dest.blockchainInfo.fee:=source.blockchainInfo.fee;
  dest.blockchainInfo.protocol_version:=source.blockchainInfo.protocol_version;
  dest.blockchainInfo.protocol_available:=source.blockchainInfo.protocol_available;
  dest.blockchainInfo.timestamp:=source.blockchainInfo.timestamp;
  dest.blockchainInfo.compact_target:=source.blockchainInfo.compact_target;
  dest.blockchainInfo.nonce:=source.blockchainInfo.nonce;
  TBaseType.To256RawBytes(source.blockchainInfo.block_payload,dest.blockchainInfo.block_payload);
  TBaseType.To32Bytes(source.blockchainInfo.initial_safe_box_hash,dest.blockchainInfo.initial_safe_box_hash);
  TBaseType.To32Bytes(source.blockchainInfo.operations_hash,dest.blockchainInfo.operations_hash);
  TBaseType.To32Bytes(source.blockchainInfo.proof_of_work,dest.blockchainInfo.proof_of_work);
  TBaseType.To32Bytes(source.blockchainInfo.previous_proof_of_work,dest.blockchainInfo.previous_proof_of_work);

  for i := Low(source.accounts) to High(source.accounts) do begin
    ToTMemAccount(source.accounts[i],dest.accounts[i]);
  end;
  TBaseType.To32Bytes(source.block_hash,dest.block_hash);
  dest.accumulatedWork := source.accumulatedWork;
  {$ELSE}
  dest := source;
  {$ENDIF}
end;

procedure ToTBlockAccount(const source : TMemBlockAccount; block_number : Cardinal; var dest : TBlockAccount);
{$IFDEF uselowmem}
var i : Integer;
  raw : TRawBytes;
{$ENDIF}
begin
  {$IFDEF uselowmem}
  dest.blockchainInfo.block:=block_number;
  {$IFDEF useAccountKeyStorage}
  dest.blockchainInfo.account_key := source.blockchainInfo.account_keyKS^;
  {$ELSE}
  TBaseType.ToRawBytes(source.blockchainInfo.account_key,raw);
  TAccountComp.RawString2Accountkey(raw,dest.blockchainInfo.account_key);
  {$ENDIF}
  dest.blockchainInfo.reward:=source.blockchainInfo.reward;
  dest.blockchainInfo.fee:=source.blockchainInfo.fee;
  dest.blockchainInfo.protocol_version:=source.blockchainInfo.protocol_version;
  dest.blockchainInfo.protocol_available:=source.blockchainInfo.protocol_available;
  dest.blockchainInfo.timestamp:=source.blockchainInfo.timestamp;
  dest.blockchainInfo.compact_target:=source.blockchainInfo.compact_target;
  dest.blockchainInfo.nonce:=source.blockchainInfo.nonce;
  TBaseType.ToRawBytes(source.blockchainInfo.block_payload,dest.blockchainInfo.block_payload);
  TBaseType.ToRawBytes(source.blockchainInfo.initial_safe_box_hash,dest.blockchainInfo.initial_safe_box_hash);
  TBaseType.ToRawBytes(source.blockchainInfo.operations_hash,dest.blockchainInfo.operations_hash);
  TBaseType.ToRawBytes(source.blockchainInfo.proof_of_work,dest.blockchainInfo.proof_of_work);
  if (source.blockchainInfo.protocol_version>=CT_PROTOCOL_5) then begin
    TBaseType.ToRawBytes(source.blockchainInfo.previous_proof_of_work,dest.blockchainInfo.previous_proof_of_work);
  end else begin
    dest.blockchainInfo.previous_proof_of_work := Nil;
  end;

  for i := Low(source.accounts) to High(source.accounts) do begin
    ToTAccount(source.accounts[i],(block_number*CT_AccountsPerBlock)+i,dest.accounts[i]);
  end;
  TBaseType.ToRawBytes(source.block_hash,dest.block_hash);
  dest.accumulatedWork := source.accumulatedWork;
  {$ELSE}
  dest := source;
  {$ENDIF}
end;

Type
  TSafeboxSnapshot = Record
    nBlockNumber : Cardinal;
    oldBlocks : TOrderedBlockAccountList; // Saves old blocks values on modified blocks
    newBlocks : TOrderedBlockAccountList; // Saves final blocks values on modified blocks
    namesDeleted : TOrderedRawList;
    namesAdded : TOrderedRawList;
    oldBufferBlocksHash: {$IFDEF USE_BIGBLOCKS_MEM_ON_DISK}TPCTemporalFileStream{$ELSE}TBytesBuffer{$ENDIF};
    oldTotalBalance: Int64;
    oldTotalFee: Int64;
    oldSafeBoxHash : TRawBytes;
    oldWorkSum : UInt64;
    oldAggregatedHashrate : TBigNum;
    oldCurrentProtocol: Integer;
  end;
  PSafeboxSnapshot = ^TSafeboxSnapshot;

Const
  CT_TSafeboxSnapshot_NUL : TSafeboxSnapshot = (nBlockNumber : 0; oldBlocks : Nil; newBlocks : Nil; namesDeleted : Nil; namesAdded : Nil;oldBufferBlocksHash:Nil;oldTotalBalance:0;oldTotalFee:0;oldSafeBoxHash:Nil;oldWorkSum:0;oldAggregatedHashrate:Nil;oldCurrentProtocol:0);

function TPCSafeBox.Account(account_number: Cardinal): TAccount;
var
  iBlock : Integer;
  blockAccount : TBlockAccount;
begin
  StartThreadSafe;
  try
    iBlock:=(Integer(account_number)  DIV CT_AccountsPerBlock);
    If (Assigned(FPreviousSafeBox)) then begin
      SearchBlockWhenOnSeparatedChain(iBlock,blockAccount);
      Result := blockAccount.accounts[account_number MOD CT_AccountsPerBlock].GetCopy;
    end else begin
      {$IFDEF USE_ABSTRACTMEM}
      if (iBlock<0) Or (iBlock>=FPCAbstractMem.AccountsCount) then raise Exception.Create('Invalid account: '+IntToStr(account_number));
      Result := FPCAbstractMem.GetAccount(account_number).GetCopy;
      {$ELSE}
      if (iBlock<0) Or (iBlock>=FBlockAccountsList.Count) then raise Exception.Create('Invalid account: '+IntToStr(account_number));
      ToTAccount(PBlockAccount(FBlockAccountsList.Items[iBlock])^.accounts[account_number MOD CT_AccountsPerBlock],account_number,Result);
      {$ENDIF}
    end;
  finally
    EndThreadSave;
  end;
end;

function TPCSafeBox.AddNew(const blockChain: TOperationBlock): TBlockAccount;
{ PIP-0011 (dev reward) workflow: (** Only on V3 protocol **)
  - Account 0 is Master Account
  - Account 0 type field (2 bytes: 0..65535) will store a Value, this value is the "dev account"
  - The "dev account" can be any account between 0..65535, and can be changed at any time.
  - The 80% of the blockChain.reward + miner fees will be added on first mined account (like V1 and V2)
  - The miner will also receive ownership of first four accounts (Before, all accounts where for miner)
  - The "dev account" will receive the last created account ownership and the 20% of the blockChain.reward
  - Example:
    - Account(0).type = 12345    <-- dev account = 12345
    - blockChain.block = 234567  <-- New block height. Accounts generated from 1172835..1172839
    - blockChain.reward = 50 PASC
    - blockChain.fee = 0.9876 PASC
    - blockChain.account_key = Miner public key
    - New generated accounts:
      - [0] = 1172835 balance: 40.9876 owner: Miner
      - [1] = 1172836 balance: 0       owner: Miner
      - [2] = 1172837 balance: 0       owner: Miner
      - [3] = 1172838 balance: 0       owner: Miner
      - [4] = 1172839 balance: 10.0000 owner: Account 12345 owner, same owner than "dev account"
    - Safebox balance increase: 50 PASC
  }

var i, base_addr : Integer;
  {$IFDEF USE_ABSTRACTMEM}
  {$ELSE}
  Pblock : PBlockAccount;
  {$ENDIF}
  accs_miner, accs_dev : Array of cardinal;
  Psnapshot : PSafeboxSnapshot;
  //
  account_dev,
  account_0 : TAccount;
  LAccountKey: TAccountKey;
  //
  acc_0_miner_reward,acc_4_dev_reward : Int64;
  acc_4_for_dev : Boolean;
  LBlockHashRate : TBigNum;
begin
  Result := CT_BlockAccount_NUL;
  Result.blockchainInfo := blockChain;
  If blockChain.block<>BlocksCount then Raise Exception.Create(Format('ERROR DEV 20170427-2 blockchain.block:%d <> BlocksCount:%d',[blockChain.block,BlocksCount]));
  If blockChain.fee<>FTotalFee then Raise Exception.Create(Format('ERROR DEV 20170427-3 blockchain.fee:%d <> Safebox.TotalFee:%d',[blockChain.fee,FTotalFee]));

  TPascalCoinProtocol.GetRewardDistributionForNewBlock(blockChain,acc_0_miner_reward,acc_4_dev_reward,acc_4_for_dev);
  account_dev := CT_Account_NUL;
  If (acc_4_for_dev) then begin
    account_0 := Account(0); // Account 0 is master account, will store "dev account" in type field
    If (AccountsCount>account_0.account_type) then begin
      account_dev := Account(account_0.account_type);
    end else account_dev := account_0;
    if (account_dev.account=0) then begin
      LAccountKey := blockChain.account_key;
    end else begin
      LAccountKey := account_dev.accountInfo.accountKey;
    end;
  end;

  base_addr := BlocksCount * CT_AccountsPerBlock;
  setlength(accs_miner,0);
  setlength(accs_dev,0);
  for i := Low(Result.accounts) to High(Result.accounts) do begin
    Result.accounts[i] := CT_Account_NUL;
    Result.accounts[i].account := base_addr + i;
    Result.accounts[i].accountInfo.state := as_Normal;
    Result.accounts[i].updated_on_block_passive_mode := BlocksCount;
    Result.accounts[i].updated_on_block_active_mode := BlocksCount;
    Result.accounts[i].n_operation := 0;
    if (acc_4_for_dev) And (i=CT_AccountsPerBlock-1) then begin
      Result.accounts[i].accountInfo.accountKey := LAccountKey;
      SetLength(accs_dev,length(accs_dev)+1);
      accs_dev[High(accs_dev)] := base_addr + i;
      Result.accounts[i].balance := acc_4_dev_reward;
    end else begin
      Result.accounts[i].accountInfo.accountKey := blockChain.account_key;
      SetLength(accs_miner,length(accs_miner)+1);
      accs_miner[High(accs_miner)] := base_addr + i;
      if i=Low(Result.accounts) then begin
        // Only first account wins the reward + fee
        Result.accounts[i].balance := acc_0_miner_reward;
      end else begin
      end;
    end;
  end;
  Inc(FWorkSum,Result.blockchainInfo.compact_target);
  Result.AccumulatedWork := FWorkSum;
  // Add Aggregated Work based on HashRate
  LBlockHashRate := TBigNum.TargetToHashRate( blockChain.compact_target );
  Try
    AggregatedHashrate.Add( LBlockHashRate );
  finally
    LBlockHashRate.Free;
  end;
  // Calc block hash
  Result.block_hash := CalcBlockHash(Result,FCurrentProtocol);
  If Assigned(FPreviousSafeBox) then begin
    FModifiedBlocksSeparatedChain.Add(Result);
    BufferBlocksHash.Add(Result.block_hash[Low(Result.block_hash)],Length(Result.block_hash));
  end else begin
    {$IFDEF USE_ABSTRACTMEM}
    FPCAbstractMem.AddBlockAccount(Result);
    {$ELSE}
    New(Pblock);
    ToTMemBlockAccount(Result,Pblock^);
    FBlockAccountsList.Add(Pblock);
    BufferBlocksHash.Add(Result.block_hash[Low(Result.block_hash)],Length(Result.block_hash));
    {$ENDIF}
  end;
  Inc(FTotalBalance,Int64(blockChain.reward + blockChain.fee));
  Dec(FTotalFee, Int64(blockChain.fee));
  If (length(accs_miner)>0) then begin
    AccountKeyListAddAccounts(blockChain.account_key,accs_miner);
  end;
  If (length(accs_dev)>0) then begin
    AccountKeyListAddAccounts(LAccountKey,accs_dev);
  end;
  // Calculating new value of safebox
  FSafeBoxHash := CalcSafeBoxHash;

  // Save previous snapshot with current state
  If (FMaxSafeboxSnapshots>0) then begin
    new(Psnapshot);
    Psnapshot^:=CT_TSafeboxSnapshot_NUL;
    Psnapshot^.nBlockNumber:=blockChain.block;
    Psnapshot^.oldBlocks := FModifiedBlocksPreviousState;
    Psnapshot^.newBlocks := FModifiedBlocksFinalState;
    Psnapshot^.namesDeleted := FDeletedNamesSincePreviousSafebox;
    Psnapshot^.namesAdded := FAddedNamesSincePreviousSafebox;
    {$IFDEF USE_BIGBLOCKS_MEM_ON_DISK}
    Psnapshot^.oldBufferBlocksHash := TPCTemporalFileStream.Create('oldbufferblockhash');
    BufferBlocksHash.SaveToStream( Psnapshot^.oldBufferBlocksHash );
    {$ELSE}
    Psnapshot^.oldBufferBlocksHash := TBytesBuffer.CreateCopy(BufferBlocksHash);
    {$ENDIF}
    Psnapshot^.oldTotalBalance:=FTotalBalance;
    Psnapshot^.oldTotalFee:=FTotalFee;
    Psnapshot^.oldSafeBoxHash := FSafeBoxHash;
    Psnapshot^.oldWorkSum := FWorkSum;
    Psnapshot^.oldAggregatedHashrate := AggregatedHashrate.Copy;
    Psnapshot^.oldCurrentProtocol:= FCurrentProtocol;
    FSnapshots.Add(Psnapshot);
    FModifiedBlocksPreviousState := TOrderedBlockAccountList.Create;
    FModifiedBlocksFinalState := TOrderedBlockAccountList.Create;
    FAddedNamesSincePreviousSafebox := TOrderedRawList.Create;
    FDeletedNamesSincePreviousSafebox := TOrderedRawList.Create;
    // Remove old snapshots!
    If (FSubChains.Count=0) And (Not Assigned(FPreviousSafeBox)) then begin
      // Remove ONLY if there is no subchain based on my snapshots!
      While (FSnapshots.Count>FMaxSafeboxSnapshots) do begin
        Psnapshot := FSnapshots[0];
        {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,Classname,Format('Deleting snapshot for block %d',[Psnapshot^.nBlockNumber]));{$ENDIF}
        FSnapshots.Delete(0);
        FreeAndNil( Psnapshot^.oldBlocks );
        FreeAndNil( Psnapshot^.newBlocks );
        FreeAndNil( Psnapshot^.namesAdded );
        FreeAndNil( Psnapshot^.namesDeleted );
        FreeAndNil( Psnapshot^.oldBufferBlocksHash );
        FreeAndNil( Psnapshot^.oldAggregatedHashrate );
        Psnapshot^.oldSafeBoxHash := Nil;
        Dispose(Psnapshot);
      end;
    end;
  end else begin
    FModifiedBlocksPreviousState.Clear;
    FModifiedBlocksFinalState.Clear;
    FAddedNamesSincePreviousSafebox.Clear;
    FDeletedNamesSincePreviousSafebox.Clear;
  end;
end;

procedure TPCSafeBox.AccountKeyListAddAccounts(const AccountKey: TAccountKey; const accounts: array of Cardinal);
Var i : Integer;
begin
  {$IFnDEF USE_ABSTRACTMEM}
  If Assigned(FOrderedAccountKeysList) then begin
    FOrderedAccountKeysList.AddAccounts(AccountKey,accounts);
  end;
  for i := 0 to FListOfOrderedAccountKeysList.count-1 do begin
    TOrderedAccountKeysList( FListOfOrderedAccountKeysList[i] ).AddAccounts(AccountKey,accounts);
  end;
  {$ENDIF}
end;

procedure TPCSafeBox.AccountKeyListRemoveAccount(const AccountKey: TAccountKey; const accounts: array of Cardinal);
Var i : Integer;
begin
  {$IFnDEF USE_ABSTRACTMEM}
  If Assigned(FOrderedAccountKeysList) then begin
    FOrderedAccountKeysList.RemoveAccounts(AccountKey,accounts);
  end;
  for i := 0 to FListOfOrderedAccountKeysList.count-1 do begin
    TOrderedAccountKeysList( FListOfOrderedAccountKeysList[i] ).RemoveAccounts(AccountKey,accounts);
  end;
  {$ENDIF}
end;

function TPCSafeBox.AccountsCount: Integer;
begin
  StartThreadSafe;
  try
    Result := BlocksCount * CT_AccountsPerBlock;
  finally
    EndThreadSave;
  end;
end;

function TPCSafeBox.AccountsOrderedBySalePrice: TAccountsOrderedBySalePrice;
begin
  {$IFDEF USE_ABSTRACTMEM}
  Result := FPCAbstractMem.AccountsOrderedBySalePrice;
  {$ELSE}
  Result := FAccountsOrderedBySalePrice;
  {$ENDIF}
end;

function TPCSafeBox.AccountsOrderedByUpdatedBlock: TAccountsOrderedByUpdatedBlock;
begin
  {$IFDEF USE_ABSTRACTMEM}
  Result := FPCAbstractMem.AccountsOrderedByUpdatedBlock;
  {$ELSE}
  Result := FAccountsOrderedByUpdatedBlock;
  {$ENDIF}
end;

function TPCSafeBox.GetBlock(block_number: Cardinal): TBlockAccount;
begin
  StartThreadSafe;
  try
    If (Assigned(FPreviousSafeBox)) then begin
      if (block_number<0) Or (block_number>=BlocksCount) then raise Exception.Create('Invalid block number for chain: '+inttostr(block_number)+' max: '+IntToStr(BlocksCount-1));
      SearchBlockWhenOnSeparatedChain(block_number,Result);
    end else begin
      {$IFDEF USE_ABSTRACTMEM}
      if (block_number<0) Or (block_number>=FPCAbstractMem.BlocksCount) then raise Exception.Create('Invalid block number: '+inttostr(block_number)+' max: '+IntToStr(FPCAbstractMem.BlocksCount-1));
      Result := FPCAbstractMem.GetBlockAccount(block_number);
      {$ELSE}
      if (block_number<0) Or (block_number>=FBlockAccountsList.Count) then raise Exception.Create('Invalid block number: '+inttostr(block_number)+' max: '+IntToStr(FBlockAccountsList.Count-1));
      ToTBlockAccount(PBlockAccount(FBlockAccountsList.Items[block_number])^,block_number,Result);
      {$ENDIF}
    end;
  finally
    EndThreadSave;
  end;
end;

class function TPCSafeBox.BlockAccountToText(const block: TBlockAccount): String;
begin
  Result := Format('Block:%d Timestamp:%d BlockHash:%s',
    [block.blockchainInfo.block, block.blockchainInfo.timestamp,
       TCrypto.ToHexaString(block.block_hash)]);
end;

function TPCSafeBox.BlocksCount: Integer;
begin
  StartThreadSafe;
  try
    If Assigned(FPreviousSafeBox) then begin
      Result := FModifiedBlocksSeparatedChain.MaxBlockNumber+1;
      If (Result<=FPreviousSafeboxOriginBlock) then begin
        Result := FPreviousSafeboxOriginBlock+1;
      end;
    end else begin
      {$IFDEF USE_ABSTRACTMEM}
      Result := FPCAbstractMem.BlocksCount;
      {$ELSE}
      Result := FBlockAccountsList.Count;
      {$ENDIF}
    end;
  finally
    EndThreadSave;
  end;
end;

class function TPCSafeBox.CalcBlockHash(const block : TBlockAccount; current_protocol : Word): TRawBytes;
  // Protocol v2 update:
  // In order to store values to generate PoW and allow Safebox checkpointing, we
  // store info about TOperationBlock on each row and use it to obtain blockchash
Var raw: TRawBytes;
  ms : TMemoryStream;
  i : Integer;
begin
  ms := TMemoryStream.Create;
  try
    If (current_protocol<CT_PROTOCOL_2) then begin
      // PROTOCOL 1 BlockHash calculation
      ms.Write(block.blockchainInfo.block,4); // Little endian
      for i := Low(block.accounts) to High(block.accounts) do begin
        block.accounts[i].SerializeAccount(ms,current_protocol);
      end;
      ms.Write(block.blockchainInfo.timestamp,4); // Little endian
    end else begin
      // PROTOCOL 2 BlockHash calculation
      TAccountComp.SaveTOperationBlockToStream(ms,block.blockchainInfo);
      for i := Low(block.accounts) to High(block.accounts) do begin
        block.accounts[i].SerializeAccount(ms,current_protocol);
      end;
      ms.Write(block.AccumulatedWork,SizeOf(block.AccumulatedWork));
    end;
    Result := TCrypto.DoSha256(ms.Memory,ms.Size)
  finally
    ms.Free;
  end;
end;

function TPCSafeBox.CalcBlockHashRateInHs(block_number: Cardinal;
  Previous_blocks_average: Cardinal): TBigNum;
Var c,t : Cardinal;
  t_sum : Extended;
  bn : TBigNum;
  LOpBlock : TOperationBlock;
begin
  FLock.Acquire;
  Try
    Result := TBigNum.Create;
    try
      if (block_number=0) then begin
        Result.Value := 1;
        exit;
      end;
      if (block_number<0) Or (block_number>=BlocksCount) then raise Exception.Create('Invalid block number: '+inttostr(block_number));
      if (Previous_blocks_average<=0) then raise Exception.Create('Dev error 20161016-1');
      if (Previous_blocks_average>block_number) then Previous_blocks_average := block_number;
      //
      c := (block_number - Previous_blocks_average)+1;
      t_sum := 0;
      while (c<=block_number) do begin
        LOpBlock := GetBlockInfo(c);
        bn := TBigNum.TargetToHashRate(LOpBlock.compact_target);
        try
          Result.Add(bn);
        finally
          bn.Free;
        end;
        t_sum := t_sum + (LOpBlock.timestamp - GetBlockInfo(c-1).timestamp);
        inc(c);
      end;
      Result.Divide(Previous_blocks_average); // Obtain target average
      t_sum := t_sum / Previous_blocks_average; // time average
      t := Round(t_sum);
      if (t<>0) then begin
        Result.Divide(t);
      end;
    Except
      Result.Free;
      Raise;
    end;
  Finally
    FLock.Release;
  End;
end;

function TPCSafeBox.CalcBlockHashRateInKhs(block_number: Cardinal; Previous_blocks_average: Cardinal): Int64;
var bn : TBigNum;
begin
  bn := CalcBlockHashRateInHs(block_number,Previous_blocks_average);
  try
    Result := bn.Divide(1000).Value; // Value in Kh/s
  finally
    bn.Free;
  end;
end;

function TPCSafeBox.CalcSafeBoxHash: TRawBytes;
begin
  StartThreadSafe;
  try
    Result := TPascalCoinProtocol.CalcSafeBoxHash(BufferBlocksHash,CurrentProtocol);
  finally
    EndThreadSave;
  end;
end;

function TPCSafeBox.CanUpgradeToProtocol(newProtocolVersion : Word) : Boolean;
begin
  If (newProtocolVersion=CT_PROTOCOL_2) then begin
    Result := (FCurrentProtocol<CT_PROTOCOL_2) and (BlocksCount >= CT_Protocol_Upgrade_v2_MinBlock);
  end else if (newProtocolVersion=CT_PROTOCOL_3) then begin
    Result := (FCurrentProtocol=CT_PROTOCOL_2) And (BlocksCount >= CT_Protocol_Upgrade_v3_MinBlock);
  end else if (newProtocolVersion=CT_PROTOCOL_4) then begin
    Result := (FCurrentProtocol=CT_PROTOCOL_3) And (BlocksCount >= CT_Protocol_Upgrade_v4_MinBlock);
  end else if (newProtocolVersion=CT_PROTOCOL_5) then begin
    Result := (FCurrentProtocol=CT_PROTOCOL_4) And (BlocksCount >= CT_Protocol_Upgrade_v5_MinBlock);
  end else if (newProtocolVersion=CT_PROTOCOL_6) then begin
    Result := (FCurrentProtocol=CT_PROTOCOL_5) And (BlocksCount >= CT_Protocol_Upgrade_v6_MinBlock);
  end else Result := False;
end;

procedure TPCSafeBox.CheckMemory;
  { Note about Free Pascal compiler
    When compiling using Delphi it's memory manager more is efficient and does not increase, but
    When compiling using Free Pascal Compiler, is a good solution to "force" generate a new SafeBox
    in order to free memory not used. Tested with FPC 3.0 }
{$IFDEF FPC}
Var sb : TPCSafeBox;
  tc : TTickCount;
  auxSnapshotsList : TList<Pointer>;
  i : Integer;
{$ENDIF}
begin
  {$IFDEF FPC}
  StartThreadSafe;
  try
    If Assigned(FPreviousSafeBox) then Exit; // When loading from snapshot, does not allow to check memory!
    tc := TPlatform.GetTickCount;
    sb := TPCSafeBox.Create;
    try
      //
      auxSnapshotsList := TList<Pointer>.Create;
      Try
        // Save snapshots:
        for i:=0 to FSnapshots.Count-1 do begin
          auxSnapshotsList.Add(FSnapshots.Items[i]);
        end;
        FSnapshots.Clear;
        //
        sb.CopyFrom(Self);
        Self.Clear;
        Self.CopyFrom(sb);
        // Restore snapshots:
        for i:=0 to auxSnapshotsList.Count-1 do begin
          FSnapshots.Add(auxSnapshotsList.Items[i]);
        end;
        // Clear changes to do not fire key activity
        for i := 0 to FListOfOrderedAccountKeysList.count-1 do begin
          TOrderedAccountKeysList( FListOfOrderedAccountKeysList[i] ).ClearAccountKeyChanges;
        end;
      finally
        auxSnapshotsList.Free;
      end;
    finally
      sb.Free;
    end;
    TLog.NewLog(ltDebug,Classname,'Checked memory '+IntToStr(TPlatform.GetElapsedMilliseconds(tc))+' milliseconds');
  finally
    EndThreadSave;
  end;
  {$ENDIF}
end;

function TPCSafeBox.GetMinimumAvailableSnapshotBlock: Integer;
Var Pss : PSafeboxSnapshot;
begin
  Result := -1;
  StartThreadSafe;
  Try
    If (FSnapshots.Count>0) then begin
      Pss := FSnapshots[0];
      Result := Pss^.nBlockNumber;
    end;
  finally
    EndThreadSave;
  end;
end;

function TPCSafeBox.GetOrderedAccountKeysList: TSafeboxPubKeysAndAccounts;
begin
  {$IFDEF USE_ABSTRACTMEM}
  Result := FPCAbstractMem.AccountKeys;
  {$ELSE}
  Result := FOrderedAccountKeysList;
  {$ENDIF}
end;

function TPCSafeBox.HasSnapshotForBlock(block_number: Cardinal): Boolean;
Var Pss : PSafeboxSnapshot;
  i : Integer;
begin
  Result := False;
  StartThreadSafe;
  Try
    i := FSnapshots.Count-1;
    while (i>=0) And (PSafeboxSnapshot( FSnapshots[i] )^.nBlockNumber<>block_number) do dec(i);
    Result := (i>=0);
  finally
    EndThreadSave;
  end;
end;

procedure TPCSafeBox.Clear;
Var i : Integer;
  {$IFnDEF USE_ABSTRACTMEM}
  P : PBlockAccount;
  {$ENDIF}
  Psnapshot : PSafeboxSnapshot;
begin
  StartThreadSafe;
  Try
    {$IFDEF USE_ABSTRACTMEM}
    {$ELSE}
    If Assigned(FOrderedAccountKeysList) then begin
      FOrderedAccountKeysList.Clear;
    end;
    for i := 0 to FBlockAccountsList.Count - 1 do begin
      P := FBlockAccountsList.Items[i];
      Dispose(P);
    end;
    FBlockAccountsList.Clear;
    BufferBlocksHash.Clear;
    AggregatedHashrate.Value := 0;
    FOrderedByName.Clear;
    {$ENDIF}
    for i := 0 to FSnapshots.Count-1 do begin
      Psnapshot := (Fsnapshots[i]);
      FreeAndNil(Psnapshot^.oldBlocks);
      FreeAndNil(Psnapshot^.newBlocks);
      FreeAndNil(Psnapshot^.namesAdded);
      FreeAndNil(Psnapshot^.namesDeleted);
      FreeAndNil(Psnapshot^.oldBufferBlocksHash);
      FreeAndNil(Psnapshot^.oldAggregatedHashrate);
      Psnapshot^.oldSafeBoxHash := Nil;
      Dispose(Psnapshot);
    end;
    FSnapshots.Clear;

    For i:=0 to FListOfOrderedAccountKeysList.count-1 do begin
      TOrderedAccountKeysList( FListOfOrderedAccountKeysList[i] ).ClearAccounts(False);
    end;
    FTotalBalance := 0;
    FTotalFee := 0;
    FSafeBoxHash := CalcSafeBoxHash;
    FWorkSum := 0;
    FCurrentProtocol := CT_PROTOCOL_1;
    FModifiedBlocksSeparatedChain.Clear;
    FModifiedBlocksFinalState.Clear;
    FModifiedBlocksPreviousState.Clear;
    FAddedNamesSincePreviousSafebox.Clear;
    FDeletedNamesSincePreviousSafebox.Clear;
  Finally
    EndThreadSave;
  end;
end;

procedure TPCSafeBox.CopyFrom(accounts: TPCSafeBox);
Var i,j : Cardinal;
  {$IFnDEF USE_ABSTRACTMEM}
  P : PBlockAccount;
  lastOAKL : TOrderedAccountKeysList;
  {$ENDIF}
  BA : TBlockAccount;
begin
  StartThreadSafe;
  Try
    accounts.StartThreadSafe;
    try
      if accounts=Self then exit;
      If (Assigned(FPreviousSafeBox)) then begin
        Raise Exception.Create('Safebox is a separated chain. Cannot copy from other Safebox');
      end;
      If (Assigned(accounts.FPreviousSafeBox)) then begin
        Raise Exception.Create('Cannot copy from a separated chain Safebox');
      end;
      Clear;
      if accounts.BlocksCount>0 then begin
        {$IFDEF USE_ABSTRACTMEM}
        FPCAbstractMem.CopyFrom(accounts.FPCAbstractMem);
        {$ELSE}
        If Assigned(FOrderedAccountKeysList) And Assigned(accounts.FOrderedAccountKeysList) then begin
          lastOAKL := FOrderedAccountKeysList;
          FOrderedAccountKeysList:=Nil;
        end else lastOAKL := Nil;
        FBlockAccountsList.Capacity:=accounts.BlocksCount;
        for i := 0 to accounts.BlocksCount - 1 do begin
          BA := accounts.GetBlock(i);
          New(P);
          ToTMemBlockAccount(BA,P^);
          FBlockAccountsList.Add(P);
          for j := Low(BA.accounts) to High(BA.accounts) do begin
            If (Length(BA.accounts[j].name)>0) then FOrderedByName.Add(BA.accounts[j].name,BA.accounts[j].account);
            AccountKeyListAddAccounts(BA.accounts[j].accountInfo.accountKey,[BA.accounts[j].account]);
          end;
        end;
        BufferBlocksHash.CopyFrom(accounts.BufferBlocksHash);
        If Assigned(lastOAKL) then begin
          lastOAKL.CopyFrom(accounts.FOrderedAccountKeysList);
          FOrderedAccountKeysList:=lastOAKL;
        end;
        {$ENDIF}
      end;
      FTotalBalance := accounts.TotalBalance;
      FTotalFee := accounts.FTotalFee;
      FSafeBoxHash := Copy(accounts.FSafeBoxHash);
      FWorkSum := accounts.FWorkSum;
      AggregatedHashrate.RawValue := accounts.AggregatedHashrate.RawValue;
      FCurrentProtocol := accounts.FCurrentProtocol;
    finally
      accounts.EndThreadSave;
    end;
  finally
    EndThreadSave;
  end;
end;

{$IFDEF USE_ABSTRACTMEM}
procedure TPCSafeBox.SetSafeboxFileName(ASafeboxFileName : String);
var i : Integer;
  LOpBl : TOperationBlockExt;
begin
  FPCAbstractMem.Free;
  FPCAbstractMem := TPCAbstractMem.Create(ASafeboxFileName,False);
  if FPCAbstractMem.BlocksCount>0 then begin
    LOpBl := FPCAbstractMem.GetBlockInfo( FPCAbstractMem.BlocksCount-1 );
    FCurrentProtocol := LOpBl.operationBlock.protocol_version;
    FWorkSum := LOpBl.accumulatedWork;
  end else FCurrentProtocol := CT_PROTOCOL_1;
  FSafeBoxHash := CalcSafeBoxHash;
  FTotalBalance := TPascalCoinProtocol.CalcTotalBalance(FPCAbstractMem.BlocksCount);
end;

procedure TPCSafeBox.SaveCheckpointing(ACheckpointingSafeboxFileName : String);
begin
  FPCAbstractMem.SaveToFile(ACheckpointingSafeboxFileName);
end;

procedure TPCSafeBox.UpdateSafeboxFileName(const ANewSafeboxFileName : String);
begin
  FPCAbstractMem.UpdateSafeboxFileName(ANewSafeboxFileName);
end;

procedure TPCSafeBox.ClearSafeboxfile;
var LFileName : String;
begin
  LFileName := FPCAbstractMem.FileName;
  SetSafeboxFileName('');
  DeleteFile(LFileName);
  SetSafeboxFileName(LFileName);
end;

{$ENDIF}

constructor TPCSafeBox.Create;
begin
  FMaxSafeboxSnapshots:=CT_DEFAULT_MaxSafeboxSnapshots;
  FLock := TPCCriticalSection.Create('TPCSafeBox_Lock');
  {$IFDEF USE_ABSTRACTMEM}
  FPCAbstractMem := TPCAbstractMem.Create('',False);
  {$ELSE}
  FBlockAccountsList := TList<Pointer>.Create;
  FAggregatedHashrate := TBigNum.Create(0);
  FOrderedByName := TOrderedRawList.Create;
  FAccountsOrderedByUpdatedBlock := TAccountsOrderedByUpdatedBlock.Create(GetAccount);
  FAccountsOrderedBySalePrice := TAccountsOrderedBySalePrice.Create(GetAccount);
  {$ENDIF}
  FListOfOrderedAccountKeysList := TList<TOrderedAccountKeysList>.Create;
  FCurrentProtocol := CT_PROTOCOL_1;
  FSnapshots := TList<Pointer>.Create;
  FPreviousSafeBox := Nil;
  FPreviousSafeboxOriginBlock := -1;
  FModifiedBlocksSeparatedChain := TOrderedBlockAccountList.Create;
  FModifiedBlocksPreviousState := TOrderedBlockAccountList.Create;
  FModifiedBlocksFinalState := TOrderedBlockAccountList.Create;
  FAddedNamesSincePreviousSafebox := TOrderedRawList.Create;
  FDeletedNamesSincePreviousSafebox := TOrderedRawList.Create;
  FSubChains := TList<TPCSafeBox>.Create;
  {$IFDEF USE_ABSTRACTMEM}
  {$ELSE}
  FBufferBlocksHash := TBytesBuffer32Safebox.Create(1000*32);
  FOrderedAccountKeysList := TOrderedAccountKeysList.Create(Nil,True);
  {$ENDIF}
  BufferBlocksHash.SafeBoxHashCalcType := sbh_Single_Sha256;
  Clear;
end;

destructor TPCSafeBox.Destroy;
Var i : Integer;
begin
  Clear;
  for i := 0 to FListOfOrderedAccountKeysList.Count - 1 do begin
    TOrderedAccountKeysList( FListOfOrderedAccountKeysList[i] ).FAccountList := Nil;
  end;
  {$IFDEF USE_ABSTRACTMEM}
  FreeAndNil(FPCAbstractMem);
  {$ELSE}
  FreeAndNil(FBlockAccountsList);
  FreeAndNil(FOrderedByName);
  {$ENDIF}
  FreeAndNil(FListOfOrderedAccountKeysList);
  FreeAndNil(FLock);
  FreeAndNil(FSnapshots);
  FreeAndNil(FModifiedBlocksSeparatedChain);
  FreeAndNil(FModifiedBlocksPreviousState);
  FreeAndNil(FModifiedBlocksFinalState);
  FreeAndNil(FAddedNamesSincePreviousSafebox);
  FreeAndNil(FDeletedNamesSincePreviousSafebox);
  FreeAndNil(FSubChains);
  {$IFnDEF USE_ABSTRACTMEM}
  FreeAndNil(FAccountsOrderedByUpdatedBlock);
  FreeAndNil(FAccountsOrderedBySalePrice);
  {$ENDIF}

  If Assigned(FPreviousSafeBox) then begin
    FPreviousSafeBox.FSubChains.Remove(Self); // Remove from current snapshot
    FPreviousSafeBox := Nil;
    FPreviousSafeboxOriginBlock:=-1;
  end;
  {$IFDEF USE_ABSTRACTMEM}
  {$ELSE}
  FreeAndNil(FOrderedAccountKeysList);
  FreeAndNil(FBufferBlocksHash);
  FreeAndNil(FAggregatedHashrate);
  {$ENDIF}
  inherited;
end;

procedure TPCSafeBox.SetToPrevious(APreviousSafeBox: TPCSafeBox; StartBlock: Cardinal);
Var i : Integer;
  Psnapshot : PSafeboxSnapshot;
begin
  StartThreadSafe;
  Try
    Clear;
    {$IFDEF USE_ABSTRACTMEM}
    FPCAbstractMem.EraseData;
    {$ENDIF}
    If Assigned(FPreviousSafeBox) then begin
      FPreviousSafeBox.FSubChains.Remove(Self); // Remove from current snapshot
      FPreviousSafeBox := Nil;
      FPreviousSafeboxOriginBlock:=-1;
    end;
    If Assigned(APreviousSafeBox) then begin
      APreviousSafeBox.StartThreadSafe;
      Try
        If (APreviousSafeBox = Self) then Raise Exception.Create('Invalid previous');
        If Assigned(APreviousSafebox.FPreviousSafeBox) then Raise Exception.Create('Previous safebox is based on a snapshot too'); // Limitation
        i := APreviousSafeBox.FSnapshots.Count-1;
        while (i>=0) And (PSafeboxSnapshot( APreviousSafeBox.FSnapshots[i] )^.nBlockNumber<>StartBlock) do dec(i);
        if (i<0) then begin
          Raise Exception.Create('Previous Safebox does not contain snapshot of block '+IntToStr(StartBlock));
        end;
        Psnapshot:=PSafeboxSnapshot( APreviousSafeBox.FSnapshots[i] );
        FPreviousSafeBox := APreviousSafeBox;
        FPreviousSafeboxOriginBlock:=StartBlock;
        //
        FPreviousSafeBox.FSubChains.Add(Self);
        //
        {$IFDEF USE_BIGBLOCKS_MEM_ON_DISK}
        BufferBlocksHash.Clear;
        BufferBlocksHash.LoadFromStream( Psnapshot^.oldBufferBlocksHash );
        {$ELSE}
        BufferBlocksHash.CopyFrom( Psnapshot^.oldBufferBlocksHash );
        {$ENDIF}

        FTotalBalance := Psnapshot^.oldTotalBalance;
        FTotalFee := Psnapshot^.oldTotalFee;
        FSafeBoxHash := Psnapshot^.oldSafeBoxHash;
        FWorkSum := Psnapshot^.oldWorkSum;
        AggregatedHashrate.RawValue := Psnapshot^.oldAggregatedHashrate.RawValue;
        FCurrentProtocol := Psnapshot^.oldCurrentProtocol;
      finally
        APreviousSafeBox.EndThreadSave;
      end;
    end else begin

    end;
  finally
    EndThreadSave;
  end;
end;

procedure TPCSafeBox.CommitToPrevious;
  procedure RedoModifiedBlocks(const modifiedblocks : TOrderedBlockAccountList);
  Var iBlock,j : Integer;
    blockAccount : TBlockAccount;
  begin
    // modifiedBlocks is sorted in ASCENDING order, will create a new block on FPreviousSafeBox when needed
    For iBlock := 0 to modifiedBlocks.Count-1 do begin
      blockAccount := modifiedBlocks.Get(iBlock);
      // Set each account to previous value:
      for j:=Low(blockAccount.accounts) to High(blockAccount.accounts) do begin
        FPreviousSafeBox.UpdateAccount(blockAccount.accounts[j].account,
          blockAccount.accounts[j].accountInfo,
          blockAccount.accounts[j].name,
          blockAccount.accounts[j].account_type,
          blockAccount.accounts[j].balance,
          blockAccount.accounts[j].n_operation,
          blockAccount.accounts[j].account_data,
          blockAccount.accounts[j].account_seal,
          aus_commiting_from_otherchain,
          blockAccount.accounts[j].updated_on_block_passive_mode,
          blockAccount.accounts[j].updated_on_block_active_mode,
          False, // Not used when aus_commiting_from_otherchain
          False  // Not used when aus_commiting_from_otherchain
          );
      end;
    end;
  end;
  Procedure RedoAddedDeletedNames(AddedNamesList,DeletedNamesList : TOrderedRawList);
  Var i : Integer;
  Begin
    // Start deleting:
    For i:=0 to DeletedNamesList.Count-1 do begin
      {$IFDEF USE_ABSTRACTMEM}
      FPreviousSafebox.FPCAbstractMem.AccountsNames.DeleteAccountName(DeletedNamesList.Get(i).ToString);
      {$ELSE}
      FPreviousSafebox.FOrderedByName.Remove(DeletedNamesList.Get(i));
      {$ENDIF}
    end;
    // Finally adding
    For i:=0 to AddedNamesList.Count-1 do begin
      {$IFDEF USE_ABSTRACTMEM}
      FPreviousSafebox.FPCAbstractMem.AccountsNames.AddNameAndNumber(AddedNamesList.Get(i).ToString,AddedNamesList.GetTag(i));
      {$ELSE}
      FPreviousSafebox.FOrderedByName.Add(AddedNamesList.Get(i),AddedNamesList.GetTag(i));
      {$ENDIF}
    end;
    FPreviousSafebox.FAddedNamesSincePreviousSafebox.CopyFrom(AddedNamesList);
    FPreviousSafebox.FDeletedNamesSincePreviousSafebox.CopyFrom(DeletedNamesList);
  end;
  procedure RedoSnapshot(Psnapshot : PSafeboxSnapshot);
  Begin
    RedoModifiedBlocks(Psnapshot^.newBlocks);
    //
    RedoAddedDeletedNames(Psnapshot^.namesAdded,Psnapshot^.namesDeleted);
    //
    FPreviousSafeBox.AddNew(GetBlockInfo(Psnapshot^.nBlockNumber));
  end;

Var errors : String;
  i : Integer;
  Pss : PSafeboxSnapshot;
begin
  If Not Assigned(FPreviousSafeBox) then Raise Exception.Create('Previous not assigned');
  StartThreadSafe;
  Try
    FPreviousSafeBox.StartThreadSafe;
    Try
      { Process is:
        - Set Previous to snapshot state
        - for each snapshot:
           - update modified blocks
           - update Names lists with changes on snapshot
           - create a new block stored in snapshot
      }
      TLog.NewLog(ltdebug,ClassName,Format('Start CommitToPrevious - Rolling back from block %d to %d',[FPreviousSafeBox.BlocksCount-1,FPreviousSafeboxOriginBlock]));
      FPreviousSafeBox.RollBackToSnapshot(FPreviousSafeboxOriginBlock);
      {$IFDEF Check_Safebox_Names_Consistency}
      If Not Check_Safebox_Names_Consistency(FPreviousSafeBox,'PREVIOUS WITH ROLLBACK',errors) then begin
        TLog.NewLog(ltdebug,ClassName,'Check_Safebox_Names_Consistency '+errors);
      end;
      {$ENDIF}
      TLog.NewLog(ltdebug,ClassName,Format('Executing %d chain snapshots to master',[FSnapshots.Count]));
      For i:=0 to  FSnapshots.Count-1 do begin
        Pss := FSnapshots[i];
        TLog.NewLog(ltdebug,ClassName,Format('Executing %d/%d chain snapshot to master with %d blocks changed at block %d',[i+1,FSnapshots.Count,Pss^.newBlocks.Count,Pss^.nBlockNumber]));
        Try
          RedoSnapshot(Pss);
        Except
          On E:Exception do begin
            E.Message:= E.Message + Format(' Executing %d/%d chain snapshot to master with %d blocks changed at block %d',[i+1,FSnapshots.Count,Pss^.newBlocks.Count,Pss^.nBlockNumber]);
            Raise;
          end;
        end;
      end;
      // Finally add current changes?
      TLog.NewLog(ltdebug,ClassName,Format('Executing %d current chain changes to master',[FModifiedBlocksFinalState.Count]));
      RedoModifiedBlocks(FModifiedBlocksFinalState);
      RedoAddedDeletedNames(FAddedNamesSincePreviousSafebox,FDeletedNamesSincePreviousSafebox);

      TLog.NewLog(ltdebug,ClassName,Format('Check process start',[]));
      // Check it !!!!
      errors := '';
      If (FPreviousSafeBox.BlocksCount<>BlocksCount) then begin
        errors := errors+'> Invalid Blockscount!';
      end;
      If TBaseType.BinStrComp(FPreviousSafeBox.FSafeBoxHash,FSafeBoxHash)<>0 then begin
        errors := errors+'> Invalid SafeBoxHash!';
      end;
      if FPreviousSafeBox.BufferBlocksHash.Compare(BufferBlocksHash)<>0 then begin
        errors := errors+'> Invalid BufferBlocksHash!';
      end;
      If (FPreviousSafeBox.FTotalBalance<>FTotalBalance) then begin
        errors := errors+'> Invalid Balance!';
      end;
      If (FPreviousSafeBox.FTotalFee<>FTotalFee) then begin
        errors := errors+'> Invalid Fee!';
      end;
      If (FPreviousSafeBox.WorkSum<>FWorkSum) then begin
        errors := errors+'> Invalid WorkSum!';
      end;
      If (FPreviousSafeBox.AggregatedHashrate.CompareTo(AggregatedHashrate)<>0) then begin
        errors := errors+'> Invalid Aggregated Hashrate!';
      end;
      If (FPreviousSafeBox.FCurrentProtocol<>FCurrentProtocol) then begin
        errors := errors+'> Invalid Protocol!';
      end;
      If (errors<>'') Then begin
        Raise Exception.Create('Errors Commiting to previous! '+errors);
      end;
      {$IFDEF Check_Safebox_Names_Consistency}
      If Not Check_Safebox_Names_Consistency(FPreviousSafeBox,'PREVIOUS',errors) then begin
        if errors='' then Raise Exception.Create('Check_Safebox_Names_Consistency '+errors);
      end;
      if Not Check_Safebox_Names_Consistency(Self,'CHAIN',errors) then begin
        if errors='' then Raise Exception.Create('Check_Safebox_Names_Consistency '+errors);
      end;
      Check_Safebox_Integrity(FPreviousSafeBox,'INTEGRITY PREVIOUS');
      Check_Safebox_Integrity(Self,'INTEGRITY CHAIN');
      {$ENDIF}
      TLog.NewLog(ltdebug,ClassName,Format('Check process end',[]));
    finally
      FPreviousSafeBox.EndThreadSave;
    end;
  finally
    EndThreadSave;
  end;
end;

procedure TPCSafeBox.RollBackToSnapshot(snapshotBlock: Cardinal);
   procedure UndoModifiedBlocks(modifiedblocks : TOrderedBlockAccountList);
   Var iBlock,j : Integer;
     blockAccount : TBlockAccount;
   begin
     For iBlock := 0 to modifiedBlocks.Count-1 do begin
       blockAccount := modifiedBlocks.Get(iBlock);
       // Set each account to previous value:
       for j:=Low(blockAccount.accounts) to High(blockAccount.accounts) do begin
         UpdateAccount(blockAccount.accounts[j].account,
           blockAccount.accounts[j].accountInfo,
           blockAccount.accounts[j].name,
           blockAccount.accounts[j].account_type,
           blockAccount.accounts[j].balance,
           blockAccount.accounts[j].n_operation,
           blockAccount.accounts[j].account_data,
           blockAccount.accounts[j].account_seal,
           aus_rollback,
           blockAccount.accounts[j].updated_on_block_passive_mode,
           blockAccount.accounts[j].updated_on_block_active_mode,
           False,  // Not used when aus_rollback
           False); // Not used when aus_rollback
       end;
     end;
   end;

   Procedure UndoAddedDeletedNames(AddedNamesList,DeletedNamesList : TOrderedRawList);
   Var i,j : Integer;
     {$IFDEF USE_ABSTRACTMEM}
     Laninfo : TAccountNameInfo;
     {$ELSE}
     {$ENDIF}
   Begin
     // Start adding
     For i:=0 to AddedNamesList.Count-1 do begin
       // It was added, so we MUST FIND on current names list
       {$IFDEF USE_ABSTRACTMEM}
       If Not FPCAbstractMem.AccountsNames.FindByName(AddedNamesList.Get(i).ToString,Laninfo) then begin
         // ERROR: It has been added, why we not found???
         If DeletedNamesList.Find(AddedNamesList.Get(i),j) then begin
         end else begin
           TLog.NewLog(lterror,ClassName,Format('ERROR DEV 20180319-1 Name %s not found at account:%d',[AddedNamesList.Get(i).ToPrintable,AddedNamesList.GetTag(i)]));
         end;
       end else begin
         FPCAbstractMem.AccountsNames.DeleteData(Laninfo);
       end;
       {$ELSE}
       If Not FOrderedByName.Find(AddedNamesList.Get(i),j) then begin
         // ERROR: It has been added, why we not found???
         If DeletedNamesList.Find(AddedNamesList.Get(i),j) then begin
         end else begin
           TLog.NewLog(lterror,ClassName,Format('ERROR DEV 20180319-1 Name %s not found at account:%d',[AddedNamesList.Get(i).ToPrintable,AddedNamesList.GetTag(i)]));
         end;
       end else FOrderedByName.Delete(j);
       {$ENDIF}
     end;
     // Finally deleting
     For i:=0 to DeletedNamesList.Count-1 do begin
       {$IFDEF USE_ABSTRACTMEM}
       // It has been deleted, we MUST NOT FIND on current names list
       If FPCAbstractMem.AccountsNames.FindByName(DeletedNamesList.Get(i).ToString,Laninfo) then begin
         if Laninfo.accountNumber<>DeletedNamesList.GetTag(i) then begin
           // ERROR: It has been deleted, why is found with another account???
           TLog.NewLog(lterror,ClassName,Format('ERROR DEV 20180319-2 Name %s found at account:%d <> saved account:%d',[DeletedNamesList.Get(i).ToPrintable,DeletedNamesList.GetTag(i),Laninfo.accountNumber]));
         end;
       end;
       // Add with Info of previous account with name (saved at Tag value)
       FPCAbstractMem.AccountsNames.AddNameAndNumber(DeletedNamesList.Get(i).ToString,DeletedNamesList.GetTag(i));
       {$ELSE}
       // It has been deleted, we MUST NOT FIND on current names list
       If FOrderedByName.Find(DeletedNamesList.Get(i),j) then begin
         // It has been deleted... now is found
         If (FOrderedByName.GetTag(j)<>DeletedNamesList.GetTag(i)) then begin
           // ERROR: It has been deleted, why is found with another account???
           TLog.NewLog(lterror,ClassName,Format('ERROR DEV 20180319-2 Name %s found at account:%d <> saved account:%d',[DeletedNamesList.Get(i).ToPrintable,DeletedNamesList.GetTag(i),FOrderedByName.GetTag(j)]));
         end;
       end;
       // Add with Info of previous account with name (saved at Tag value)
       FOrderedByName.Add(DeletedNamesList.Get(i),DeletedNamesList.GetTag(i));
       {$ENDIF}
     end;
   end;

Var i,iPrevSnapshotTarget : Integer;
  Psnapshot : PSafeboxSnapshot;
  {$IFnDEF USE_ABSTRACTMEM}
  PBlock : PBlockAccount;
  {$ENDIF}
  {$IFDEF Check_Safebox_Names_Consistency}
  errors : String;
  {$ENDIF}
begin
  StartThreadSafe;
  Try
    { Process is:
      - Find Previous snapshot (target)
      - Undo current pending operations
      - For each snapshot do
        - Undo snapshot operations
        - Undo created BlockAccount
      - At target snapshot:
        - Restore values
      - Clear data
      - Delete "future" snapshots
    }
    iPrevSnapshotTarget := FSnapshots.Count-1;
    while (iPrevSnapshotTarget>=0) And (PSafeboxSnapshot(FSnapshots[iPrevSnapshotTarget])^.nBlockNumber<>snapshotBlock) do Dec(iPrevSnapshotTarget);
    If (iPrevSnapshotTarget<0) then Raise Exception.Create('Cannot Rollback to previous Snapshot block: '+IntToStr(snapshotBlock)+' current '+IntToStr(BlocksCount));
    // Go back starting from current state
    UndoModifiedBlocks(FModifiedBlocksPreviousState);
    UndoAddedDeletedNames(FAddedNamesSincePreviousSafebox,FDeletedNamesSincePreviousSafebox);
    // Go back based on snapshots: EXCEPT target
    For i:=FSnapshots.Count-1 downto (iPrevSnapshotTarget+1) do begin
      Psnapshot := FSnapshots[i];
      TLog.NewLog(ltdebug,ClassName,Format('Executing %d/%d rollback %d blocks changed at block %d',[i+1,FSnapshots.Count,Psnapshot^.oldBlocks.Count,Psnapshot^.nBlockNumber]));

      // Must UNDO changes:
      UndoModifiedBlocks(Psnapshot^.oldBlocks);
      if Psnapshot=Nil then Exit;

      UndoAddedDeletedNames(Psnapshot^.namesAdded,Psnapshot^.namesDeleted);

      // Undo Created BlockAccount
      // Undo ONLY of if not target
      {$IFDEF USE_ABSTRACTMEM}
      FPCAbstractMem.DeleteBlockAccount( FPCAbstractMem.BlocksCount - 1);
      {$ELSE}
      PBlock:=FBlockAccountsList.Items[FBlockAccountsList.Count-1];
      FBlockAccountsList.Delete(FBlockAccountsList.Count-1);
      Dispose(PBlock);
      // Redo FBufferBlocksHash
      BufferBlocksHash.SetLength(BufferBlocksHash.Length - 32);
      {$ENDIF}

      // Delete
      FSnapshots.Delete(i);
      Psnapshot^.oldBlocks.Free;
      Psnapshot^.newBlocks.Free;
      Psnapshot^.namesAdded.Free;
      Psnapshot^.namesDeleted.Free;
      Psnapshot^.oldBufferBlocksHash.Free;
      Psnapshot^.oldAggregatedHashrate.Free;
      Psnapshot^.oldSafeBoxHash := Nil;
      Dispose(Psnapshot);
    end;
    // Set saved Safebox values:
    Psnapshot := FSnapshots[iPrevSnapshotTarget];

    if BufferBlocksHash.Compare(Psnapshot^.oldBufferBlocksHash)<>0 then begin
      raise Exception.Create('ERROR DEV 20180322-1 Rollback invalid BufferBlocksHash value');
    end;

    FTotalBalance := Psnapshot^.oldTotalBalance;
    FTotalFee := Psnapshot^.oldTotalFee;
    FSafeBoxHash := Psnapshot^.oldSafeBoxHash;
    FWorkSum := Psnapshot^.oldWorkSum;
    AggregatedHashrate.RawValue := Psnapshot^.oldAggregatedHashrate.RawValue;
    FCurrentProtocol := Psnapshot^.oldCurrentProtocol;
    // Clear data
    FAddedNamesSincePreviousSafebox.Clear;
    FDeletedNamesSincePreviousSafebox.Clear;
    FModifiedBlocksPreviousState.Clear;
    FModifiedBlocksFinalState.Clear;
    {$IFDEF Check_Safebox_Names_Consistency}
    if Not Check_Safebox_Names_Consistency(Self,'ROLLBACK',errors) then begin
      if errors='' then Raise Exception.Create('Check_Safebox_Names_Consistency '+errors);
    end;
    {$ENDIF}
  finally
    EndThreadSave;
  end;
end;

function TPCSafeBox.DoUpgradeToProtocol2: Boolean;
var block_number : Cardinal;
  aux : TRawBytes;
  LBlockAcc : TBlockAccount;
begin
  // Upgrade process to protocol 2
  Result := false;
  If Not CanUpgradeToProtocol(CT_PROTOCOL_2) then exit;
  // Recalc all BlockAccounts block_hash value
  aux := CalcSafeBoxHash;
  TLog.NewLog(ltInfo,ClassName,'Start Upgrade to protocol 2 - Old Safeboxhash:'+TCrypto.ToHexaString(FSafeBoxHash)+' calculated: '+TCrypto.ToHexaString(aux)+' Blocks: '+IntToStr(BlocksCount));
  for block_number := 0 to BlocksCount - 1 do begin
    {$IFDEF USE_ABSTRACTMEM}
    LBlockAcc := FPCAbstractMem.GetBlockAccount(block_number);
    LBlockAcc.block_hash := CalcBlockHash( LBlockAcc, CT_PROTOCOL_2);
    BufferBlocksHash.Replace(block_number * 32, LBlockAcc.block_hash);
    {$ELSE}
    ToTBlockAccount(PBlockAccount(FBlockAccountsList.Items[block_number])^,block_number,LBlockAcc);
    LBlockAcc.block_hash := CalcBlockHash( LBlockAcc, CT_PROTOCOL_2);
    {$IFDEF uselowmem}
    TBaseType.To32Bytes(LBlockAcc.block_hash,PBlockAccount(FBlockAccountsList.Items[block_number])^.block_hash);
    {$ENDIF}
    BufferBlocksHash.Replace(block_number * 32, LBlockAcc.block_hash);
    {$ENDIF}
  end;
  FSafeBoxHash := CalcSafeBoxHash;
  FCurrentProtocol := CT_PROTOCOL_2;
  Result := True;
  TLog.NewLog(ltInfo,ClassName,'End Upgraded to protocol 2 - New safeboxhash:'+TCrypto.ToHexaString(FSafeBoxHash));
end;

function TPCSafeBox.DoUpgradeToProtocol3: Boolean;
begin
  FCurrentProtocol := CT_PROTOCOL_3;
  Result := True;
  TLog.NewLog(ltInfo,ClassName,'End Upgraded to protocol 3 - New safeboxhash:'+TCrypto.ToHexaString(FSafeBoxHash));
end;

function TPCSafeBox.DoUpgradeToProtocol4: Boolean;
begin
  FCurrentProtocol := CT_PROTOCOL_4;
  Result := True;
  TLog.NewLog(ltInfo,ClassName,'End Upgraded to protocol 4 - New safeboxhash:'+TCrypto.ToHexaString(FSafeBoxHash));
end;

function TPCSafeBox.DoUpgradeToProtocol5: Boolean;
var LAux : TRawBytes;
  LBlockNumber : Cardinal;
  i : Integer;
  {$IFDEF USE_ABSTRACTMEM}
  LBlockAccount : TBlockAccount;
  {$ELSE}
  LPtrBlockAccount : PBlockAccount;
  {$ENDIF}
begin
  Result := True;
  // V5 adds a new "CalcSafeBoxHash" method, so need to recalc
  // Recalc all BlockAccounts block_hash value caused by new fields added on Account at Protocol 5
  LAux := CalcSafeBoxHash;
  TLog.NewLog(ltInfo,ClassName,'Start Upgrade to protocol 5 - Old Safeboxhash:'+TCrypto.ToHexaString(FSafeBoxHash)+' calculated: '+TCrypto.ToHexaString(LAux)+' Blocks: '+IntToStr(BlocksCount));
  FCurrentProtocol := CT_PROTOCOL_5;
  BufferBlocksHash.SafeBoxHashCalcType := sbh_Merkle_Root_Hash;
  for LBlockNumber := 0 to BlocksCount - 1 do begin
    {$IFDEF USE_ABSTRACTMEM}
    LBlockAccount := FPCAbstractMem.GetBlockAccount(LBlockNumber);
    for i := Low(LBlockAccount.accounts) to High(LBlockAccount.accounts) do begin
      // Set the initial "updated_on_block_active_mode" value at same "updated_on_block_passive_mode"
      LBlockAccount.accounts[i].updated_on_block_active_mode := LBlockAccount.accounts[i].updated_on_block_passive_mode;
    end;
    LBlockAccount.block_hash := CalcBlockHash( LBlockAccount, CT_PROTOCOL_5);
    FPCAbstractMem.SetBlockAccount(LBlockAccount);
    {$ELSE}
    LPtrBlockAccount := PBlockAccount(FBlockAccountsList.Items[LBlockNumber]);
    for i := Low(LPtrBlockAccount^.accounts) to High(LPtrBlockAccount^.accounts) do begin
      // Set the initial "updated_on_block_active_mode" value at same "updated_on_block_passive_mode"
      LPtrBlockAccount^.accounts[i].updated_on_block_active_mode := LPtrBlockAccount^.accounts[i].updated_on_block_passive_mode;
    end;
    {$IFDEF uselowmem}
    TBaseType.To32Bytes(CalcBlockHash( GetBlock(LBlockNumber), CT_PROTOCOL_5),PBlockAccount(FBlockAccountsList.Items[LBlockNumber])^.block_hash);
    {$ELSE}
    PBlockAccount(FBlockAccountsList.Items[LBlockNumber])^.block_hash := CalcBlockHash( Block(LBlockNumber), CT_PROTOCOL_5);
    {$ENDIF}
    BufferBlocksHash.Replace(LBlockNumber * 32, PBlockAccount(FBlockAccountsList.Items[LBlockNumber])^.block_hash[0], 32 );
    {$ENDIF}
  end;
  FSafeBoxHash := CalcSafeBoxHash;
  TLog.NewLog(ltInfo,ClassName,'End Upgraded to protocol 5 - New safeboxhash:'+TCrypto.ToHexaString(FSafeBoxHash));
end;

function TPCSafeBox.DoUpgradeToProtocol6: Boolean;
begin
  FCurrentProtocol := CT_PROTOCOL_6;
  Result := True;
  TLog.NewLog(ltInfo,ClassName,'End Upgraded to protocol 6 - New safeboxhash:'+TCrypto.ToHexaString(FSafeBoxHash));
end;

function TPCSafeBox.BufferBlocksHash: TBytesBuffer32Safebox;
begin
  {$IFnDEF USE_ABSTRACTMEM}
  Result := FBufferBlocksHash;
  {$ELSE}
  Result := FPCAbstractMem.BufferBlocksHash;
  {$ENDIF}
end;

procedure TPCSafeBox.EndThreadSave;
begin
  FLock.Release;
end;

function TPCSafeBox.LoadSafeBoxChunkFromStream(Stream : TStream; checkAll : Boolean; checkSafeboxHash : TRawBytes; progressNotify : TProgressNotify; previousCheckedSafebox : TPCSafebox; var ALastReadBlock : TBlockAccount; var errors : String) : Boolean;
Var
  iblock,iacc, LTempCardinal : Cardinal;
  raw, LPreviousProofOfWork : TRawBytes;
  LBlock : TBlockAccount;
  {$IFnDEF USE_ABSTRACTMEM}
  P : PBlockAccount;
  {$ENDIF}
  i,j : Integer;
  savedSBH : TRawBytes;
  nPos,posOffsetZone : Int64;
  offsets : Array of Cardinal;
  sbHeader : TPCSafeBoxHeader;
  tc, LStartTickCount : TTickCount;
  LPrevious_Block_info : TOperationBlock;
  do_check_blockchain_info : Boolean;
  aux_errors : String;
  LUseMultiThreadOperationsBlockValidator, LAddToMultiThreadOperationsBlockValidator : Boolean;
  LPCOperationsBlockValidator : TPCOperationsBlockValidator;
  LValidatedOPOk, LValidatedOPError, LValidatedOPPending : Integer;
  LBlockHashRate : TBigNum;
begin
  Result := false;
  LPCOperationsBlockValidator := Nil;
  if checkAll then begin
    LUseMultiThreadOperationsBlockValidator := TCPUTool.GetLogicalCPUCount>1;
  end else LUseMultiThreadOperationsBlockValidator := False;
  If Assigned(FPreviousSafeBox) then Raise Exception.Create('Cannot loadSafeBoxFromStream on a Safebox in a Separate chain');
  if (previousCheckedSafebox = Self) then previousCheckedSafebox := Nil; // Protection
  tc := TPlatform.GetTickCount;
  StartThreadSafe;
  try
    {$IFDEF USE_ABSTRACTMEM}
    FPCAbstractMem.SavingNewSafeboxMode := True;
    {$ENDIF}
    LStartTickCount := tc;
    // Read Header info
    If not LoadSafeBoxStreamHeader(Stream,sbHeader) then begin
      errors := 'Invalid stream. Invalid header/version';
      exit;
    end;
    // Is this stream a Chunk?
    if (sbHeader.IsFullSafebox) or (sbHeader.ContainsFirstBlock) then begin
      ALastReadBlock := CT_BlockAccount_NUL;
      Clear; // Clear only when reading an entire safebox or starting at 0 block
      {$IFDEF USE_ABSTRACTMEM}
      if sbHeader.blocksCount<FPCAbstractMem.BlocksCount then begin
        FPCAbstractMem.EraseData;
      end else begin
        FPCAbstractMem.AccountsNames.EraseTree;
      end;
      AggregatedHashrate.Value := 0;
      {$ELSE}
      FBlockAccountsList.Capacity := sbHeader.blockscount;
      BufferBlocksHash.SetLength(sbHeader.blocksCount*32);
      {$ENDIF}
    end;
    Try
      case sbHeader.protocol of
        CT_PROTOCOL_1 : FCurrentProtocol := 1;
        CT_PROTOCOL_2 : FCurrentProtocol := 2;
        CT_PROTOCOL_3 : FCurrentProtocol := 3;
        CT_PROTOCOL_4 : FCurrentProtocol := 3; // In order to allow Upgrade to V4
        CT_PROTOCOL_5 : FCurrentProtocol := 5; // In order to upgrade to V4..V5
      else
        if sbHeader.protocol>CT_PROTOCOL_MAX then begin
          errors := 'Invalid protocol version or corrupted stream ('+IntToStr(sbHeader.protocol)+')';
          exit;
        end else FCurrentProtocol := sbHeader.protocol;
      end;
      if sbHeader.IsAChunk then begin
        if (sbHeader.startBlock<>BlocksCount) then begin
          errors := Format('Safebox (current 0..%d) chunk starts at %d',[BlocksCount,sbHeader.startBlock]);
          Exit;
        end;
      end;
      // Offset zone
      posOffsetZone := Stream.Position;
      If checkAll then begin
        SetLength(offsets,(sbHeader.GetSavedBlocksCount)+1); // Last offset = End of blocks
        Stream.Read(offsets[0],4*((sbHeader.GetSavedBlocksCount)+1));
      end else begin
        nPos := Stream.Position + (((sbHeader.GetSavedBlocksCount)+1) * 4);
        if Stream.Size<npos then exit;
        Stream.Position := nPos;
      end;
      // Build 1.3.0 to increase reading speed:
      errors := 'Corrupted stream';
      do_check_blockchain_info := Not Assigned(previousCheckedSafebox);
      if checkAll then
        LPCOperationsBlockValidator := TPCOperationsBlockValidator.Create
      else LPCOperationsBlockValidator := Nil;
      try
        if Assigned(LPCOperationsBlockValidator) then
          LPCOperationsBlockValidator.StartThreads;
        if sbHeader.ContainsFirstBlock then LPreviousProofOfWork := Nil
        else begin
          LPreviousProofOfWork := GetBlockInfo(BlocksCount-1).proof_of_work;
        end;
      for iblock := sbHeader.startBlock to sbHeader.endBlock do begin
        if (Assigned(progressNotify)) and ((TPlatform.GetElapsedMilliseconds(tc)>=300)) then begin
          tc := TPlatform.GetTickCount;
          progressNotify(Self,Format('Reading Safebox block %d/%d',[iblock+1,sbHeader.blocksCount]),iblock,sbHeader.blocksCount+1);
        end;
        errors := Format('Corrupted stream reading block %d/%d of %d',[iblock+1,sbHeader.endBlock+1,sbHeader.blockscount]);
        if (checkAll) then begin
          If (offsets[iblock - (sbHeader.startBlock) ]<>Stream.Position-posOffsetZone) then begin
            errors := errors + Format(' - offset[%d]:%d <> %d Position:%d offset:%d',[iblock,offsets[iblock - (sbHeader.startBlock)],Stream.Position-posOffsetZone,Stream.Position,posOffsetZone]);
            exit;
          end;
        end;

        LBlock := CT_BlockAccount_NUL;
        If Not TAccountComp.LoadTOperationBlockFromStream(Stream,LBlock.blockchainInfo) then begin
          errors := errors + ' Cannot load TOperationBlock';
          exit;
        end;
        if LBlock.blockchainInfo.block<>iBlock then begin
          errors := errors + Format(' invalid block number %d <> %d ',[LBlock.blockchainInfo.block,iBlock]);
          exit;
        end;
        for iacc := Low(LBlock.accounts) to High(LBlock.accounts) do begin
          errors := 'Corrupted stream reading account '+inttostr(iacc+1)+'/'+inttostr(length(LBlock.accounts))+' of block '+inttostr(iblock+1)+'/'+inttostr(sbHeader.blockscount);
          if Stream.Read(LBlock.accounts[iacc].account,4)<4 then exit;
          if TStreamOp.ReadAnsiString(Stream,raw)<0 then exit;
          LBlock.accounts[iacc].accountInfo := TAccountComp.RawString2AccountInfo(raw);
          if Stream.Read(LBlock.accounts[iacc].balance,SizeOf(UInt64))<SizeOf(UInt64) then exit;
          if Stream.Read(LBlock.accounts[iacc].updated_on_block_passive_mode,4)<4 then exit;
          if FCurrentProtocol>=CT_PROTOCOL_5 then begin
            if Stream.Read(LBlock.accounts[iacc].updated_on_block_active_mode,4)<4 then exit;
          end else LBlock.accounts[iacc].updated_on_block_active_mode := LBlock.accounts[iacc].updated_on_block_passive_mode;
          if Stream.Read(LBlock.accounts[iacc].n_operation,4)<4 then exit;
          If FCurrentProtocol>=CT_PROTOCOL_2 then begin
            if TStreamOp.ReadAnsiString(Stream,LBlock.accounts[iacc].name)<0 then exit;
            if Stream.Read(LBlock.accounts[iacc].account_type,2)<2 then exit;
          end;
          if FCurrentProtocol>=CT_PROTOCOL_5 then begin
            if TStreamOp.ReadAnsiString(Stream,LBlock.accounts[iacc].account_data)<0 then Exit;
            if (Length(LBlock.accounts[iacc].account_data)>CT_MaxAccountDataSize) then Exit;
            if TStreamOp.ReadAnsiString(Stream,LBlock.accounts[iacc].account_seal)<0 then Exit;
          end else begin
            if Stream.Read(LTempCardinal,4)<4 then exit;
          end;
          //
          // check valid
          If (Length(LBlock.accounts[iacc].name)>0) then begin
            if Not TPCSafeBox.ValidAccountName(LBlock.accounts[iacc].name,aux_errors) then begin
              errors := errors + ' > Invalid name "'+LBlock.accounts[iacc].name.ToPrintable+'": '+aux_errors;
              Exit;
            end;
            {$IFDEF USE_ABSTRACTMEM}
            if FPCAbstractMem.AccountsNames.FindByName(LBlock.accounts[iacc].name.ToString ) then begin
              errors := errors + ' Duplicate name "'+LBlock.accounts[iacc].name.ToPrintable+'"';
              Exit;
            end;
            FPCAbstractMem.AccountsNames.AddNameAndNumber(LBlock.accounts[iacc].name.ToString,LBlock.accounts[iacc].account);
            {$ELSE}
            if FOrderedByName.IndexOf(LBlock.accounts[iacc].name)>=0 then begin
              errors := errors + ' Duplicate name "'+LBlock.accounts[iacc].name.ToPrintable+'"';
              Exit;
            end;
            FOrderedByName.Add(LBlock.accounts[iacc].name,LBlock.accounts[iacc].account);
            {$ENDIF}
          end;
          If checkAll then begin
            if not TAccountComp.IsValidAccountInfo(LBlock.accounts[iacc].accountInfo,FCurrentProtocol,aux_errors) then begin
              errors := errors + ' > '+aux_errors;
              Exit;
            end;
          end;
          inc(FTotalBalance,Int64(LBlock.accounts[iacc].balance));
        end;
        errors := 'Corrupted stream reading block '+inttostr(iblock+1)+'/'+inttostr(sbHeader.blockscount);
        If TStreamOp.ReadAnsiString(Stream,LBlock.block_hash)<0 then exit;
        If Stream.Read(LBlock.accumulatedWork,SizeOf(LBlock.accumulatedWork)) < SizeOf(LBlock.accumulatedWork) then exit;

        if checkAll then begin
          if (Not do_check_blockchain_info) then begin
            // Only check if block not found on previous or different block
            if previousCheckedSafebox.BlocksCount>LBlock.blockchainInfo.block then begin
              LPrevious_Block_info := previousCheckedSafebox.GetBlockInfo( LBlock.blockchainInfo.block );
              do_check_blockchain_info := Not TAccountComp.EqualOperationBlocks(LBlock.blockchainInfo,LPrevious_Block_info);
            end else do_check_blockchain_info := True;
          end else do_check_blockchain_info := True;
          // Check is valid:
          if do_check_blockchain_info then begin
            // STEP 1: Validate the block
            {$IFDEF TESTNET}
              // For TESTNET increase speed purposes, will only check latests blocks
            if ((iblock + (CT_BankToDiskEveryNBlocks * 10)) >= sbHeader.blockscount) then begin
            {$ENDIF}
              {$IFDEF ASSUME_VALID_POW_OLD_PROTOCOLS}
              LAddToMultiThreadOperationsBlockValidator := False;
              {$ELSE}
              LAddToMultiThreadOperationsBlockValidator := (LUseMultiThreadOperationsBlockValidator) and (LBlock.blockchainInfo.protocol_version=CT_PROTOCOL_4) and (Assigned(LPCOperationsBlockValidator));
              {$ENDIF}
              If not IsValidNewOperationsBlock(LBlock.blockchainInfo,False,Not LAddToMultiThreadOperationsBlockValidator,aux_errors) then begin
                errors := errors + ' > ' + aux_errors;
                exit;
              end;
              if (LAddToMultiThreadOperationsBlockValidator) then begin
                LPCOperationsBlockValidator.AddToValidate(LBlock.blockchainInfo);
                LPCOperationsBlockValidator.GetStatus(LValidatedOPOk, LValidatedOPError, LValidatedOPPending);
                if LValidatedOPError>0 then begin
                  LPCOperationsBlockValidator.FillErrors(errors);
                  Exit;
                end;
              end;
            {$IFDEF TESTNET}
            end;
            {$ENDIF}
          end;

          // STEP 2: Check if valid block hash
          if (Not TBaseType.Equals(CalcBlockHash(LBlock,FCurrentProtocol),LBlock.block_hash)) then begin
            errors := errors + ' > Invalid block hash '+inttostr(iblock+1)+'/'+inttostr(sbHeader.blockscount);
            exit;
          end;
          // STEP 3: Check accumulatedWork
          if (iblock>0) then begin
            If (ALastReadBlock.accumulatedWork)+LBlock.blockchainInfo.compact_target <> LBlock.accumulatedWork then begin
              errors := errors + ' > Invalid accumulatedWork';
              exit;
            end;
          end;
        end;
        // Checking previous_proof_of_work
        if LBlock.blockchainInfo.protocol_version>=CT_PROTOCOL_5 then begin
          if (Not TBaseType.Equals(LBlock.blockchainInfo.previous_proof_of_work,LPreviousProofOfWork)) then begin
            errors := errors + ' > previous_proof_of_work does not match!';
            Exit;
          end;
        end else begin
          // Ensure no value on "previous_proof_of_work" field
          if (Length(LBlock.blockchainInfo.previous_proof_of_work)>0)
            and (Not TBaseType.Equals(LBlock.blockchainInfo.previous_proof_of_work,LPreviousProofOfWork)) then begin
            errors := errors + ' > contains previous_proof_of_work on protocol<5 different than needed!';
            Exit;
          end;
        end;

        // Add
        {$IFDEF USE_ABSTRACTMEM}
        FPCAbstractMem.SetBlockAccount(LBlock);
        {$ELSE}
        New(P);
        ToTMemBlockAccount(LBlock,P^);
        FBlockAccountsList.Add(P);
        // BufferBlocksHash fill with data
        j := (length(LBlock.block_hash)*(iBlock));
        BufferBlocksHash.Replace( j, LBlock.block_hash[0], 32 );
        for j := low(LBlock.accounts) to High(LBlock.accounts) do begin
          FAccountsOrderedByUpdatedBlock.Update(
            LBlock.accounts[j].account,
            0,
            LBlock.accounts[j].updated_on_block_active_mode);
          FAccountsOrderedBySalePrice.UpdateAccountBySalePrice(
            LBlock.accounts[j].account,
            CT_AccountInfo_NUL,
            LBlock.accounts[j].accountInfo
            );
        end;
        {$ENDIF}
        for j := low(LBlock.accounts) to High(LBlock.accounts) do begin
          AccountKeyListAddAccounts(LBlock.accounts[j].accountInfo.accountKey,[LBlock.accounts[j].account]);
        end;
        ALastReadBlock := LBlock;
        Inc(FWorkSum,LBlock.blockchainInfo.compact_target);
        LBlockHashRate := TBigNum.TargetToHashRate( LBlock.blockchainInfo.compact_target );
        Try
          AggregatedHashrate.Add( LBlockHashRate );
        finally
          LBlockHashRate.Free;
        end;
        // Upgrade to Protocol 4,5... step:CT_PROTOCOL_5
        if (LBlock.blockchainInfo.protocol_version>FCurrentProtocol) then begin
          if (LBlock.blockchainInfo.protocol_version = CT_PROTOCOL_4) then begin
            FCurrentProtocol := CT_PROTOCOL_4;
          end else if (LBlock.blockchainInfo.protocol_version = CT_PROTOCOL_5) then begin
            FCurrentProtocol := CT_PROTOCOL_5;
          end;
        end;
        // Assign to previous
        LPreviousProofOfWork := LBlock.blockchainInfo.proof_of_work;
      end; // For iBlock ...
        if Assigned(LPCOperationsBlockValidator) then begin
          repeat
            LPCOperationsBlockValidator.GetStatus(LValidatedOPOk, LValidatedOPError, LValidatedOPPending);
            if LValidatedOPError>0 then begin
              LPCOperationsBlockValidator.FillErrors(errors);
              Exit;
            end;
            if LValidatedOPPending>0 then begin
              if (Assigned(progressNotify)) and ((TPlatform.GetElapsedMilliseconds(tc)>=500)) then begin
                tc := TPlatform.GetTickCount;
                progressNotify(Self,Format('Validating OperationBlock info %d/%d',[LValidatedOPOk,LValidatedOPOk+LValidatedOPPending]),LValidatedOPOk,LValidatedOPOk+LValidatedOPPending);
              end else Sleep(10)
            end;
          until LValidatedOPPending<=0 ;
        end;
      finally
        LPCOperationsBlockValidator.Free;
      end;
      if (Assigned(progressNotify)) And (sbHeader.ContainsLastBlock) then begin
        progressNotify(Self,'Checking Safebox integrity',(sbHeader.endBlock-sbHeader.startBlock+1),(sbHeader.endBlock-sbHeader.startBlock+1));
      end;
      If checkAll then begin
        If (offsets[(sbHeader.endBlock-sbHeader.startBlock+1)]<>0) And (offsets[(sbHeader.endBlock-sbHeader.startBlock+1)]<>Stream.Position-posOffsetZone) then begin
          errors := errors + Format(' - Final offset[%d of %d]=%d <> Eof Position:%d offset:%d',[(sbHeader.endBlock-sbHeader.startBlock+1),sbHeader.blockscount,offsets[(sbHeader.endBlock-sbHeader.startBlock+1)],Stream.Position-posOffsetZone,posOffsetZone]);
          exit;
        end;
      end;
      // Finally load SafeBoxHash
      If TStreamOp.ReadAnsiString(stream,savedSBH)<0 then begin
        errors := 'No SafeBoxHash value';
        exit;
      end;
      // Check worksum value
      If sbHeader.blockscount>0 then begin
        If (FWorkSum<>ALastReadBlock.accumulatedWork) then begin
          errors := 'Invalid WorkSum value';
          exit;
        end;
      end;
      if (sbHeader.IsFullSafebox) or (sbHeader.ContainsLastBlock) then begin
        // Calculating safe box hash
        FSafeBoxHash := CalcSafeBoxHash;
        // Checking saved SafeBoxHash
        If (Not TBaseType.Equals(FSafeBoxHash,savedSBH)) then begin
          errors := 'Invalid SafeBoxHash value in stream '+TCrypto.ToHexaString(FSafeBoxHash)+'<>'+TCrypto.ToHexaString(savedSBH)+' Last block:'+IntToStr(ALastReadBlock.blockchainInfo.block);
          exit;
        end;
        // Check that checkSafeboxHash is as expected
        if (Length(checkSafeboxHash)>0) then begin
          if (Not TBaseType.Equals(checkSafeboxHash,FSafeBoxHash)) then begin
            errors := 'Invalid SafeboxHash, does not match '+TCrypto.ToHexaString(FSafeBoxHash)+'<>'+TCrypto.ToHexaString(checkSafeboxHash)+' Last block:'+IntToStr(ALastReadBlock.blockchainInfo.block);
            Exit;
          end;

        end;
      end;
      Result := true;
    Finally
      if Not Result then Clear else errors := '';
    End;
  Finally
    {$IFDEF USE_ABSTRACTMEM}
    FPCAbstractMem.SavingNewSafeboxMode := False;
    {$ENDIF}
    EndThreadSave;
  end;
  TLog.NewLog(ltdebug,ClassName,Format('Finalized read Safebox from blocks %d to %d (total %d blocks) in %.2f seconds',
    [sbHeader.startBlock,sbHeader.endBlock,sbHeader.blocksCount,TPlatform.GetElapsedMilliseconds(LStartTickCount)/1000]));
end;


function TPCSafeBox.LoadSafeBoxFromStream(Stream : TStream; checkAll : Boolean; checkSafeboxHash : TRawBytes; progressNotify : TProgressNotify; previousCheckedSafebox : TPCSafebox; var ALastReadBlock : TBlockAccount; var errors : String) : Boolean;
begin
  Result := LoadSafeBoxChunkFromStream(Stream,checkAll,checkSafeboxHash,progressNotify,previousCheckedSafebox,ALastReadBlock,errors);
end;

function TPCSafeBox.LoadSafeBoxFromStream(Stream: TStream; checkAll: Boolean; var LastReadBlock: TBlockAccount; var errors: String): Boolean;
var pn : TProgressNotify;
begin
  pn := Nil;
  Result := LoadSafeBoxFromStream(Stream,checkAll,Nil,pn,Nil,LastReadBlock,errors);
end;

class function TPCSafeBox.LoadSafeBoxStreamHeader(Stream: TStream; var sbHeader : TPCSafeBoxHeader) : Boolean;
  // This function reads SafeBox stream info and sets position at offset start zone if valid, otherwise sets position to actual position
Var w : Word;
  raw : TRawBytes;
  safeBoxBankVersion : Word;
  offsetPos, initialPos  : Int64;
  endBlocks : Cardinal;
begin
  Result := false;
  sbHeader := CT_PCSafeBoxHeader_NUL;
  initialPos := Stream.Position;
  try
    TStreamOp.ReadAnsiString(Stream,raw);
    if (raw.ToPrintable<>CT_MagicIdentificator) then exit;
    if Stream.Size<8 then exit;
    Stream.Read(w,SizeOf(w));
    if not (w in [CT_PROTOCOL_1..CT_PROTOCOL_MAX]) then exit;
    sbHeader.protocol := w;
    Stream.Read(safeBoxBankVersion,2);
    if safeBoxBankVersion<>CT_SafeBoxBankVersion then exit;
    Stream.Read(sbHeader.blocksCount,4);
    Stream.Read(sbHeader.startBlock,4);
    Stream.Read(sbHeader.endBlock,4);
    if (sbHeader.blocksCount<=0) Or (sbHeader.blocksCount>(CT_NewLineSecondsAvg*2000000)) then exit; // Protection for corrupted data...
    offsetPos := Stream.Position;
    // Go to read SafeBoxHash
    If (Stream.size<offsetPos + (((sbHeader.endBlock - sbHeader.startBlock)+2)*4)) then exit;
    Stream.position := offsetPos + (((sbHeader.endBlock - sbHeader.startBlock)+1)*4);
    Stream.Read(endBlocks,4);
    // Go to end
    If (Stream.Size<offsetPos + (endBlocks)) then exit;
    Stream.Position:=offsetPos + endBlocks;
    If TStreamOp.ReadAnsiString(Stream,sbHeader.safeBoxHash)<0 then exit;
    // Back
    Stream.Position:=offsetPos;
    Result := True;
  finally
    If not Result then Stream.Position := initialPos;
  end;
end;

class function TPCSafeBox.SaveSafeBoxStreamHeader(Stream: TStream;
  protocol: Word; OffsetStartBlock, OffsetEndBlock,
  CurrentSafeBoxBlocksCount: Cardinal): Boolean;
var c : Cardinal;
begin
  Result := False;
  // Header zone
  TStreamOp.WriteAnsiString(Stream,TEncoding.ASCII.GetBytes(CT_MagicIdentificator));
  Stream.Write(protocol,SizeOf(protocol));
  Stream.Write(CT_SafeBoxBankVersion,SizeOf(CT_SafeBoxBankVersion));
  c := CurrentSafeBoxBlocksCount;
  Stream.Write(c,Sizeof(c)); // Save Total blocks of the safebox
  c := OffsetStartBlock;
  Stream.Write(c,Sizeof(c)); // Save first block saved
  c := OffsetEndBlock;
  Stream.Write(c,Sizeof(c)); // Save last block saved
  Result := True;
end;

class function TPCSafeBox.MustSafeBoxBeSaved(BlocksCount: Cardinal): Boolean;
begin
  Result := (BlocksCount MOD CT_BankToDiskEveryNBlocks)=0;
end;

class procedure TPCSafeBox.SaveSafeBoxBlockToAStream(ADestStream : TStream; ACurrentProtocol : Integer; const ABlock : TBlockAccount);
var
  iacc : integer;
  Stream : TStream;
  LCardinal : Cardinal;
begin
  if ADestStream is TMemoryStream then Stream := ADestStream
  else begin
    Stream := TMemoryStream.Create;
  end;
  try
    TAccountComp.SaveTOperationBlockToStream(Stream,ABlock.blockchainInfo);
    for iacc := Low(ABlock.accounts) to High(ABlock.accounts) do begin
      Stream.Write(ABlock.accounts[iacc].account,Sizeof(ABlock.accounts[iacc].account));
      TStreamOp.WriteAnsiString(Stream,TAccountComp.AccountInfo2RawString(ABlock.accounts[iacc].accountInfo));
      Stream.Write(ABlock.accounts[iacc].balance,Sizeof(ABlock.accounts[iacc].balance));
      if ACurrentProtocol>=CT_PROTOCOL_5 then begin
        Stream.Write(ABlock.accounts[iacc].updated_on_block_passive_mode,Sizeof(ABlock.accounts[iacc].updated_on_block_passive_mode));
        Stream.Write(ABlock.accounts[iacc].updated_on_block_active_mode,Sizeof(ABlock.accounts[iacc].updated_on_block_active_mode));
      end else begin
        LCardinal := ABlock.accounts[iacc].GetLastUpdatedBlock;
        Stream.Write(LCardinal,SizeOf(LCardinal));
      end;
      Stream.Write(ABlock.accounts[iacc].n_operation,Sizeof(ABlock.accounts[iacc].n_operation));
      If ACurrentProtocol>=CT_PROTOCOL_2 then begin
        TStreamOp.WriteAnsiString(Stream,ABlock.accounts[iacc].name);
        Stream.Write(ABlock.accounts[iacc].account_type,SizeOf(ABlock.accounts[iacc].account_type));
      end;
      if ACurrentProtocol>=CT_PROTOCOL_5 then begin
        TStreamOp.WriteAnsiString(Stream,ABlock.accounts[iacc].account_data);
        TStreamOp.WriteAnsiString(Stream,ABlock.accounts[iacc].account_seal);
      end else begin
        LCardinal := 0;
        Stream.Write(LCardinal,Sizeof(LCardinal));
      end;
    end;
    TStreamOp.WriteAnsiString(Stream,ABlock.block_hash);
    Stream.Write(ABlock.accumulatedWork,Sizeof(ABlock.accumulatedWork));
  finally
    if (Stream<>ADestStream) then begin
      ADestStream.CopyFrom(Stream,0);
      Stream.Free;
    end;
  end;
end;

procedure TPCSafeBox.SaveSafeBoxToAStream(Stream: TStream; FromBlock, ToBlock : Cardinal);
Var
  totalBlocks,iblock : Cardinal;
  b : TBlockAccount;
  posOffsetZone, posFinal : Int64;
  offsets : TCardinalsArray;
  raw : TRawBytes;
begin
  If (FromBlock>ToBlock) Or (ToBlock>=BlocksCount) then Raise Exception.Create(Format('Cannot save SafeBox from %d to %d (currently %d blocks)',[FromBlock,ToBlock,BlocksCount]));
  StartThreadSafe;
  Try
    // Header zone
    SaveSafeBoxStreamHeader(Stream,FCurrentProtocol,FromBlock,ToBlock,BlocksCount);
    totalBlocks := ToBlock - FromBlock + 1;
    // Offsets zone
    posOffsetZone:=Stream.Position;
    SetLength(raw,(totalBlocks+1)*4); // Last position = end
    FillChar(raw[Low(raw)],Length(raw),0);
    Stream.WriteBuffer(raw[Low(raw)],Length(raw));
    setLength(offsets,totalBlocks+1); // c = total blocks  - Last position = offset to end
    // Blocks zone
    for iblock := FromBlock to ToBlock do begin
      offsets[iBlock] := Stream.Position - posOffsetZone;
      b := GetBlock(iBlock);
      SaveSafeBoxBlockToAStream(Stream,FCurrentProtocol,b);
    end;
    offsets[High(offsets)] := Stream.Position - posOffsetZone;
    // Save offsets zone with valid values
    posFinal := Stream.Position;
    Stream.Position := posOffsetZone;
    for iblock := FromBlock to ToBlock+1 do begin
      Stream.Write(offsets[iblock],SizeOf(offsets[iblock]));
    end;
    Stream.Position := posFinal;
    // Final zone: Save safeboxhash for next block
    If (ToBlock+1<BlocksCount) then begin
      b := GetBlock(ToBlock);
      TStreamOp.WriteAnsiString(Stream,b.blockchainInfo.initial_safe_box_hash);
    end else begin
      TStreamOp.WriteAnsiString(Stream,FSafeBoxHash);
    end;
  Finally
    EndThreadSave;
  end;
end;

{$IFDEF USE_ABSTRACTMEM}
class function TPCSafeBox.CopyAbstractMemToSafeBoxStream(
  ASource: TPCAbstractMem; ADestStream: TStream; AFromBlock, AToBlock: Cardinal;
  var AErrors: String): Boolean;
Var
  iblock : Cardinal;
  raw : TRawBytes;
  LposOffsetZoneDest,
  LposFinal,
  LposBlocksZoneDest : Int64;
  LOffsetsDest : TCardinalsArray;
  LdestTotalBlocks : Cardinal;
  LsbHeader : TPCSafeBoxHeader;
  LBlock : TBlockAccount;
  Ltc : TTickCount;
begin
  Ltc := TPlatform.GetTickCount;
  Result := False; AErrors := '';
  try
    If (AFromBlock>AToBlock) then begin
      AErrors := Format('Invalid CopySafeBoxStream(from %d, to %d)',[AFromBlock,AToBlock]);
      Exit;
    end;
    LsbHeader := CT_PCSafeBoxHeader_NUL;
    LsbHeader.startBlock := 0;
    LsbHeader.blocksCount := ASource.BlocksCount;
    LsbHeader.endBlock := LsbHeader.blocksCount-1;
    if (LsbHeader.endBlock>0) then begin
      LsbHeader.protocol := ASource.GetBlockInfo( LsbHeader.endBlock ).operationBlock.protocol_version;
      LsbHeader.safeBoxHash := ASource.BufferBlocksHash.GetSafeBoxHash;
    end else begin
      AErrors := Format('Invalid TPCAbstractMem (from %d, to %d)',[AFromBlock,AToBlock]);
      Exit;
    end;

    if (LsbHeader.startBlock>AFromBlock) Or (LsbHeader.endBlock<AToBlock) Or ((LsbHeader.startBlock + LsbHeader.blocksCount)<AToBlock) then begin
      AErrors := Format('Stream contain blocks from %d to %d (of %d). Need between %d and %d !',[LsbHeader.startBlock,LsbHeader.endBlock,LsbHeader.blocksCount,AFromBlock,AToBlock]);
      exit;
    end;
    LdestTotalBlocks := AToBlock - AFromBlock + 1;
    // DEST STREAM:
    // Init dest stream
    // Header zone
    SaveSafeBoxStreamHeader(ADestStream,LsbHeader.protocol,AFromBlock,AToBlock,LsbHeader.blocksCount);
    // Offsets zone (Will save later)
    LposOffsetZoneDest:=ADestStream.Position;
    SetLength(raw,(LdestTotalBlocks+1)*4); // Cardinal = 4 bytes for each block + End position
    FillChar(raw[Low(raw)],Length(raw),0);
    ADestStream.WriteBuffer(raw[Low(raw)],Length(raw));
    setLength(LOffsetsDest,LdestTotalBlocks+1);
    // Blocks zone
    LposBlocksZoneDest := ADestStream.Position;

    for iblock := AFromBlock to AToBlock do begin
      LOffsetsDest[iblock - AFromBlock] := ADestStream.Position - LposOffsetZoneDest;
      LBlock := ASource.GetBlockAccount( iBlock );
      TPCSafeBox.SaveSafeBoxBlockToAStream( ADestStream, LsbHeader.protocol, LBlock);
    end;
    LOffsetsDest[High(LOffsetsDest)] := ADestStream.Position - LposOffsetZoneDest;

    TStreamOp.WriteAnsiString(ADestStream,ASource.BufferBlocksHash.GetSafeBoxHash);

    // Save offsets zone
    LposFinal := ADestStream.Position;
    ADestStream.Position := LposOffsetZoneDest;
    ADestStream.WriteBuffer(LOffsetsDest[0],Length(LOffsetsDest)*4);
    ADestStream.Position := LposFinal;

    Result := true;
    TLog.NewLog(ltDebug,ClassName,Format('CopyAbstractMemToSafeBoxStream from safebox with %d to %d (of %d sbh:%s) to safebox with %d and %d in %.2f seconds',
      [LsbHeader.startBlock,LsbHeader.endBlock,LsbHeader.BlocksCount,TCrypto.ToHexaString(LsbHeader.safeBoxHash),AFromBlock,AToBlock,
       TPlatform.GetElapsedMilliseconds(Ltc)/1000]));
  finally
  end;
end;
{$ENDIF}

class function TPCSafeBox.CopySafeBoxStream(Source, Dest: TStream; FromBlock,ToBlock: Cardinal; var errors : String) : Boolean;
Var
  iblock : Cardinal;
  raw : TRawBytes;
  posOffsetZoneSource, posOffsetZoneDest, posFinal, posBlocksZoneDest, posInitial : Int64;
  offsetsSource,offsetsDest : TCardinalsArray;
  destTotalBlocks : Cardinal;
  sbHeader : TPCSafeBoxHeader;
begin
  Result := False; errors := '';
  posInitial := Source.Position;
  try
    If (FromBlock>ToBlock) then begin
      errors := Format('Invalid CopySafeBoxStream(from %d, to %d)',[FromBlock,ToBlock]);
      exit;
    end;
    If not LoadSafeBoxStreamHeader(Source,sbHeader) then begin
      errors := 'Invalid stream. Invalid header/version';
      exit;
    end;
    if (sbHeader.startBlock>FromBlock) Or (sbHeader.endBlock<ToBlock) Or ((sbHeader.startBlock + sbHeader.blocksCount)<ToBlock) then begin
      errors := Format('Stream contain blocks from %d to %d (of %d). Need between %d and %d !',[sbHeader.startBlock,sbHeader.endBlock,sbHeader.blocksCount,FromBlock,ToBlock]);
      exit;
    end;
    destTotalBlocks := ToBlock - FromBlock + 1;
    TLog.NewLog(ltDebug,ClassName,Format('CopySafeBoxStream from safebox with %d to %d (of %d sbh:%s) to safebox with %d and %d',
      [sbHeader.startBlock,sbHeader.endBlock,sbHeader.BlocksCount,TCrypto.ToHexaString(sbHeader.safeBoxHash),FromBlock,ToBlock]));
    // Read Source Offset zone
    posOffsetZoneSource := Source.Position;
    SetLength(offsetsSource,(sbHeader.endBlock-sbHeader.startBlock)+2);
    Source.Read(offsetsSource[0],4*Length(offsetsSource));
    // DEST STREAM:
    // Init dest stream
    // Header zone
    SaveSafeBoxStreamHeader(Dest,sbHeader.protocol,FromBlock,ToBlock,sbHeader.blocksCount);
    // Offsets zone
    posOffsetZoneDest:=Dest.Position;
    SetLength(raw,(destTotalBlocks+1)*4); // Cardinal = 4 bytes for each block + End position
    FillChar(raw[Low(raw)],Length(raw),0);
    Dest.WriteBuffer(raw[Low(raw)],Length(raw));
    setLength(offsetsDest,destTotalBlocks+1);
    // Blocks zone
    posBlocksZoneDest := Dest.Position;
    TLog.NewLog(ltDebug,Classname,
      Format('Copying Safebox Stream from source Position %d (size:%d) to dest %d bytes - OffsetSource[%d] - OffsetSource[%d]',
       [posOffsetZoneSource + offsetsSource[FromBlock - sbHeader.startBlock], Source.Size,
        offsetsSource[ToBlock - sbHeader.startBlock + 1] - offsetsSource[FromBlock - sbHeader.startBlock],
        ToBlock - sbHeader.startBlock + 1, FromBlock - sbHeader.startBlock
        ]));

    Source.Position:=posOffsetZoneSource + offsetsSource[FromBlock - sbHeader.startBlock];
    Dest.CopyFrom(Source,offsetsSource[ToBlock - sbHeader.startBlock + 1] - offsetsSource[FromBlock - sbHeader.startBlock]);
    // Save offsets zone with valid values
    posFinal := Dest.Position;
    Dest.Position := posOffsetZoneDest;
    for iblock := FromBlock to ToBlock do begin
      offsetsDest[iblock - FromBlock] := offsetsSource[iblock - (sbHeader.startBlock)] - offsetsSource[FromBlock - sbHeader.startBlock] + (posBlocksZoneDest - posOffsetZoneDest);
    end;
    offsetsDest[high(offsetsDest)] := posFinal - posOffsetZoneDest;

    Dest.WriteBuffer(offsetsDest[0],Length(offsetsDest)*4);
    Dest.Position := posFinal;
    Source.Position := offsetsSource[High(offsetsSource)] + posOffsetZoneSource;
    TStreamOp.ReadAnsiString(Source,raw);
    TStreamOp.WriteAnsiString(Dest,raw);
    Result := true;
  finally
    Source.Position:=posInitial;
  end;
end;

class function TPCSafeBox.ConcatSafeBoxStream(Source1, Source2, Dest: TStream; var errors: String): Boolean;
  function MinCardinal(v1,v2 : Cardinal) : Cardinal;
  begin
    if v1<v2 then Result:=v1
    else Result:=v2;
  end;
  function MaxCardinal(v1,v2 : Cardinal) : Cardinal;
  begin
    if v1>v2 then Result:=v1
    else Result:=v2;
  end;
  function ReadSafeBoxBlockFromStream(safeBoxStream : TStream; offsetIndex : Cardinal; destStream : TStream) : Cardinal;
    // PRE: safeBoxStream is a valid SafeBox Stream (with enough size) located at Offsets zone, and offsetIndex is >=0 and <= end block
    // Returns the size of the saved block at destStream
  var offsetPos, auxPos : Int64;
    c,cNext : Cardinal;
  begin
    Result := 0;
    offsetPos := safeBoxStream.Position;
    try
      safeBoxStream.Seek(4*offsetIndex,soFromCurrent);
      safeBoxStream.Read(c,4);
      safeBoxStream.Read(cNext,4);
      if cNext<c then exit;
      Result := cNext - c; // Result is the offset difference between blocks
      if Result<=0 then exit;
      auxPos := offsetPos + c;
      if safeBoxStream.Size<auxPos+Result then exit; // Invalid
      safeBoxStream.Position:=auxPos;
      destStream.CopyFrom(safeBoxStream,Result);
    finally
      safeBoxStream.Position:=offsetPos;
    end;
  end;

  procedure WriteSafeBoxBlockToStream(Stream, safeBoxStream : TStream; nBytes : Integer; offsetIndex, totalOffsets : Cardinal);
  // PRE: safeBoxStream is a valid SafeBox Stream located at Offsets zone, and offsetIndex=0 or offsetIndex-1 has a valid value
  var offsetPos : Int64;
    c,cLength : Cardinal;
  begin
    offsetPos := safeBoxStream.Position;
    try
      if offsetIndex=0 then begin
        // First
        c := ((totalOffsets+1)*4);
        safeBoxStream.Write(c,4);
      end else begin
        safeBoxStream.Seek(4*(offsetIndex),soFromCurrent);
        safeBoxStream.Read(c,4); // c is position
      end;
      cLength := c + nBytes;
      safeBoxStream.Write(cLength,4);
      safeBoxStream.Position := offsetPos + c;
      safeBoxStream.CopyFrom(Stream,nBytes);
    finally
      safeBoxStream.Position:=offsetPos;
    end;
  end;

Var destStartBlock, destEndBlock, nBlock : Cardinal;
  source1InitialPos, source2InitialPos,
  destOffsetPos: Int64;
  ms : TMemoryStream;
  c : Cardinal;
  destOffsets : TCardinalsArray;
  i : Integer;
  s1Header,s2Header : TPCSafeBoxHeader;
begin
  Result := False; errors := '';
  source1InitialPos:=Source1.Position;
  source2InitialPos:=Source2.Position;
  Try
    If not LoadSafeBoxStreamHeader(Source1,s1Header) then begin
      errors := 'Invalid source 1 stream. Invalid header/version';
      exit;
    end;
    If not LoadSafeBoxStreamHeader(Source2,s2Header) then begin
      errors := 'Invalid source 2 stream. Invalid header/version';
      exit;
    end;
    // Check SBH and blockcount
    if (Not TBaseType.Equals(s1Header.safeBoxHash,s2Header.safeBoxHash)) or (s1Header.blocksCount<>s2Header.blocksCount) Or (s1Header.protocol<>s2Header.protocol) then begin
      errors := Format('Source1 and Source2 have diff safebox. Source 1 %d %s (protocol %d) Source 2 %d %s (protocol %d)',
       [s1Header.blocksCount,TCrypto.ToHexaString(s1Header.safeBoxHash),s1Header.protocol,
        s2Header.blocksCount,TCrypto.ToHexaString(s2Header.safeBoxHash),s2Header.protocol]);
      exit;
    end;
    // Save dest heaer
    destStartBlock := MinCardinal(s1Header.startBlock,s2Header.startBlock);
    destEndBlock := MaxCardinal(s1Header.endBlock,s2Header.endBlock);
    SaveSafeBoxStreamHeader(Dest,s1Header.protocol,destStartBlock,destEndBlock,s1Header.blocksCount);
    // Save offsets
    destOffsetPos:=Dest.Position;
    SetLength(destOffsets,((destEndBlock-destStartBlock)+2));
    for i:=low(destOffsets) to high(destOffsets) do destOffsets[i] := 0;
    Dest.Write(destOffsets[0],((destEndBlock-destStartBlock)+2)*4);
    Dest.Position:=destOffsetPos;
    //
    nBlock := destStartBlock;
    ms := TMemoryStream.Create;
    try
      for nBlock :=destStartBlock to destEndBlock do begin
        ms.Clear;
        if (nBlock>=s1Header.startBlock) And (nBlock<=s1Header.endBlock) then begin
          c := ReadSafeBoxBlockFromStream(Source1,nBlock-s1Header.startBlock,ms);
          ms.Position:=0;
          WriteSafeBoxBlockToStream(ms,Dest,c,nBlock-destStartBlock,destEndBlock-destStartBlock+1);
        end else if (nBlock>=s2Header.startBlock) and (nBlock<=s2Header.endBlock) then begin
          c := ReadSafeBoxBlockFromStream(Source2,nBlock-s2Header.startBlock,ms);
          ms.Position:=0;
          WriteSafeBoxBlockToStream(ms,Dest,c,nBlock-destStartBlock,destEndBlock-destStartBlock+1);
        end else Raise Exception.Create('ERROR DEV 20170518-1');
      end;
    Finally
      ms.Free;
    end;
    // Save SafeBoxHash at the end
    Dest.Seek(0,soFromEnd);
    TStreamOp.WriteAnsiString(Dest,s1Header.safeBoxHash);
    Result := true;
  Finally
    Source1.Position:=source1InitialPos;
    Source2.Position:=source2InitialPos;
  end;
end;


class function TPCSafeBox.ValidAccountName(const new_name: TRawBytes; var errors : String): Boolean;
  { Note:
    This function is case senstive, and only lower case chars are valid.
    Execute a LowerCase() prior to call this function!
    }
Const CT_PascalCoin_Base64_Charset : RawByteString = 'abcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()-+{}[]\_:"|<>,.?/~';
      // First char can't start with a number
      CT_PascalCoin_FirstChar_Charset : RawByteString = 'abcdefghijklmnopqrstuvwxyz!@#$%^&*()-+{}[]\_:"|<>,.?/~';
      CT_PascalCoin_name_min_length = 3;
      CT_PascalCoin_name_max_length = 64;
var i,j : Integer;
begin
  Result := False; errors := '';
  if (length(new_name)<CT_PascalCoin_name_min_length) Or (length(new_name)>CT_PascalCoin_name_max_length) then begin
    errors := 'Invalid length:'+IntToStr(Length(new_name))+' (valid from '+Inttostr(CT_PascalCoin_name_max_length)+' to '+IntToStr(CT_PascalCoin_name_max_length)+')';
    Exit;
  end;
  for i:=Low(new_name) to High(new_name) do begin
    if (i=Low(new_name)) then begin
      j:=Low(CT_PascalCoin_FirstChar_Charset);
      // First char can't start with a number
      While (j<=High(CT_PascalCoin_FirstChar_Charset)) and (Ord(new_name[i])<>Ord(CT_PascalCoin_FirstChar_Charset[j])) do inc(j);
      if j>High(CT_PascalCoin_FirstChar_Charset) then begin
        errors := 'Invalid char '+Char(new_name[i])+' at first pos';
        Exit; // Not found
      end;
    end else begin
      j:=Low(CT_PascalCoin_Base64_Charset);
      While (j<=High(CT_PascalCoin_Base64_Charset)) and (Ord(new_name[i])<>Ord(CT_PascalCoin_Base64_Charset[j])) do inc(j);
      if j>High(CT_PascalCoin_Base64_Charset) then begin
        errors := 'Invalid char '+Char(new_name[i])+' at pos '+IntToStr(i);
        Exit; // Not found
      end;
    end;
  end;
  Result := True;
end;

var _initialSafeboxHash : TRawBytes = Nil;

class function TPCSafeBox.InitialSafeboxHash: TRawBytes;
begin
  if (Length(_initialSafeboxHash)=0) then begin
     _initialSafeboxHash := TCrypto.DoSha256(TEncoding.ASCII.GetBytes(CT_Genesis_Magic_String_For_Old_Block_Hash))
  end;
  Result := Copy(_initialSafeboxHash);
end;

function TPCSafeBox.IsValidNewOperationsBlock(const newOperationBlock: TOperationBlock; checkSafeBoxHash, checkValidOperationsBlock : Boolean; var errors: String): Boolean;
  { This function will check a OperationBlock info as a valid candidate to be included in the safebox

    TOperationBlock contains the info of the new block EXCEPT the operations, including only operations_hash value (SHA256 of the Operations)
    So, cannot check operations and fee values
  }
var target_hash, pow : TRawBytes;
  i : Integer;
  lastBlock : TOperationBlock;
  isChangeTargetBlock : Boolean;
  newMinimumTargetBlock : Cardinal;
begin
  Result := False;
  errors := '';
  If BlocksCount>0 then lastBlock := GetBlockInfo(BlocksCount-1)
  else lastBlock := CT_OperationBlock_NUL;
  // Check block
  if (BlocksCount <> newOperationBlock.block) then begin
    errors := 'block ('+inttostr(newOperationBlock.block)+') is not new position ('+inttostr(BlocksCount)+')';
    exit;
  end;

  // fee: Cannot be checked only with the safebox
  // protocol available is not checked
  isChangeTargetBlock := False;
  if (newOperationBlock.block > 0) then begin
    // protocol
    if (newOperationBlock.protocol_version<>lastBlock.protocol_version) then begin
      // Protocol must be 1 or 2. If 1 then all prior blocksmust be 1 and never 2 (invalide blockchain version scenario v1...v2...v1)
      If (lastBlock.protocol_version>newOperationBlock.protocol_version) then begin
        errors := 'Invalid PascalCoin protocol version: '+IntToStr( newOperationBlock.protocol_version )+' Current: '+IntToStr(CurrentProtocol)+' Previous:'+IntToStr(lastBlock.protocol_version);
        exit;
      end;
      If (newOperationBlock.protocol_version=CT_PROTOCOL_6) then begin
        If (newOperationBlock.block<CT_Protocol_Upgrade_v6_MinBlock) then begin
          errors := 'Upgrade to protocol version 6 available at block: '+IntToStr(CT_Protocol_Upgrade_v6_MinBlock);
          exit;
        end;
      end else If (newOperationBlock.protocol_version=CT_PROTOCOL_5) then begin
        If (newOperationBlock.block<CT_Protocol_Upgrade_v5_MinBlock) then begin
          errors := 'Upgrade to protocol version 5 available at block: '+IntToStr(CT_Protocol_Upgrade_v5_MinBlock);
          exit;
        end;
      end else If (newOperationBlock.protocol_version=CT_PROTOCOL_4) then begin
        If (newOperationBlock.block<CT_Protocol_Upgrade_v4_MinBlock) then begin
          errors := 'Upgrade to protocol version 4 available at block: '+IntToStr(CT_Protocol_Upgrade_v4_MinBlock);
          exit;
        end;
        {$IFDEF ACTIVATE_RANDOMHASH_V4}
        // Change target on first block of V4 protocol
        isChangeTargetBlock := true;
        newMinimumTargetBlock := TPascalCoinProtocol.ResetTarget(newOperationBlock.compact_target,CT_PROTOCOL_4);
        {$ENDIF}
      end else If (newOperationBlock.protocol_version=CT_PROTOCOL_3) then begin
        If (newOperationBlock.block<CT_Protocol_Upgrade_v3_MinBlock) then begin
          errors := 'Upgrade to protocol version 3 available at block: '+IntToStr(CT_Protocol_Upgrade_v3_MinBlock);
          exit;
        end;
      end else If (newOperationBlock.protocol_version=CT_PROTOCOL_2) then begin
        If (newOperationBlock.block<CT_Protocol_Upgrade_v2_MinBlock) then begin
          errors := 'Upgrade to protocol version 2 available at block: '+IntToStr(CT_Protocol_Upgrade_v2_MinBlock);
          exit;
        end;
      end else if (newOperationBlock.protocol_version<>CT_PROTOCOL_1) then begin
        errors := 'Invalid protocol version change to '+IntToStr(newOperationBlock.protocol_version);
        exit;
      end;
    end else begin
      // If we are here protocol didn't changed... make sure it's not a upgrade block!
      if ((newOperationBlock.block = CT_Protocol_Upgrade_v2_MinBlock) and (newOperationBlock.protocol_version<>CT_PROTOCOL_2))
           or ((newOperationBlock.block = CT_Protocol_Upgrade_v3_MinBlock) and (newOperationBlock.protocol_version<>CT_PROTOCOL_3))
           or ((newOperationBlock.block = CT_Protocol_Upgrade_v4_MinBlock) and (newOperationBlock.protocol_version<>CT_PROTOCOL_4))
           or ((newOperationBlock.block = CT_Protocol_Upgrade_v5_MinBlock) and (newOperationBlock.protocol_version<>CT_PROTOCOL_5))
           or ((newOperationBlock.block = CT_Protocol_Upgrade_v6_MinBlock) and (newOperationBlock.protocol_version<>CT_PROTOCOL_6))
           then begin
         errors := Format('In block %d protocol must be upgraded! Current %d',[newOperationBlock.block,newOperationBlock.protocol_version]);
         exit;
      end;
    end;
    // timestamp
    if ((newOperationBlock.timestamp) < (lastBlock.timestamp)) then begin
      errors := 'Invalid timestamp (Back timestamp: New timestamp:'+inttostr(newOperationBlock.timestamp)+' < last timestamp ('+Inttostr(BlocksCount-1)+'):'+Inttostr(lastBlock.timestamp)+')';
      exit;
    end;
  end;
  if (isChangeTargetBlock) then begin
    target_hash := TPascalCoinProtocol.TargetFromCompact(newMinimumTargetBlock,newOperationBlock.protocol_version);
    if (newOperationBlock.compact_target <> newMinimumTargetBlock) then begin
      errors := 'Invalid target found:'+IntToHex(newOperationBlock.compact_target,8)+' actual:'+IntToHex(TPascalCoinProtocol.TargetToCompact(target_hash,newOperationBlock.protocol_version),8);
      exit;
    end;
  end else begin
    // compact_target
    target_hash:=GetActualTargetHash(newOperationBlock.protocol_version);
    if (newOperationBlock.compact_target <> TPascalCoinProtocol.TargetToCompact(target_hash,newOperationBlock.protocol_version)) then begin
      errors := 'Invalid target found:'+IntToHex(newOperationBlock.compact_target,8)+' actual:'+IntToHex(TPascalCoinProtocol.TargetToCompact(target_hash,newOperationBlock.protocol_version),8);
      exit;
    end;
  end;
  // initial_safe_box_hash: Only can be checked when adding new blocks, not when restoring a safebox
  If checkSafeBoxHash then begin
    if (Not TBaseType.Equals(newOperationBlock.initial_safe_box_hash,FSafeBoxHash)) then begin
      errors := 'BlockChain Safe box hash invalid: '+TCrypto.ToHexaString(newOperationBlock.initial_safe_box_hash)+' var: '+
        TCrypto.ToHexaString(FSafeBoxHash)+
        ' Calculated:'+TCrypto.ToHexaString(CalcSafeBoxHash);
      exit;
    end;
  end;
  if (newOperationBlock.protocol_version >= CT_PROTOCOL_5)
     And (Not TBaseType.Equals(lastBlock.proof_of_work, newOperationBlock.previous_proof_of_work)) then begin
    errors := 'Proof of work N-1 is different than newOperationBlock.previous_proof_of_work '+lastBlock.proof_of_work.ToHexaString+'<>'+newOperationBlock.previous_proof_of_work.ToHexaString;
    Exit;
  end;
  {$IFnDEF TESTING_NO_POW_CHECK}
  if (TBaseType.BinStrComp(newOperationBlock.proof_of_work,target_hash)>0) then begin
    errors := 'Proof of work is higher than target '+TCrypto.ToHexaString(newOperationBlock.proof_of_work)+' > '+TCrypto.ToHexaString(target_hash);
    exit;
  end;
  {$ENDIF}
  if checkValidOperationsBlock then begin
    Result := IsValidOperationBlock(newOperationBlock,errors);
  end else Result := True;
end;

class function TPCSafeBox.IsValidOperationBlock(const newOperationBlock: TOperationBlock; var errors: String): Boolean;
  { This class function will check a OperationBlock basic info as a valid info

    Creted at Build 2.1.7 as a division of IsValidNewOperationsBlock for easily basic check TOperationBlock

    TOperationBlock contains the info of the new block, but cannot be checked with current Safebox state
    (Use IsValidNewOperationsBlock instead) and also cannot check operations, operations_hash, fees...
  }
var pow : TRawBytes;
  i : Integer;
begin
  Result := False;
  errors := '';
  // Check Account key
  if Not TAccountComp.IsValidAccountKey(newOperationBlock.account_key,newOperationBlock.protocol_version,errors) then begin
    exit;
  end;
  // reward
  if (newOperationBlock.reward<>TPascalCoinProtocol.GetRewardForNewLine(newOperationBlock.block)) then begin
    errors := 'Invalid reward';
    exit;
  end;
  // fee: Cannot be checked only with the safebox
  // Checking valid protocol version
  // protocol available is not checked
  if (newOperationBlock.block >= CT_Protocol_Upgrade_v6_MinBlock) then begin
    if Not newOperationBlock.protocol_version = CT_PROTOCOL_6 then begin
      errors := Format('Invalid protocol version at block %d Found:%d Expected:%d',[newOperationBlock.block,newOperationBlock.protocol_version,CT_PROTOCOL_6]);
      exit;
    end;
  end else if (newOperationBlock.block >= CT_Protocol_Upgrade_v5_MinBlock) then begin
    if Not newOperationBlock.protocol_version = CT_PROTOCOL_5 then begin
      errors := Format('Invalid protocol version at block %d Found:%d Expected:%d',[newOperationBlock.block,newOperationBlock.protocol_version,CT_PROTOCOL_5]);
      exit;
    end;
  end else if (newOperationBlock.block >= CT_Protocol_Upgrade_v4_MinBlock) then begin
    if newOperationBlock.protocol_version <> CT_PROTOCOL_4 then begin
      errors := Format('Invalid protocol version at block %d Found:%d Expected:%d',[newOperationBlock.block,newOperationBlock.protocol_version,CT_PROTOCOL_4]);
      exit;
    end;
  end else if (newOperationBlock.block >= CT_Protocol_Upgrade_v3_MinBlock) then begin
    if newOperationBlock.protocol_version <> CT_PROTOCOL_3 then begin
      errors := Format('Invalid protocol version at block %d Found:%d Expected:%d',[newOperationBlock.block,newOperationBlock.protocol_version,CT_PROTOCOL_3]);
      exit;
    end;
  end else if (newOperationBlock.block >= CT_Protocol_Upgrade_v2_MinBlock) then begin
    if newOperationBlock.protocol_version <> CT_PROTOCOL_2 then begin
      errors := Format('Invalid protocol version at block %d Found:%d Expected:%d',[newOperationBlock.block,newOperationBlock.protocol_version,CT_PROTOCOL_2]);
      exit;
    end;
  end else begin
    if newOperationBlock.protocol_version <> CT_PROTOCOL_1 then begin
      errors := Format('Invalid protocol version at block %d Found:%d Expected:%d',[newOperationBlock.block,newOperationBlock.protocol_version,CT_PROTOCOL_1]);
      exit;
    end;
  end;
  if (newOperationBlock.block > 0) then begin
  end else begin
    if (CT_Zero_Block_Proof_of_work_in_Hexa<>'') then begin
      // Check if valid Zero block
      if Not (AnsiSameText(TCrypto.ToHexaString(newOperationBlock.proof_of_work),CT_Zero_Block_Proof_of_work_in_Hexa)) then begin
        errors := 'Zero block not valid, Proof of Work invalid: '+TCrypto.ToHexaString(newOperationBlock.proof_of_work)+'<>'+CT_Zero_Block_Proof_of_work_in_Hexa;
        exit;
      end;
    end;
  end;
  // Checking Miner Payload valid chars/length
  If Not TPascalCoinProtocol.IsValidMinerBlockPayload(newOperationBlock.block_payload) then begin
    errors := 'Invalid Miner Payload value. Length: '+inttostr(Length(newOperationBlock.block_payload));
    exit;
  end;
  // operations_hash: NOT CHECKED WITH OPERATIONS!
  If (length(newOperationBlock.operations_hash)<>32) then begin
    errors := 'Invalid Operations hash value: '+TCrypto.ToHexaString(newOperationBlock.operations_hash)+' length='+IntToStr(Length(newOperationBlock.operations_hash));
    exit;
  end;
  // proof_of_work:
  {$IFnDEF TESTING_NO_POW_CHECK}
  {$IFDEF ASSUME_VALID_POW_OLD_PROTOCOLS}
  if (newOperationBlock.protocol_version>=CT_PROTOCOL_5) then begin
  {$ENDIF}
  TPascalCoinProtocol.CalcProofOfWork(newOperationBlock,pow);
  if (Not TBaseType.Equals(pow,newOperationBlock.proof_of_work)) then begin
    errors := 'Proof of work is bad calculated '+TCrypto.ToHexaString(newOperationBlock.proof_of_work)+' <> Good: '+TCrypto.ToHexaString(pow);
    exit;
  end;
  {$IFDEF ASSUME_VALID_POW_OLD_PROTOCOLS}
  end;
  {$ENDIF}

  {$ENDIF}
  Result := true;
end;

function TPCSafeBox.GetActualTargetHash(protocolVersion : Word): TRawBytes;
{ Target is calculated in each block with avg obtained in previous
  CT_CalcNewDifficulty blocks.
  If Block is lower than CT_CalcNewDifficulty then is calculated
  with all previous blocks.
}
Var ts1, ts2, tsTeorical, tsReal, tsTeoricalStop, tsRealStop: Int64;
  CalcBack : Integer;
  lastBlock : TOperationBlock;
begin
  if (BlocksCount <= 1) then begin
    // Important: CT_MinCompactTarget is applied for blocks 0 until ((CT_CalcNewDifficulty*2)-1)
    Result := TPascalCoinProtocol.TargetFromCompact(CT_MinCompactTarget_v1,CT_PROTOCOL_1);
  end else begin
    if BlocksCount > CT_CalcNewTargetBlocksAverage then CalcBack := CT_CalcNewTargetBlocksAverage
    else CalcBack := BlocksCount-1;
    lastBlock := GetBlockInfo(BlocksCount-1);
    // Calc new target!
    ts1 := lastBlock.timestamp;
    ts2 := GetBlockInfo(BlocksCount-CalcBack-1).timestamp;
    tsTeorical := (CalcBack * CT_NewLineSecondsAvg);
    tsReal := (ts1 - ts2);
    If (protocolVersion=CT_PROTOCOL_1) then begin
      Result := TPascalCoinProtocol.GetNewTarget(tsTeorical, tsReal,protocolVersion,False,TPascalCoinProtocol.TargetFromCompact(lastBlock.compact_target,lastBlock.protocol_version));
    end else if (protocolVersion<=CT_PROTOCOL_MAX) then begin
      CalcBack := CalcBack DIV CT_CalcNewTargetLimitChange_SPLIT;
      If CalcBack<=0 then CalcBack := 1;
      ts2 := GetBlockInfo(BlocksCount-CalcBack-1).timestamp;
      tsTeoricalStop := (CalcBack * CT_NewLineSecondsAvg);
      tsRealStop := (ts1 - ts2);
      { Protocol 2 change:
        Only will increase/decrease Target if (CT_CalcNewTargetBlocksAverage DIV 10) needs to increase/decrease too, othewise use
        current Target.
        This will prevent sinusoidal movement and provide more stable hashrate, computing always time from CT_CalcNewTargetBlocksAverage }
      If ((tsTeorical>tsReal) and (tsTeoricalStop>tsRealStop))
         Or
         ((tsTeorical<tsReal) and (tsTeoricalStop<tsRealStop)) then begin
        Result := TPascalCoinProtocol.GetNewTarget(tsTeorical, tsReal,protocolVersion,False,TPascalCoinProtocol.TargetFromCompact(lastBlock.compact_target,lastBlock.protocol_version));
      end else begin
        if (protocolVersion=CT_PROTOCOL_2) then begin
          // Nothing to do!
          Result:=TPascalCoinProtocol.TargetFromCompact(lastBlock.compact_target,lastBlock.protocol_version);
        end else begin
          // New on V3 protocol:
          // Harmonization of the sinusoidal effect modifying the rise / fall over the "stop" area
          Result := TPascalCoinProtocol.GetNewTarget(tsTeoricalStop,tsRealStop,protocolVersion,True,TPascalCoinProtocol.TargetFromCompact(lastBlock.compact_target,lastBlock.protocol_version));
        end;
      end;
    end else begin
      Raise Exception.Create('ERROR DEV 20180306-1 Protocol not valid: '+IntToStr(protocolVersion));
    end;
  end;
end;

function TPCSafeBox.GetAggregatedHashrate: TBigNum;
begin
  {$IFDEF USE_ABSTRACTMEM}
  Result := FPCAbstractMem.AggregatedHashrate;
  {$ELSE}
  Result := FAggregatedHashrate;
  {$ENDIF}
end;

procedure TPCSafeBox.GetAggregatedHashrateOnBlock(ABlockNumber: Cardinal; const AAggregatedHashrate: TBigNum);
var i : Integer;
  LHashRate : TBigNum;
begin
  AAggregatedHashrate.Value := 0; // Set to zero
  if ABlockNumber>=BlocksCount then raise Exception.Create(Format('BlockNumber %d out of range (%d / %d)',[ABlockNumber,0,BlocksCount]));

  if (BlocksCount DIV 2) < ABlockNumber then begin
    // decrease mode
    AAggregatedHashrate.RawValue := AggregatedHashrate.RawValue;
    for i := Integer(BlocksCount)-1 downto (ABlockNumber+1) do begin
      LHashRate := TBigNum.TargetToHashRate( GetBlockInfo(i).compact_target );
      try
        AAggregatedHashrate.Sub( LHashRate );
      finally
        LHashRate.Free;
      end;
    end;
  end else begin
    // Increase mode
    for i := 0 to ABlockNumber do begin
      LHashRate := TBigNum.TargetToHashRate( GetBlockInfo(i).compact_target );
      try
        AAggregatedHashrate.Add( LHashRate );
      finally
        LHashRate.Free;
      end;
    end;
  end;

end;

function TPCSafeBox.GetBlockInfo(ABlockNumber: Cardinal): TOperationBlock;
var LBlock : TBlockAccount;
begin
  StartThreadSafe;
  try
    If (Assigned(FPreviousSafeBox)) then begin
      if (ABlockNumber<0) Or (ABlockNumber>=BlocksCount) then raise Exception.Create('Invalid block number for GetBlockInfo chain: '+inttostr(ABlockNumber)+' max: '+IntToStr(BlocksCount-1));
      SearchBlockWhenOnSeparatedChain(ABlockNumber,LBlock);
      Result := LBlock.blockchainInfo.GetCopy;
    end else begin
      {$IFDEF USE_ABSTRACTMEM}
      if (ABlockNumber<0) Or (ABlockNumber>=FPCAbstractMem.BlocksCount) then raise Exception.Create('Invalid GetBlockInfo block number: '+inttostr(ABlockNumber)+' max: '+IntToStr(FPCAbstractMem.BlocksCount-1));
      Result := FPCAbstractMem.GetBlockInfo(ABlockNumber).operationBlock.GetCopy;
      {$ELSE}
      if (ABlockNumber<0) Or (ABlockNumber>=FBlockAccountsList.Count) then raise Exception.Create('Invalid GetBlockInfo block number: '+inttostr(ABlockNumber)+' max: '+IntToStr(FBlockAccountsList.Count-1));
      ToTBlockAccount(PBlockAccount(FBlockAccountsList.Items[ABlockNumber])^,ABlockNumber,LBlock);
      Result := LBlock.blockchainInfo;
      {$ENDIF}
    end;
  finally
    EndThreadSave;
  end;
end;

function TPCSafeBox.GetAccount(AAccountNumber: Integer; var AAccount: TAccount): Boolean;
begin
  AAccount := Account(AAccountNumber).GetCopy;
  Result := True;
end;

function TPCSafeBox.GetActualCompactTargetHash(protocolVersion : Word): Cardinal;
begin
  Result := TPascalCoinProtocol.TargetToCompact(GetActualTargetHash(protocolVersion),protocolVersion);
end;

function TPCSafeBox.FindAccountByName(const aName: String): Integer;
begin
  Result := FindAccountByName(TEncoding.ASCII.GetBytes(LowerCase(aName)));
end;

function TPCSafeBox.FindAccountByName(const aName: TRawBytes): Integer;
Var i,j,k : Integer;
  Psnapshot : PSafeboxSnapshot;
  {$IFDEF USE_ABSTRACTMEM}
  Laninfo : TAccountNameInfo;
  {$ENDIF}
begin
  {$IFDEF USE_ABSTRACTMEM}
  if FPCAbstractMem.AccountsNames.FindByName(aName.ToString,Laninfo) then
    Result := Laninfo.accountNumber
  {$ELSE}
  i := FOrderedByName.IndexOf(aName);
  if i>=0 then Result := FOrderedByName.GetTag(i)
  {$ENDIF}
  else begin
    Result := -1;
    If Assigned(FPreviousSafeBox) then begin
      // Now doesn't exists, was deleted before?
      Result := FPreviousSafeBox.FindAccountByName(aName);
      j := FPreviousSafeBox.FSnapshots.Count-1;
      // Start with current changes on FPreviousSafebox
      // Start with Added
      If (Result>=0) then begin
        k := FPreviousSafeBox.FAddedNamesSincePreviousSafebox.IndexOf(aName);
        If (k>=0) then Result := -1;
      end;
      // Then with deleted
      If (Result<0) then begin
        // I've not found nameLower, search if was deleted
        k := (FPreviousSafeBox.FDeletedNamesSincePreviousSafebox.IndexOf(aName));
        If (k>=0) then begin
          // Was deleted, rescue previous account number with name
          Result := FPreviousSafeBox.FDeletedNamesSincePreviousSafebox.GetTag(k);
        end;
      end;
      //
      while (j>=0) And (PSafeboxSnapshot(FPreviousSafeBox.FSnapshots[j])^.nBlockNumber>FPreviousSafeboxOriginBlock) do begin //  > ????
        Psnapshot := PSafeboxSnapshot(FPreviousSafeBox.FSnapshots[j]);
        // Start with added:
        If (Result>=0) then begin
          // I've found nameLower, search if was added (to undo)
          k := (Psnapshot^.namesAdded.IndexOf(aName));
          if (k>=0) then begin
            // Was addded, delete name
            Result := -1;
          end;
        end;
        // Then with deleted (in order to restore)
        If (Result<0) then begin
          // I've not found nameLower, search if was deleted
          k := (Psnapshot^.namesDeleted.IndexOf(aName));
          If (k>=0) then begin
            // Was deleted, rescue previous account number with name
            Result := Psnapshot^.namesDeleted.GetTag(k);
          end;
        end;
        dec(j); // Next previous snapshot
      end;
    end;
  end;
end;

function TPCSafeBox.FindAccountsStartingByName(const AStartName: TRawBytes;
  const ARawList: TOrderedRawList; const AMax: Integer = 0): Integer;
var
  LRaw : TRawBytes;
  {$IFDEF USE_ABSTRACTMEM}
  Laninfo : TAccountNameInfo;
  {$ELSE}
  LIndex : Integer;
  {$ENDIF}
begin
  ARawList.Clear;
  StartThreadSafe;
  try
    {$IFDEF USE_ABSTRACTMEM}
    FPCAbstractMem.AccountsNames.FindByName(AStartName.ToString,Laninfo);
    while (Laninfo.accountName.StartsWith(AStartName.ToString))
      and ((AMax<=0) or (AMax>ARawList.Count)) do begin
      LRaw.FromString(Laninfo.accountName);
      ARawList.Add( LRaw, Laninfo.accountNumber );
      if not FPCAbstractMem.AccountsNames.FindDataSuccessor(Laninfo,Laninfo) then Break;
    end;
    {$ELSE}
    if FOrderedByName.Find(AStartName,LIndex) then begin
      ARawList.Add( FOrderedByName.Get(LIndex), FOrderedByName.GetTag(LIndex) );
      inc(LIndex);
    end;
    while (LIndex<FOrderedByName.Count) and (TBaseType.StartsWith(AStartName,FOrderedByName.Get(LIndex)))
      and ((AMax<=0) or (AMax>ARawList.Count)) // AMax <=0 inifinte results
      do begin
      ARawList.Add( FOrderedByName.Get(LIndex), FOrderedByName.GetTag(LIndex) );
      inc(LIndex);
    end;
    {$ENDIF}

    Result := ARawList.Count;
  finally
    EndThreadSave;
  end;
end;

procedure TPCSafeBox.SearchBlockWhenOnSeparatedChain(blockNumber: Cardinal; out blockAccount: TBlockAccount);
  Function WasUpdatedBeforeOrigin : Boolean;
  var j, maxUB : Integer;
  Begin
    // Is valid?
    maxUB := 0;
    for j:=Low(blockAccount.accounts) to High(blockAccount.accounts) do begin
      If blockAccount.accounts[j].GetLastUpdatedBlock>maxUB then maxUB := blockAccount.accounts[j].GetLastUpdatedBlock;
    end;
    Result := (maxUB <= FPreviousSafeboxOriginBlock);
  end;
var i,j : Integer;
  Pss : PSafeboxSnapshot;
begin
  If Not Assigned(FPreviousSafeBox) then Raise Exception.Create('ERROR DEV 20180320-1');
  // It's not stored on FBlockAccountsList
  // Search on my chain current chain
  If FModifiedBlocksSeparatedChain.Find(blockNumber,i) then begin
    blockAccount := FModifiedBlocksSeparatedChain.Get(i);
    Exit;
  end else begin
    // Has not changed on my chain, must search on PreviousSafebox chain AT OriginStartPos
    blockAccount := FPreviousSafeBox.GetBlock(blockNumber);
    // Is valid?
    If WasUpdatedBeforeOrigin then Exit;
    //
    If FPreviousSafeBox.FModifiedBlocksPreviousState.Find(blockNumber,j) then begin
      blockAccount := FPreviousSafeBox.FModifiedBlocksPreviousState.Get(j);
      if WasUpdatedBeforeOrigin then Exit;
    end;

    // Must search on Previous when was updated!
    i := FPreviousSafeBox.FSnapshots.Count-1;
    while (i>=0) do begin
      Pss := FPreviousSafeBox.FSnapshots[i];
      If Pss.oldBlocks.Find(blockNumber,j) then begin
        blockAccount := Pss.oldBlocks.Get(j);
        If WasUpdatedBeforeOrigin then Exit;
      end;
      dec(i);
    end;
    Raise Exception.Create('ERROR DEV 20180318-1'); // Must find before!
  end;
end;

procedure TPCSafeBox.UpdateAccount(account_number : Cardinal; const newAccountInfo: TAccountInfo;
  const newName : TRawBytes; newType : Word; newBalance: UInt64; newN_operation: Cardinal;
  const newAccountData, newAccountSeal : TRawBytes;
  accountUpdateStyle : TAccountUpdateStyle; newUpdated_block_pasive_mode, newUpdated_block_active_mode : Cardinal;
  AHasBenUpdatedOnActiveMode, AHasBenUpdatedOnPasiveMode : Boolean);
Var iBlock : Cardinal;
  i,j,iAccount, iDeleted, iAdded : Integer;
  lastbalance : UInt64;
  blockAccount : TBlockAccount;
  {$IFnDEF USE_ABSTRACTMEM}
  Pblock : PBlockAccount;
  {$ELSE}
  Laninfo : TAccountNameInfo;
  {$ENDIF}
begin
  iBlock := account_number DIV CT_AccountsPerBlock;
  iAccount := account_number MOD CT_AccountsPerBlock;

  blockAccount := GetBlock(iBlock);
  FModifiedBlocksPreviousState.AddIfNotExists(blockAccount);
  {$IFnDEF USE_ABSTRACTMEM}
  If Assigned(FPreviousSafeBox) then begin
    Pblock := Nil;
  end else begin
    Pblock := FBlockAccountsList.Items[iBlock];
  end;
  FAccountsOrderedByUpdatedBlock.Update(
    account_number,
    blockAccount.accounts[iAccount].updated_on_block_active_mode,
    newUpdated_block_active_mode
   );
  FAccountsOrderedBySalePrice.UpdateAccountBySalePrice(
    account_number,
    blockAccount.accounts[iAccount].accountInfo,
    newAccountInfo
   );
  {$ENDIF}

  if (NOT TAccountComp.EqualAccountKeys(blockAccount.accounts[iAccount].accountInfo.accountKey,newAccountInfo.accountKey)) then begin
    AccountKeyListRemoveAccount(blockAccount.accounts[iAccount].accountInfo.accountKey,[account_number]);
    AccountKeyListAddAccounts(newAccountInfo.accountKey,[account_number]);
  end;

  {$IFDEF useAccountKeyStorage}
  // Delete old references prior to change
  TAccountKeyStorage.KS.RemoveAccountKey(blockAccount.accounts[iAccount].accountInfo.accountKey);
  TAccountKeyStorage.KS.RemoveAccountKey(blockAccount.accounts[iAccount].accountInfo.new_publicKey);
  {$ENDIF}

  blockAccount.accounts[iAccount].accountInfo := newAccountInfo;
  blockAccount.accounts[iAccount].account_type:=newType;
  lastbalance := blockAccount.accounts[iAccount].balance;
  blockAccount.accounts[iAccount].balance := newBalance;
  blockAccount.accounts[iAccount].n_operation := newN_operation;

  If (accountUpdateStyle In [aus_rollback,aus_commiting_from_otherchain]) then begin
    // Directly update name and updated values
    blockAccount.accounts[iAccount].name:=newName;
    blockAccount.accounts[iAccount].updated_on_block_passive_mode:=newUpdated_block_pasive_mode;
    blockAccount.accounts[iAccount].updated_on_block_active_mode:=newUpdated_block_active_mode;
  end else begin
    // Name:
    If Not TBaseType.Equals(blockAccount.accounts[iAccount].name,newName) then begin
      If Length(blockAccount.accounts[iAccount].name)>0 then begin

        {$IFDEF USE_ABSTRACTMEM}
        if Not FPCAbstractMem.AccountsNames.FindByName(blockAccount.accounts[iAccount].name.ToString,Laninfo) then begin
          If (Not Assigned(FPreviousSafeBox)) then begin
            TLog.NewLog(ltError,ClassName,'ERROR DEV 20170606-1 Name "'+blockAccount.accounts[iAccount].name.ToPrintable+'" not found for delete on account '+IntToStr(account_number));
          end;
        end else begin
          If (Laninfo.accountNumber<>account_number) then begin
            TLog.NewLog(ltError,ClassName,'ERROR DEV 20170606-3 Name "'+blockAccount.accounts[iAccount].name.ToPrintable+'" not found for delete at suposed account '+IntToStr(account_number)+' found at '+IntToStr(Laninfo.accountNumber)+' '+Laninfo.accountName);
          end;
          FPCAbstractMem.AccountsNames.DeleteData(Laninfo);
        end;
        {$ELSE}
        i := FOrderedByName.IndexOf(blockAccount.accounts[iAccount].name);
        if i<0 then begin
          If (Not Assigned(FPreviousSafeBox)) then begin
            TLog.NewLog(ltError,ClassName,'ERROR DEV 20170606-1 Name "'+blockAccount.accounts[iAccount].name.ToPrintable+'" not found for delete on account '+IntToStr(account_number));
          end;
        end else begin
          If (FOrderedByName.GetTag(i)<>account_number) then begin
            TLog.NewLog(ltError,ClassName,'ERROR DEV 20170606-3 Name "'+blockAccount.accounts[iAccount].name.ToPrintable+'" not found for delete at suposed account '+IntToStr(account_number)+' found at '+IntToStr(FOrderedByName.GetTag(i)));
          end;
          FOrderedByName.Delete(i);
        end;
        {$ENDIF}

        iDeleted := FDeletedNamesSincePreviousSafebox.IndexOf(blockAccount.accounts[iAccount].name);
        iAdded := FAddedNamesSincePreviousSafebox.IndexOf(blockAccount.accounts[iAccount].name);

        If (iDeleted<0) then begin
          If (iAdded<0) then begin
            {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,ClassName,Format('Deleted from PREVIOUS snapshot name:%s at account:%d',[blockAccount.accounts[iAccount].name.ToPrintable,account_number]));{$ENDIF}
            FDeletedNamesSincePreviousSafebox.Add(blockAccount.accounts[iAccount].name,account_number); // Very important to store account_number in order to restore a snapshot!
          end else begin
            // Was added, so delete from added
            {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,ClassName,Format('Deleted from current snapshot name:%s at account:%d',[blockAccount.accounts[iAccount].name.ToPrintable,account_number]));{$ENDIF}
            FAddedNamesSincePreviousSafebox.Delete(iAdded);
          end;
        end else begin
          // Was deleted before, delete from added
          If (iAdded>=0) then begin
            FAddedNamesSincePreviousSafebox.Delete(iAdded);
          end;
        end;
      end;
      blockAccount.accounts[iAccount].name:=newName;
      If Length(blockAccount.accounts[iAccount].name)>0 then begin
        {$IFDEF USE_ABSTRACTMEM}
        if FPCAbstractMem.AccountsNames.FindByName(blockAccount.accounts[iAccount].name.ToString,Laninfo) then begin
          TLog.NewLog(ltError,ClassName,'ERROR DEV 20170606-2 New Name "'+blockAccount.accounts[iAccount].name.ToPrintable+'" for account '+IntToStr(account_number)+' found at account '+IntToStr(Laninfo.accountNumber));
          FPCAbstractMem.AccountsNames.DeleteData(Laninfo);
        end;
        FPCAbstractMem.AccountsNames.AddNameAndNumber(blockAccount.accounts[iAccount].name.ToString,account_number);
        {$ELSE}
        i := FOrderedByName.IndexOf(blockAccount.accounts[iAccount].name);
        if i>=0 then TLog.NewLog(ltError,ClassName,'ERROR DEV 20170606-2 New Name "'+blockAccount.accounts[iAccount].name.ToPrintable+'" for account '+IntToStr(account_number)+' found at account '+IntToStr(FOrderedByName.GetTag(i)));
        FOrderedByName.Add(blockAccount.accounts[iAccount].name,account_number);
        {$ENDIF}

        iDeleted := FDeletedNamesSincePreviousSafebox.IndexOf(blockAccount.accounts[iAccount].name);
        iAdded := FAddedNamesSincePreviousSafebox.IndexOf(blockAccount.accounts[iAccount].name);

        // Adding
        If (iDeleted>=0) Then begin
          if (FDeletedNamesSincePreviousSafebox.GetTag(iDeleted)=account_number) then begin
            // Is restoring to initial position, delete from deleted
            {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,ClassName,Format('Adding equal to PREVIOUS (DELETING FROM DELETED) snapshot name:%s at account:%d',[blockAccount.accounts[iAccount].name.ToPrintable,account_number]));{$ENDIF}
            FDeletedNamesSincePreviousSafebox.Delete(iDeleted);
            if iAdded>=0 then FAddedNamesSincePreviousSafebox.Remove(blockAccount.accounts[iAccount].name);
          end else begin
            // Was deleted, but now adding to a new account
            {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,ClassName,Format('Adding again name:%s to new account account:%d',[blockAccount.accounts[iAccount].name.ToPrintable,account_number]));{$ENDIF}
            FAddedNamesSincePreviousSafebox.Add(blockAccount.accounts[iAccount].name,account_number);
          end;
        end else begin
          // Was not deleted, Add it
          {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,ClassName,Format('Adding first time at this snapshot name:%s at account:%d',[blockAccount.accounts[iAccount].name.ToPrintable,account_number]));{$ENDIF}
          FAddedNamesSincePreviousSafebox.Add(blockAccount.accounts[iAccount].name,account_number);
        end;
      end;
    end;
    if CurrentProtocol < CT_PROTOCOL_5 then begin
      // On protocol 1..4 the "updated_on_block" was a single value without active/pasive distinction
      // so it will be stored at "updated_on_block_passive_mode" always
      blockAccount.accounts[iAccount].updated_on_block_active_mode := BlocksCount;
      blockAccount.accounts[iAccount].updated_on_block_passive_mode := BlocksCount;
    end;
    if (AHasBenUpdatedOnActiveMode) then begin
      // This flag will indicate that this account has been used as ACTIVE MODE so needs to update on which block was updated
      blockAccount.accounts[iAccount].updated_on_block_active_mode := BlocksCount;
    end;
    if (AHasBenUpdatedOnPasiveMode) then begin
      blockAccount.accounts[iAccount].updated_on_block_passive_mode := BlocksCount;
    end;
  end;

  // New Protocol 5 fields
  blockAccount.accounts[iAccount].account_data := newAccountData;
  blockAccount.accounts[iAccount].account_seal := newAccountSeal;

  // Save new account values
  blockAccount.block_hash:=CalcBlockHash(blockAccount,FCurrentProtocol);
  FModifiedBlocksFinalState.Add(blockAccount);
  If Assigned(FPreviousSafeBox) then begin
    FModifiedBlocksSeparatedChain.Add(blockAccount);
  end;
  {$IFDEF USE_ABSTRACTMEM}
  If Not Assigned(FPreviousSafeBox) then begin
    FPCAbstractMem.SetAccount( blockAccount.accounts[iAccount] );
  end;
  {$ELSE}
  If (Assigned(Pblock)) then begin
    ToTMemAccount(blockAccount.accounts[iAccount],Pblock^.accounts[iAccount]);
    {$IFDEF uselowmem}
    TBaseType.To32Bytes(blockAccount.block_hash,Pblock^.block_hash);
    {$ELSE}
    Pblock^.block_hash := blockAccount.block_hash;
    {$ENDIF}
  end;
  {$ENDIF}
  // Update buffer block hash
  j := (Length(blockAccount.block_hash)*(iBlock));  // j in 0,32,64...
  BufferBlocksHash.Replace(j,blockAccount.block_hash[0],32);
  FTotalBalance := FTotalBalance - (Int64(lastbalance)-Int64(newBalance));
  FTotalFee := FTotalFee + (Int64(lastbalance)-Int64(newBalance));
end;

procedure TPCSafeBox.StartThreadSafe;
begin
  TPCThread.ProtectEnterCriticalSection(Self,FLock);
end;

{ TPCSafeBoxTransaction }

function TPCSafeBoxTransaction.Account(account_number: Cardinal): TAccount;
Var i :Integer;
begin
  if FOrderedList.Find(account_number,i) then Result := PSealedAccount(FOrderedList.FList[i])^.AccountSealed^
  else begin
    Result := FreezedSafeBox.Account(account_number);
  end;
end;

function TPCSafeBoxTransaction.BuyAccount(APrevious : TAccountPreviousBlockInfo; const AOpID : TRawBytes;  ABuyer, AAccountToBuy, ASeller: Cardinal; ANOperation: Cardinal; AAmount, AAccountPrice, AFee: UInt64;  const ANewAccountKey: TAccountKey; const AHashLockKey : TRawBytes; ARecipientSigned : Boolean; var AErrors: String): Boolean;
var
  LPBuyerAccount, LPAccountToBuy, LPSellerAccount : PAccount;
  LPBuyerAccount_Sealed, LPAccountToBuy_Sealed, LPSellerAccount_Sealed : PSealedAccount;
begin
  Result := false;
  AErrors := '';
  if not CheckIntegrity then begin
    AErrors := 'Invalid integrity in accounts transaction';
    exit;
  end;
  if (ABuyer<0) Or (ABuyer>=(Origin_BlocksCount*CT_AccountsPerBlock)) Or
     (AAccountToBuy<0) Or (AAccountToBuy>=(Origin_BlocksCount*CT_AccountsPerBlock)) Or
     (ASeller<0) Or (ASeller>=(Origin_BlocksCount*CT_AccountsPerBlock)) then begin
     AErrors := 'Invalid account number on buy';
     exit;
  end;
  if TAccountComp.IsAccountBlockedByProtocol(ABuyer,Origin_BlocksCount) then begin
    AErrors := 'Buyer account is blocked for protocol';
    Exit;
  end;
  if TAccountComp.IsAccountBlockedByProtocol(AAccountToBuy,Origin_BlocksCount) then begin
    AErrors := 'Account to buy is blocked for protocol';
    Exit;
  end;
  if TAccountComp.IsAccountBlockedByProtocol(ASeller,Origin_BlocksCount) then begin
    AErrors := 'Seller account is blocked for protocol';
    Exit;
  end;
  LPBuyerAccount := GetInternalAccount(ABuyer,LPBuyerAccount_Sealed);
  LPAccountToBuy := GetInternalAccount(AAccountToBuy,LPAccountToBuy_Sealed);
  LPSellerAccount := GetInternalAccount(ASeller,LPSellerAccount_Sealed);
  if (LPBuyerAccount^.n_operation+1<>ANOperation) then begin
    AErrors := 'Incorrect n_operation';
    Exit;
  end;
  if (LPBuyerAccount^.balance < (AAmount+AFee)) then begin
    AErrors := 'Insufficient Funds';
    Exit;
  end;
  if (AFee>CT_MaxTransactionFee) then begin
    AErrors := 'Max fee';
    Exit;
  end;
  if (TAccountComp.IsAccountLocked(LPBuyerAccount^.accountInfo,Origin_BlocksCount) AND (NOT ARecipientSigned)) then begin
    AErrors := 'Buyer account is locked until block '+Inttostr(LPBuyerAccount^.accountInfo.locked_until_block);
    Exit;
  end;
  If not (TAccountComp.IsAccountForSaleOrSwap(LPAccountToBuy^.accountInfo)) then begin
    AErrors := 'Account is not for sale or swap';
    Exit;
  end;
  if (LPAccountToBuy^.accountInfo.new_publicKey.EC_OpenSSL_NID<>CT_TECDSA_Public_Nul.EC_OpenSSL_NID) And
     (Not TAccountComp.EqualAccountKeys(LPAccountToBuy^.accountInfo.new_publicKey,ANewAccountKey)) then begin
    AErrors := 'New public key is not equal to allowed new public key for account';
    Exit;
  end;
  // Buy an account applies when account_to_buy.amount + operation amount >= price
  // Then, account_to_buy.amount will be (account_to_buy.amount + amount - price)
  // and buyer.amount will be buyer.amount + price
  if (LPAccountToBuy^.accountInfo.price > (LPAccountToBuy^.balance+AAmount)) then begin
    AErrors := 'Account price '+TAccountComp.FormatMoney(LPAccountToBuy^.accountInfo.price)+' < balance '+
      TAccountComp.FormatMoney(LPAccountToBuy^.balance)+' + amount '+TAccountComp.FormatMoney(AAmount);
    Exit;
  end;
  if TAccountComp.IsAccountForSwap(LPAccountToBuy^.accountInfo) AND (NOT TAccountComp.IsValidAccountInfoHashLockKey(LPAccountToBuy^.accountInfo, AHashLockKey)) then begin
    AErrors := 'Account is not unlocked by supplied hash lock key';
    Exit;
  end;


  // Overflow checks:
  if AAmount > (AAmount + AFee) then begin
    AErrors := 'Critical overflow error detected, aborting SafeBox update. Ref: 37E7343143614D4C8489FA9963CE8C3C';
    exit;
  end;
  if LPBuyerAccount^.balance < (AAmount + AFee) then begin
    AErrors := 'Critical overflow error detected, aborting SafeBox update. Ref: F06169D9A209410AACB1AAD324B7A191';
    exit;
  end;

  if LPAccountToBuy^.balance > (LPAccountToBuy^.balance + AAmount) then begin
    AErrors := 'Critical overflow error detected, aborting SafeBox update. Ref: 390BB1E7241B4A0BA5A2D934E67F39D3';
    exit;
  end;
  if (LPAccountToBuy^.balance + AAmount) < (LPAccountToBuy^.accountInfo.price) then begin
    AErrors := 'Critical overflow error detected, aborting SafeBox update. Ref: 19D80241DA584D0B93ED30F27110B9A9';
    exit;
  end;

  if LPSellerAccount^.balance > (LPSellerAccount^.balance + LPAccountToBuy^.accountInfo.price) then begin
    AErrors := 'Critical overflow error detected, aborting SafeBox update. Ref: 972CA85C6E4E4081ABB767F6D8019421';
    exit;
  end;


  // NOTE:
  // At this point, we have checked integrity, cannot check later!

  APrevious.UpdateIfLower(LPBuyerAccount^.account,LPBuyerAccount^.GetLastUpdatedBlock);
  APrevious.UpdateIfLower(LPAccountToBuy^.account,LPAccountToBuy^.GetLastUpdatedBlock);
  APrevious.UpdateIfLower(LPSellerAccount^.account,LPSellerAccount^.GetLastUpdatedBlock);

  UpdateSealAndActiveModeFlag(LPBuyerAccount_Sealed,AOpID,True);  // Only the buyer account is the Active account
  UpdateSealAndActiveModeFlag(LPAccountToBuy_Sealed,AOpID,False);
  UpdateSealAndActiveModeFlag(LPSellerAccount_Sealed,AOpID,False);

  // Inc buyer n_operation
  LPBuyerAccount^.n_operation := ANOperation;
  // Set new balance values
  LPBuyerAccount^.balance := LPBuyerAccount^.balance - (AAmount + AFee);
  LPAccountToBuy^.balance := LPAccountToBuy^.balance + AAmount - LPAccountToBuy^.accountInfo.price;
  LPSellerAccount^.balance := LPSellerAccount^.balance + LPAccountToBuy^.accountInfo.price;

  // After buy, account will be unlocked and set to normal state and new account public key changed
  LPAccountToBuy^.accountInfo := CT_AccountInfo_NUL;
  LPAccountToBuy^.accountInfo.state := as_Normal;
  LPAccountToBuy^.accountInfo.accountKey := ANewAccountKey;

  Dec(FTotalBalance,Int64(AFee));
  inc(FTotalFee,Int64(AFee));
  Result := true;
end;

function TPCSafeBoxTransaction.CheckIntegrity: Boolean;
begin
  Result := TBaseType.Equals(FOldSafeBoxHash,Origin_SafeboxHash);
end;

procedure TPCSafeBoxTransaction.CleanTransaction;
begin
  FOrderedList.Clear;
  FOldSafeBoxHash := Origin_SafeboxHash;
  FTotalBalance := Origin_TotalBalance;
  FTotalFee := 0;
  FAccountNames_Added.Clear;
  FAccountNames_Deleted.Clear;
end;

function TPCSafeBoxTransaction.Commit(const operationBlock: TOperationBlock;
  var errors: String): Boolean;
Var i : Integer;
  Pa : PAccount;
  PSealed : PSealedAccount;
begin
  Result := false;
  errors := '';
  FFreezedAccounts.StartThreadSafe;
  try
    if not CheckIntegrity then begin
      errors := 'Invalid integrity in accounts transaction on commit';
      exit;
    end;
    for i := 0 to FOrderedList.FList.Count - 1 do begin
      PSealed := PSealedAccount(FOrderedList.FList[i]);
      Pa := PSealed^.AccountSealed;
      FFreezedAccounts.UpdateAccount(Pa^.account,
            Pa^.accountInfo,
            Pa^.name,
            Pa^.account_type,
            Pa^.balance,
            Pa^.n_operation,
            Pa^.account_data,
            Pa^.account_seal,
            aus_transaction_commit,
            Pa^.updated_on_block_passive_mode,
            Pa^.updated_on_block_active_mode,
            PSealed^.UsedAsActiveMode,
            PSealed^.UsedAsPasiveMode);
    end;
    //
    if (Origin_TotalBalance<>FTotalBalance) then begin
      TLog.NewLog(lterror,ClassName,Format('Invalid integrity balance! StrongBox:%d Transaction:%d',[Origin_TotalBalance,FTotalBalance]));
    end;
    if (Origin_TotalFee<>FTotalFee) then begin
      TLog.NewLog(lterror,ClassName,Format('Invalid integrity fee! StrongBox:%d Transaction:%d',[Origin_TotalFee,FTotalFee]));
    end;
    FFreezedAccounts.AddNew(operationBlock);
    CleanTransaction;
    //
    if (FFreezedAccounts.FCurrentProtocol<CT_PROTOCOL_2) And (operationBlock.protocol_version=CT_PROTOCOL_2) then begin
      // First block with new protocol!
      if FFreezedAccounts.CanUpgradeToProtocol(CT_PROTOCOL_2) then begin
        TLog.NewLog(ltInfo,ClassName,'Protocol upgrade to v2');
        If not FFreezedAccounts.DoUpgradeToProtocol2 then begin
          raise Exception.Create('Cannot upgrade to protocol v2 !');
        end;
      end;
    end;
    if (FFreezedAccounts.FCurrentProtocol<CT_PROTOCOL_3) And (operationBlock.protocol_version=CT_PROTOCOL_3) then begin
      // First block with V3 protocol
      if FFreezedAccounts.CanUpgradeToProtocol(CT_PROTOCOL_3) then begin
        TLog.NewLog(ltInfo,ClassName,'Protocol upgrade to v3');
        If not FFreezedAccounts.DoUpgradeToProtocol3 then begin
          raise Exception.Create('Cannot upgrade to protocol v3 !');
        end;
      end;
    end;
    if (FFreezedAccounts.FCurrentProtocol<CT_PROTOCOL_4) And (operationBlock.protocol_version=CT_PROTOCOL_4) then begin
      // First block with V4 protocol
      if FFreezedAccounts.CanUpgradeToProtocol(CT_PROTOCOL_4) then begin
        TLog.NewLog(ltInfo,ClassName,'Protocol upgrade to v4');
        If not FFreezedAccounts.DoUpgradeToProtocol4 then begin
          raise Exception.Create('Cannot upgrade to protocol v4 !');
        end;
      end;
    end;
    if (FFreezedAccounts.FCurrentProtocol<CT_PROTOCOL_5) And (operationBlock.protocol_version=CT_PROTOCOL_5) then begin
      // First block with V5 protocol
      if FFreezedAccounts.CanUpgradeToProtocol(CT_PROTOCOL_5) then begin
        TLog.NewLog(ltInfo,ClassName,'Protocol upgrade to v5');
        If not FFreezedAccounts.DoUpgradeToProtocol5 then begin
          raise Exception.Create('Cannot upgrade to protocol v5 !');
        end;
      end;
    end;
    if (FFreezedAccounts.FCurrentProtocol<CT_PROTOCOL_6) And (operationBlock.protocol_version=CT_PROTOCOL_6) then begin
      // First block with V6 protocol
      if FFreezedAccounts.CanUpgradeToProtocol(CT_PROTOCOL_6) then begin
        TLog.NewLog(ltInfo,ClassName,'Protocol upgrade to v6');
        If not FFreezedAccounts.DoUpgradeToProtocol6 then begin
          raise Exception.Create('Cannot upgrade to protocol v6 !');
        end;
      end;
    end;
    Result := true;
  finally
    FFreezedAccounts.EndThreadSave;
  end;
end;

procedure TPCSafeBoxTransaction.CopyFrom(transaction : TPCSafeBoxTransaction);
begin
  if transaction=Self then exit;
  if transaction.FFreezedAccounts<>FFreezedAccounts then raise Exception.Create('Invalid Freezed accounts to copy');
  CleanTransaction;
  FOrderedList.CopyFrom(transaction.FOrderedList);
  FOldSafeBoxHash := transaction.FOldSafeBoxHash;
  FTotalBalance := transaction.FTotalBalance;
  FTotalFee := transaction.FTotalFee;
end;

constructor TPCSafeBoxTransaction.Create(SafeBox : TPCSafeBox);
begin
  FOrderedList := TSealedAccountList.Create(Self);
  FFreezedAccounts := SafeBox;
  FOldSafeBoxHash := SafeBox.FSafeBoxHash;
  FTotalBalance := FFreezedAccounts.FTotalBalance;
  FTotalFee := 0;
  FAccountNames_Added := TOrderedRawList.Create;
  FAccountNames_Deleted := TOrderedRawList.Create;
end;

destructor TPCSafeBoxTransaction.Destroy;
begin
  CleanTransaction;
  FreeAndNil(FOrderedList);
  FreeAndNil(FAccountNames_Added);
  FreeAndNil(FAccountNames_Deleted);
  inherited;
end;

function TPCSafeBoxTransaction.Origin_BlocksCount: Cardinal;
begin
  Result := FFreezedAccounts.BlocksCount;
end;

function TPCSafeBoxTransaction.Origin_SafeboxHash: TRawBytes;
begin
  Result := FFreezedAccounts.FSafeBoxHash;
end;

function TPCSafeBoxTransaction.Origin_TotalBalance: Int64;
begin
  Result := FFreezedAccounts.TotalBalance;
end;

function TPCSafeBoxTransaction.Origin_TotalFee: Int64;
begin
  Result := FFreezedAccounts.FTotalFee;
end;

function TPCSafeBoxTransaction.Origin_FindAccountByName(const account_name: TRawBytes): Integer;
begin
  Result := FFreezedAccounts.FindAccountByName(account_name);
end;

function TPCSafeBoxTransaction.GetInternalAccount(account_number : Cardinal; var APtrSealedAccount : PSealedAccount) : PAccount;
begin
  // This process will return both pointers to Account and to TSealedAccount without
  // executing the seal process
  APtrSealedAccount := FOrderedList.GetAccount_Whitout_Sealing(account_number);
  Result := APtrSealedAccount^.AccountSealed;
end;

function TPCSafeBoxTransaction.FindAccountByNameInTransaction(const findName: TRawBytes; out isAddedInThisTransaction, isDeletedInThisTransaction : Boolean) : Integer;
Var iSafeBox, iAdded, iDeleted : Integer;
begin
  Result := -1;
  isAddedInThisTransaction := False;
  isDeletedInThisTransaction := False;
  If Length(findName)=0 then begin
    Exit; // No name, no found
  end;
  iSafeBox := Origin_FindAccountByName(findName);
  iAdded := FAccountNames_Added.IndexOf(findName);
  iDeleted := FAccountNames_Deleted.IndexOf(findName);
  isAddedInThisTransaction := (iAdded >= 0);
  isDeletedInThisTransaction := (iDeleted >= 0);
  if (iSafeBox<0) then begin
    // Not found previously, check added in current trans?
    If iAdded>=0 then begin
      Result := FAccountNames_Added.GetTag(iAdded);
    end;
  end else begin
    // Was found previously, check if deleted
    if iDeleted<0 then begin
      // Not deleted! "iSafebox" value contains account number using name
      Result := iSafeBox;
    end;
  end;
end;

procedure TPCSafeBoxTransaction.Rollback;
begin
  CleanTransaction;
end;

function TPCSafeBoxTransaction.TransferAmount(previous : TAccountPreviousBlockInfo; const AOpID : TRawBytes; sender,signer,target: Cardinal;
  n_operation: Cardinal; amount, fee: UInt64; var errors: String): Boolean;
Var
  PaccSender, PaccTarget,PaccSigner : PAccount;
  PaccSender_Sealed, PaccTarget_Sealed , PaccSigner_Sealed : PSealedAccount;
begin
  Result := false;
  errors := '';
  if not CheckIntegrity then begin
    errors := 'Invalid integrity in accounts transaction';
    exit;
  end;
  if (sender<0) Or (sender>=(Origin_BlocksCount*CT_AccountsPerBlock)) Or
     (signer<0) Or (signer>=(Origin_BlocksCount*CT_AccountsPerBlock)) Or
     (target<0) Or (target>=(Origin_BlocksCount*CT_AccountsPerBlock)) then begin
     errors := 'Invalid sender, signer or target on transfer';
     exit;
  end;
  if TAccountComp.IsAccountBlockedByProtocol(sender,Origin_BlocksCount) then begin
    errors := 'Sender account is blocked for protocol';
    Exit;
  end;
  if TAccountComp.IsAccountBlockedByProtocol(target,Origin_BlocksCount) then begin
    errors := 'Target account is blocked for protocol';
    Exit;
  end;
  PaccSender := GetInternalAccount(sender,PaccSender_Sealed);
  PaccTarget := GetInternalAccount(target,PaccTarget_Sealed);

  if (sender=signer) then begin
    if (PaccSender^.n_operation+1<>n_operation) then begin
      errors := 'Incorrect sender n_operation';
      Exit;
    end;
    if (PaccSender^.balance < (amount+fee)) then begin
      errors := 'Insuficient funds';
      Exit;
    end;
    PaccSigner := Nil;
    PaccSigner_Sealed := Nil;
  end else begin
    PaccSigner := GetInternalAccount(signer,PaccSigner_Sealed);
    if (PaccSigner^.n_operation+1<>n_operation) then begin
      errors := 'Incorrect signer n_operation';
      Exit;
    end;
    if (PaccSender^.balance < (amount)) then begin
      errors := 'Insufficient sender funds';
      Exit;
    end;
    if (PaccSigner^.balance < (fee)) then begin
      errors := 'Insufficient signer funds';
      Exit;
    end;
    if (TAccountComp.IsAccountLocked(PaccSigner^.accountInfo,Origin_BlocksCount)) then begin
      errors := 'Signer account is locked until block '+Inttostr(PaccSigner^.accountInfo.locked_until_block);
      Exit;
    end;
  end;
  if ((PaccTarget^.balance + amount)>CT_MaxWalletAmount) then begin
    errors := 'Max account balance';
    Exit;
  end;
  if (fee>CT_MaxTransactionFee) then begin
    errors := 'Max fee';
    Exit;
  end;
  if (TAccountComp.IsAccountLocked(PaccSender^.accountInfo,Origin_BlocksCount)) then begin
    errors := 'Sender account is locked until block '+Inttostr(PaccSender^.accountInfo.locked_until_block);
    Exit;
  end;

  previous.UpdateIfLower(PaccSender^.account,PaccSender^.GetLastUpdatedBlock);
  previous.UpdateIfLower(PaccTarget^.account,PaccTarget^.GetLastUpdatedBlock);

  UpdateSealAndActiveModeFlag(PaccSender_Sealed,AOpID,True);
  UpdateSealAndActiveModeFlag(PaccTarget_Sealed,AOpID,False);

  if (sender<>signer) then begin
    previous.UpdateIfLower(PaccSigner^.account,PaccSigner^.GetLastUpdatedBlock);
    UpdateSealAndActiveModeFlag(PaccSigner_Sealed,AOpID,True);
    PaccSigner^.n_operation := n_operation;
    PaccSigner^.balance := PaccSigner^.balance - (fee);
    PaccSender^.balance := PaccSender^.balance - (amount);
  end else begin
    PaccSender^.n_operation := n_operation;
    PaccSender^.balance := PaccSender^.balance - (amount + fee);
  end;
  PaccTarget^.balance := PaccTarget^.balance + (amount);

  Dec(FTotalBalance,Int64(fee));
  inc(FTotalFee,Int64(fee));
  Result := true;
end;

function TPCSafeBoxTransaction.TransferAmounts(previous : TAccountPreviousBlockInfo; const AOpID : TRawBytes;
  const senders, n_operations: array of Cardinal; const sender_amounts: array of UInt64;
  const receivers: array of Cardinal; const receivers_amounts: array of UInt64;
  var errors: String): Boolean;
Var i,j : Integer;
  PaccSender, PaccTarget : PAccount;
  PaccSender_Sealed, PaccTarget_Sealed : PSealedAccount;
  nTotalAmountSent, nTotalAmountReceived, nTotalFee : Int64;
begin
  Result := false;
  errors := '';
  nTotalAmountReceived:=0;
  nTotalAmountSent:=0;
  if not CheckIntegrity then begin
    errors := 'Invalid integrity in transfer amounts transaction';
    Exit;
  end;
  if (Length(senders)<>Length(n_operations)) Or
     (Length(senders)<>Length(sender_amounts)) Or
     (Length(senders)=0)
     then begin
    errors := 'Invalid senders/n_operations/amounts arrays length';
    Exit;
  end;
  if (Length(receivers)<>Length(receivers_amounts)) Or
     (Length(receivers)=0) then begin
    errors := 'Invalid receivers/amounts arrays length';
    Exit;
  end;
  // Check sender
  for i:=Low(senders) to High(senders) do begin
    for j:=i+1 to High(senders) do begin
      if (senders[i]=senders[j]) then begin
        errors := 'Duplicated sender';
        Exit;
      end;
    end;
    if (senders[i]<0) Or (senders[i]>=(Origin_BlocksCount*CT_AccountsPerBlock)) then begin
       errors := 'Invalid sender on transfer';
       exit;
    end;
    if TAccountComp.IsAccountBlockedByProtocol(senders[i],Origin_BlocksCount) then begin
      errors := 'Sender account is blocked for protocol';
      Exit;
    end;
    if (sender_amounts[i]<=0) then begin
      errors := 'Invalid amount for multiple sender';
      Exit;
    end;
    PaccSender := GetInternalAccount(senders[i],PaccSender_Sealed);
    if (PaccSender^.n_operation+1<>n_operations[i]) then begin
      errors := 'Incorrect multisender n_operation';
      Exit;
    end;
    if (PaccSender^.balance < sender_amounts[i]) then begin
      errors := 'Insuficient funds';
      Exit;
    end;
    if (TAccountComp.IsAccountLocked(PaccSender^.accountInfo,Origin_BlocksCount)) then begin
      errors := 'Multi sender account is locked until block '+Inttostr(PaccSender^.accountInfo.locked_until_block);
      Exit;
    end;
    inc(nTotalAmountSent,Int64(sender_amounts[i]));
  end;
  //
  for i:=Low(receivers) to High(receivers) do begin
    if (receivers[i]<0) Or (receivers[i]>=(Origin_BlocksCount*CT_AccountsPerBlock)) then begin
       errors := 'Invalid receiver on transfer';
       exit;
    end;
    if TAccountComp.IsAccountBlockedByProtocol(receivers[i],Origin_BlocksCount) then begin
      errors := 'Receiver account is blocked for protocol';
      Exit;
    end;
    if (receivers_amounts[i]<=0) then begin
      errors := 'Invalid amount for multiple receivers';
      Exit;
    end;
    inc(nTotalAmountReceived,Int64(receivers_amounts[i]));
    PaccTarget := GetInternalAccount(receivers[i],PaccTarget_Sealed);
    if ((PaccTarget^.balance + receivers_amounts[i])>CT_MaxWalletAmount) then begin
      errors := 'Max receiver balance';
      Exit;
    end;
  end;
  //
  nTotalFee := nTotalAmountSent - nTotalAmountReceived;
  If (nTotalAmountSent<nTotalAmountReceived) then begin
    errors := Format('Total amount sent %d < %d total amount received. fee %d',[nTotalAmountSent,nTotalAmountReceived,nTotalFee]);
    Exit;
  end;
  if (nTotalFee>CT_MaxTransactionFee) then begin
    errors := 'Max fee';
    Exit;
  end;
  // Ok, execute!
  for i:=Low(senders) to High(senders) do begin
    PaccSender := GetInternalAccount(senders[i],PaccSender_Sealed);

    previous.UpdateIfLower(PaccSender^.account,PaccSender^.GetLastUpdatedBlock);

    UpdateSealAndActiveModeFlag(PaccSender_Sealed,AOpID,True);

    Inc(PaccSender^.n_operation);
    PaccSender^.balance := PaccSender^.balance - (sender_amounts[i]);
  end;
  for i:=Low(receivers) to High(receivers) do begin
    PaccTarget := GetInternalAccount(receivers[i],PaccTarget_Sealed);

    previous.UpdateIfLower(PaccTarget^.account,PaccTarget^.GetLastUpdatedBlock);

    UpdateSealAndActiveModeFlag(PaccTarget_Sealed,AOpID,False);

    PaccTarget^.balance := PaccTarget^.balance + receivers_amounts[i];
  end;
  Dec(FTotalBalance,nTotalFee);
  inc(FTotalFee,nTotalFee);
  Result := true;
end;

function TPCSafeBoxTransaction.UpdateAccountInfo(previous : TAccountPreviousBlockInfo;
  const AOpID : TRawBytes;
  signer_account, signer_n_operation, target_account: Cardinal;
  const accountInfo: TAccountInfo; const newName, newData: TRawBytes; newType: Word; fee: UInt64; var errors: String): Boolean;
Var i : Integer;
  P_signer, P_target : PAccount;
  P_signer_Sealed, P_target_Sealed : PSealedAccount;
begin
  Result := false;
  errors := '';
  if not CheckIntegrity then begin
    errors := 'Invalid integrity on Update account info';
    Exit;
  end;
  if (signer_account<0) Or (signer_account>=(Origin_BlocksCount*CT_AccountsPerBlock)) Or
     (target_account<0) Or (target_account>=(Origin_BlocksCount*CT_AccountsPerBlock)) Then begin
     errors := 'Invalid account';
     exit;
  end;
  if (TAccountComp.IsAccountBlockedByProtocol(signer_account,Origin_BlocksCount)) Or
     (TAccountComp.IsAccountBlockedByProtocol(target_account,Origin_BlocksCount)) then begin
    errors := 'account is blocked for protocol';
    Exit;
  end;
  P_signer := GetInternalAccount(signer_account,P_signer_Sealed);
  P_target := GetInternalAccount(target_account,P_target_Sealed);
  if (P_signer^.n_operation+1<>signer_n_operation) then begin
    errors := 'Incorrect n_operation';
    Exit;
  end;
  if (P_signer^.balance < fee) then begin
    errors := 'Insufficient Funds';
    Exit;
  end;
  if (TAccountComp.IsAccountLocked(P_signer^.accountInfo,Origin_BlocksCount)) then begin
    errors := 'Signer account is locked until block '+Inttostr(P_signer^.accountInfo.locked_until_block);
    Exit;
  end;
  if (TAccountComp.IsAccountLocked(P_target^.accountInfo,Origin_BlocksCount)) then begin
    errors := 'Target account is locked until block '+Inttostr(P_target^.accountInfo.locked_until_block);
    Exit;
  end;
  if Not TAccountComp.EqualAccountKeys(P_signer^.accountInfo.accountKey,P_target^.accountInfo.accountKey) then begin
    errors := 'Signer and target have diff key';
    Exit;
  end;
  if (Not TBaseType.Equals(newName,P_target^.name)) then begin
    // NEW NAME CHANGE CHECK:
    if Length(newName)>0 then begin
      If Not TPCSafeBox.ValidAccountName(newName,errors) then begin
        errors := 'Invalid account name "'+newName.ToPrintable+'" length:'+IntToStr(length(newName))+': '+errors;
        Exit;
      end;
      i := Origin_FindAccountByName(newName);
      if (i>=0) then begin
        // This account name is in the safebox... check if deleted:
        i := FAccountNames_Deleted.IndexOf(newName);
        if i<0 then begin
          errors := 'Account name "'+newName.ToPrintable+'" is in current use';
          Exit;
        end;
      end;
      i := FAccountNames_Added.IndexOf(newName);
      if (i>=0) then begin
        // This account name is added in this transaction! (perhaps deleted also, but does not allow to "double add same name in same block")
        errors := 'Account name "'+newName.ToPrintable+'" is in same transaction';
        Exit;
      end;
    end;
    // Ok, include
    if (Length(P_target^.name)>0) then begin
      // In use in the safebox, mark as deleted
      FAccountNames_Deleted.Add(P_target^.name,target_account);
    end;
    if (Length(newName)>0) then begin
      FAccountNames_Added.Add(newName,target_account);
    end;
  end;
  if (Length(newData)>CT_MaxAccountDataSize) then begin
    errors := 'Account Data size '+IntToStr(Length(newData))+'>'+IntToStr(CT_MaxAccountDataSize);
    Exit;
  end;
  // All Ok, can do changes

  previous.UpdateIfLower(P_signer^.account,P_signer^.GetLastUpdatedBlock);
  previous.UpdateIfLower(P_target^.account,P_target^.GetLastUpdatedBlock);

  UpdateSealAndActiveModeFlag(P_signer_Sealed,AOpID,True);
  UpdateSealAndActiveModeFlag(P_target_Sealed,AOpID,True); // BOTH signer and target are ACTIVE

  P_signer^.n_operation := signer_n_operation;

  P_target^.accountInfo := accountInfo;
  P_target^.name := newName;
  P_target^.account_data := newData;
  P_target^.account_type := newType;
  Dec(P_signer^.balance,Int64(fee)); // Signer is who pays the fee
  Dec(FTotalBalance,Int64(fee));
  Inc(FTotalFee,Int64(fee));
  Result := true;
end;

procedure TPCSafeBoxTransaction.UpdateSealAndActiveModeFlag(APtrSealedAccount : PSealedAccount; AOpID : TRawBytes; ASetUsedAsActiveMode : Boolean);
begin
  FOrderedList.DoUpdateSealIfNeeded(APtrSealedAccount,AOpID);
  if ASetUsedAsActiveMode then begin
    APtrSealedAccount^.UsedAsActiveMode := True;
    APtrSealedAccount^.AccountSealed^.updated_on_block_active_mode := Origin_BlocksCount;
  end else begin
    APtrSealedAccount^.UsedAsPasiveMode := True;
    APtrSealedAccount^.AccountSealed^.updated_on_block_passive_mode := Origin_BlocksCount;
  end;
  if FreezedSafeBox.CurrentProtocol<CT_PROTOCOL_5 then begin
    // V5 introduced active/pasive mode, but v4 (and previous) does not made distinction
    APtrSealedAccount^.AccountSealed^.updated_on_block_active_mode := Origin_BlocksCount;
    APtrSealedAccount^.AccountSealed^.updated_on_block_passive_mode := Origin_BlocksCount;
  end;
end;

{ TPCSafeBoxTransaction.TSealedAccountList }

procedure TPCSafeBoxTransaction.TSealedAccountList.Clear;
var i : Integer;
  p : PSealedAccount;
begin
  for i := FList.Count-1 downto 0 do begin
    p := FList[i];
    Dispose( p^.AccountSealed );
    p^.LatestOpIDUsedForSeal := Nil;
    p^.AccountSealed := Nil;
    Dispose(p);
    FList[i] := Nil;
  end;
  FList.Clear;
end;

procedure TPCSafeBoxTransaction.TSealedAccountList.CopyFrom(ASource: TSealedAccountList);
var i : Integer;
  p : PSealedAccount;
begin
  if (ASource = Self) then Exit;
  Clear;
  for i := 0 to ASource.FList.Count-1 do begin
    New(p);
    p^.LatestOpIDUsedForSeal := PSealedAccount(ASource.FList[i])^.LatestOpIDUsedForSeal;
    New(p^.AccountSealed);
    p^.AccountSealed^ := PSealedAccount(ASource.FList[i])^.AccountSealed^;
    p^.SealChangesCounter := PSealedAccount(ASource.FList[i])^.SealChangesCounter;
    p^.UsedAsPasiveMode := PSealedAccount(ASource.FList[i])^.UsedAsPasiveMode;
    p^.UsedAsActiveMode := PSealedAccount(ASource.FList[i])^.UsedAsActiveMode;
    FList.Add(p);
  end;
end;

function TPCSafeBoxTransaction.TSealedAccountList.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TPCSafeBoxTransaction.TSealedAccountList.Create(ASafeBoxTransaction : TPCSafeBoxTransaction);
begin
  FSafeBoxTransaction := ASafeBoxTransaction;
  FList := TList<Pointer>.Create;
end;

destructor TPCSafeBoxTransaction.TSealedAccountList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TPCSafeBoxTransaction.TSealedAccountList.DoUpdateSealIfNeeded(APtrSealedAccount: PSealedAccount; const AOpID: TRawBytes);
var LStream : TMemoryStream;
begin
  // Do Seal process
  if (FSafeBoxTransaction.FreezedSafeBox.CurrentProtocol>=CT_PROTOCOL_5) then begin
    if Not TBaseType.Equals(APtrSealedAccount^.LatestOpIDUsedForSeal,AOpID) then begin
      // If protocol>=5 and latest Seal was made with an other OPID, then update Seal
      APtrSealedAccount^.LatestOpIDUsedForSeal := AOpID;
      Inc(APtrSealedAccount^.SealChangesCounter);
      LStream := TMemoryStream.Create;
      Try
        // New Seal = RIPEMD160(  SHA2_256(    SerializedAccount ++ OpID ++ LatestSafeboxHash  ) )
        APtrSealedAccount^.AccountSealed.SerializeAccount(LStream,FSafeBoxTransaction.FreezedSafeBox.CurrentProtocol); // Serialize with LATEST seal value
        LStream.WriteBuffer( AOpID[0], Length(AOpID));
        LStream.WriteBuffer( FSafeBoxTransaction.FOldSafeBoxHash[0], Length(FSafeBoxTransaction.FOldSafeBoxHash) );
        APtrSealedAccount^.AccountSealed^.account_seal := TCrypto.DoRipeMD160AsRaw( TCrypto.DoSha256(LStream.Memory,LStream.Size) );
      finally
        LStream.Free;
      end;
    end;
  end;
end;

function TPCSafeBoxTransaction.TSealedAccountList.Find(
  const account_number: Cardinal; var Index: Integer): Boolean;
var L, H, I: Integer;
  C : Int64;
begin
  Result := False;
  L := 0;
  H := FList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Int64((PSealedAccount(FList.Items[i])^.AccountSealed^.account) - Int64(account_number));
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

function TPCSafeBoxTransaction.TSealedAccountList.GetAccount_Whitout_Sealing(account_number: Cardinal) : PSealedAccount;
var i : Integer;
begin
  If Not Find(account_number,i) then begin
    // Save for first time
    New(Result);
    Result^.LatestOpIDUsedForSeal := Nil;
    New( Result^.AccountSealed );
    Result^.AccountSealed^ := FSafeBoxTransaction.FreezedSafeBox.Account(account_number);
    Result^.SealChangesCounter := 0;
    Result^.UsedAsPasiveMode := False;
    Result^.UsedAsActiveMode := False;
    FList.Insert(i,Result);
  end else begin
    Result := FList.Items[i];
  end;
end;

{ TOrderedBlockAccountList }

Type
  TOrderedBlockAccount = Record
    block : Cardinal;
    memBlock : TMemBlockAccount;
  end;
  POrderedBlockAccount = ^TOrderedBlockAccount;

function TOrderedBlockAccountList.Find(const block_number: Cardinal; out Index: Integer): Boolean;
var L, H, I: Integer;
  C : Int64;
begin
  Result := False;
  L := 0;
  H := FList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Int64(POrderedBlockAccount(FList[I])^.block) - Int64(block_number);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

function TOrderedBlockAccountList.SaveBlockAccount(const blockAccount: TBlockAccount; UpdateIfFound: Boolean): Integer;
Var P : POrderedBlockAccount;
begin
  If Not Find(blockAccount.blockchainInfo.block,Result) then begin
    New(P);
    P^.block:=blockAccount.blockchainInfo.block;
    FList.Insert(Result,P);
    ToTMemBlockAccount(blockAccount,P^.memBlock);
    If Integer(blockAccount.blockchainInfo.block)>FMaxBlockNumber then FMaxBlockNumber:=blockAccount.blockchainInfo.block;
  end else if (UpdateIfFound) then begin
    P := FList[Result];
    ToTMemBlockAccount(blockAccount,P^.memBlock);
  end;
end;

constructor TOrderedBlockAccountList.Create;
begin
  FList := TList<Pointer>.Create;
  FMaxBlockNumber:=-1;
end;

destructor TOrderedBlockAccountList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TOrderedBlockAccountList.Clear;
var P : POrderedBlockAccount;
  i : Integer;
begin
  For i:=0 to FList.Count-1 do begin
    P := FList[i];
    Dispose(P);
  end;
  FList.Clear;
  FMaxBlockNumber:=-1;
end;

function TOrderedBlockAccountList.AddIfNotExists(const blockAccount: TBlockAccount): Integer;
begin
  Result := SaveBlockAccount(blockAccount,False);
end;

function TOrderedBlockAccountList.Add(const blockAccount: TBlockAccount): Integer;
begin
  Result := SaveBlockAccount(blockAccount,True);
end;

function TOrderedBlockAccountList.Count: Integer;
begin
  Result := FList.Count;
end;

function TOrderedBlockAccountList.Get(index: Integer): TBlockAccount;
begin
  ToTBlockAccount(POrderedBlockAccount(FList[index])^.memBlock,POrderedBlockAccount(FList[index])^.block,Result);
end;

function TOrderedBlockAccountList.MaxBlockNumber: Integer;
begin
  Result := FMaxBlockNumber;
end;

{ TOrderedAccountList }

Function TOrderedAccountList.Add(const account: TAccount) : Integer;
Var P : PAccount;
begin
  if Find(account.account,Result) then begin
    PAccount(FList[Result])^ := account;
  end else begin
    New(P);
    P^:=account;
    FList.Insert(Result,P);
  end;
end;

procedure TOrderedAccountList.Clear;
Var i : Integer;
  P : PAccount;
begin
  for I := 0 to FList.Count - 1 do begin
    P := FList[i];
    Dispose(P);
  end;
  FList.Clear;
end;

function TOrderedAccountList.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TOrderedAccountList.Create;
begin
  FList := TList<Pointer>.Create;
end;

destructor TOrderedAccountList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TOrderedAccountList.Find(const account_number: Cardinal; var Index: Integer): Boolean;
var L, H, I: Integer;
  C : Int64;
begin
  Result := False;
  L := 0;
  H := FList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Int64(PAccount(FList[I]).account) - Int64(account_number);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

function TOrderedAccountList.Get(index: Integer): TAccount;
begin
  Result := PAccount(FList.Items[index])^;
end;

function TOrderedAccountList.IndexOf(account_number: Cardinal): Integer;
begin
  If Not Find(account_number,Result) then Result := -1;
end;

{ TOrderedAccountKeysList }
Type
  TOrderedAccountKeyList = Record
    {$IFDEF useAccountKeyStorage}
    accountKeyPtr : PAccountKey;
    {$ELSE}
    rawaccountkey : TRawBytes;
    {$ENDIF}
    {$IFnDEF USE_ABSTRACTMEM}
    accounts_number : TOrderedCardinalList;
    {$ENDIF}
    changes_counter : Integer;
  end;
  POrderedAccountKeyList = ^TOrderedAccountKeyList;
Const
  CT_TOrderedAccountKeyList_NUL : TOrderedAccountKeyList = ({$IFDEF useAccountKeyStorage}accountKeyPtr:Nil{$ELSE}rawaccountkey:Nil{$ENDIF};{$IFnDEF USE_ABSTRACTMEM}accounts_number:Nil;{$ENDIF}changes_counter:0);

function SortOrdered(Item1, Item2: Pointer): Integer;
begin
   Result := PtrInt(Item1) - PtrInt(Item2);
end;

procedure TOrderedAccountKeysList.AddAccountKey(const AccountKey: TAccountKey);
Var P : POrderedAccountKeyList;
  i : Integer;
  lockedList, safeboxLockedList : TList<Pointer>;
begin
  lockedList := Lock;
  Try
    if Not Find(lockedList,AccountKey,i) then begin
      New(P);
      P^ := CT_TOrderedAccountKeyList_NUL;
      {$IFDEF useAccountKeyStorage}
      P^.accountKeyPtr:=TAccountKeyStorage.KS.AddAccountKey(AccountKey);
      {$ELSE}
      P^.rawaccountkey := TAccountComp.AccountKey2RawString(AccountKey);
      {$ENDIF}
      {$IFnDEF USE_ABSTRACTMEM}
      P^.accounts_number := TOrderedCardinalList.Create;
      {$ENDIF}
      inc(P^.changes_counter);
      inc(FTotalChanges);
      lockedList.Insert(i,P);
      // Search this key in the AccountsList and add all...
      if Assigned(FAccountList) then begin
        {$IFnDEF USE_ABSTRACTMEM}
        If (Assigned(FAccountList.OrderedAccountKeysList)) then begin
          safeboxLockedList := FAccountList.OrderedAccountKeysList.Lock;
          try
            i := FAccountList.OrderedAccountKeysList.IndexOfAccountKey(AccountKey);
            if (i>=0) then begin
              P^.accounts_number.CopyFrom( FAccountList.OrderedAccountKeysList.AccountKeyList[i] );
            end;
          finally
            FAccountList.OrderedAccountKeysList.Unlock;
          end;
        end else begin
          For i:=0 to FAccountList.AccountsCount-1 do begin
            If TAccountComp.EqualAccountKeys(FAccountList.Account(i).accountInfo.accountkey,AccountKey) then begin
              // Note: P^.accounts will be ascending ordered due to "for i:=0 to ..."
              P^.accounts_number.Add(i);
            end;
          end;
        end;
        TLog.NewLog(ltdebug,Classname,Format('Adding account key (%d with %d accounts) %s',[lockedList.Count,P^.accounts_number.Count,TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(AccountKey))]));
        {$ENDIF}
      end else begin
        TLog.NewLog(ltdebug,Classname,Format('Adding account key (no Account List) %s',[TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(AccountKey))]));
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TOrderedAccountKeysList.AddAccountKeys(
  const AccountKeys: array of TAccountKey);
var i : integer;
begin
  for i := Low(AccountKeys) to High(AccountKeys) do
    AddAccountKey(AccountKeys[i]);
end;

{$IFnDEF USE_ABSTRACTMEM}
procedure TOrderedAccountKeysList.AddAccounts(const AccountKey: TAccountKey; const accounts: array of Cardinal);
Var P : POrderedAccountKeyList;
  i : Integer;
  lockedList : TList<Pointer>;
begin
  lockedList := Lock;
  Try
    if Find(lockedList,AccountKey,i) then begin
      P :=  POrderedAccountKeyList(lockedList[i]);
    end else if (FAutoAddAll) then begin
      New(P);
      P^ := CT_TOrderedAccountKeyList_NUL;
      {$IFDEF useAccountKeyStorage}
      P^.accountKeyPtr:=TAccountKeyStorage.KS.AddAccountKey(AccountKey);
      {$ELSE}
      P^.rawaccountkey := TAccountComp.AccountKey2RawString(AccountKey);
      {$ENDIF}
      P^.accounts_number := TOrderedCardinalList.Create;
      lockedList.Insert(i,P);
    end else exit;
    for i := Low(accounts) to High(accounts) do begin
      P^.accounts_number.Add(accounts[i]);
    end;
    inc(P^.changes_counter);
    inc(FTotalChanges);
  finally
    Unlock;
  end;
end;
{$ENDIF}

procedure TOrderedAccountKeysList.Clear;
begin
  Lock;
  Try
    ClearAccounts(true);
    FTotalChanges := 1; // 1 = At least 1 change
  finally
    Unlock;
  end;
end;

function TOrderedAccountKeysList.ToArray : TAccountKeyArray;
var i : Integer;
begin
  Lock;
  Try
    SetLength(Result, Count);
    for i := 0 to Count - 1 do Result[i] := Self.AccountKey[i];
  finally
    Unlock;
  end;
end;

function TOrderedAccountKeysList.Lock: TList<Pointer>;
begin
  Result := FOrderedAccountKeysList.LockList;
end;

procedure TOrderedAccountKeysList.Unlock;
begin
  FOrderedAccountKeysList.UnlockList;
end;

function TOrderedAccountKeysList.HasAccountKeyChanged: Boolean;
begin
  Result := FTotalChanges>0;
end;

procedure TOrderedAccountKeysList.CopyFrom(const source: TOrderedAccountKeysList);
var selfList,sourceList : TList<Pointer>;
  i : Integer;
  newP,sourceP : POrderedAccountKeyList;
begin
  If Self=source then Exit;
  selfList := Lock;
  try
    sourceList := source.Lock;
    try
      Clear;
      selfList.Capacity:=sourceList.Capacity;
      for i:=0 to sourceList.Count-1 do begin
        sourceP := POrderedAccountKeyList(sourceList[i]);
        new(newP);
        newP^ := CT_TOrderedAccountKeyList_NUL;
        {$IFnDEF USE_ABSTRACTMEM}
        newP^.accounts_number := TOrderedCardinalList.Create;
        newP^.accounts_number.CopyFrom(sourceP^.accounts_number);
        {$ENDIF}
        newP^.changes_counter:=sourceP^.changes_counter;
        {$IFDEF useAccountKeyStorage}
        newP^.accountKeyPtr:=TAccountKeyStorage.KS.AddAccountKey(sourceP^.accountKeyPtr^);
        {$ELSE}
        newP^.rawaccountkey:=sourceP^.rawaccountkey;
        {$ENDIF}
        selfList.Add(newP);
      end;
    finally
      source.Unlock;
    end;
  finally
    Unlock;
  end;
end;

procedure TOrderedAccountKeysList.ClearAccounts(RemoveAccountList : Boolean);
Var P : POrderedAccountKeyList;
  i : Integer;
  lockedList : TList<Pointer>;
begin
  lockedList := Lock;
  Try
    for i := 0 to  lockedList.Count - 1 do begin
      P := lockedList[i];
      inc(P^.changes_counter);
      if RemoveAccountList then begin
        {$IFDEF useAccountKeyStorage}
        TAccountKeyStorage.KS.RemoveAccountKey(P^.accountKeyPtr^);
        {$ENDIF}
        {$IFnDEF USE_ABSTRACTMEM}
        P^.accounts_number.Free;
        {$ENDIF}
        Dispose(P);
      end else begin
        {$IFnDEF USE_ABSTRACTMEM}
        P^.accounts_number.Clear;
        {$ENDIF}
      end;
    end;
    if RemoveAccountList then begin
      lockedList.Clear;
    end;
    FTotalChanges:=lockedList.Count + 1; // At least 1 change
  finally
    Unlock;
  end;
end;

function TOrderedAccountKeysList.Count: Integer;
var lockedList : TList<Pointer>;
begin
  lockedList := Lock;
  Try
    Result := lockedList.Count;
  finally
    Unlock;
  end;
end;

constructor TOrderedAccountKeysList.Create(AccountList : TPCSafeBox; AutoAddAll : Boolean);
Var i : Integer;
begin
  TLog.NewLog(ltdebug,Classname,'Creating an Ordered Account Keys List adding all:'+CT_TRUE_FALSE[AutoAddAll]);
  FAutoAddAll := AutoAddAll;
  FAccountList := AccountList;
  FTotalChanges:=0;
  FOrderedAccountKeysList := TPCThreadList<Pointer>.Create(ClassName);
  if Assigned(AccountList) then begin
    Lock;
    Try
      AccountList.FListOfOrderedAccountKeysList.Add(Self);
      if AutoAddAll then begin
        for i := 0 to AccountList.AccountsCount - 1 do begin
          AddAccountKey(AccountList.Account(i).accountInfo.accountkey);
        end;
      end;
    finally
      Unlock;
    end;
  end else begin
  {$IFDEF USE_ABSTRACTMEM}
  raise Exception.Create('ERROR DEV 2020050101-01 ABSTRACTMEM ERROR Cannot capture accounts/pubkeys if no Safebox');
  {$ENDIF}
  end;
end;

destructor TOrderedAccountKeysList.Destroy;
begin
  TLog.NewLog(ltdebug,Classname,'Destroying an Ordered Account Keys List adding all:'+CT_TRUE_FALSE[FAutoAddAll]);
  if Assigned(FAccountList) then begin
    FAccountList.FListOfOrderedAccountKeysList.Remove(Self);
  end;
  ClearAccounts(true);
  FreeAndNil(FOrderedAccountKeysList);
  inherited;
end;

function TOrderedAccountKeysList.Find(lockedList : TList<Pointer>; const AccountKey: TAccountKey; var Index: Integer): Boolean;
var L, H, I: Integer;
  C : Int64;
  {$IFDEF useAccountKeyStorage}
  pacsd : PAccountKeyStorageData;
  {$ELSE}
  rak : TRawBytes;
  {$ENDIF}
begin
  Result := False;
  L := 0;
  H := lockedList.Count - 1;
  {$IFDEF useAccountKeyStorage}
  pacsd:=TAccountKeyStorage.KS.AddAccountKeyExt(AccountKey);
  {$ELSE}
  rak := TAccountComp.AccountKey2RawString(AccountKey);
  {$ENDIF}
  while L <= H do
  begin
    I := (L + H) shr 1;
    {$IFDEF useAccountKeyStorage}
    C := PtrInt(POrderedAccountKeyList(lockedList[I])^.accountKeyPtr)-PtrInt(pacsd^.ptrAccountKey);
    {$ELSE}
    C := TBaseType.BinStrComp( POrderedAccountKeyList(lockedList[I]).rawaccountkey, rak );
    {$ENDIF}
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  {$IFDEF useAccountKeyStorage}
  Dec(pacsd^.counter);
  {$ENDIF}
  Index := L;
end;

function TOrderedAccountKeysList.GetAccountKeyChanges(index : Integer): Integer;
var lockedList : TList<Pointer>;
begin
  lockedList := Lock;
  Try
    Result :=  POrderedAccountKeyList(lockedList[index])^.changes_counter;
  finally
    Unlock;
  end;
end;

function TOrderedAccountKeysList.GetAccountKey(index: Integer): TAccountKey;
Var lockedList : TList<Pointer>;
  {$IFDEF useAccountKeyStorage}
  {$ELSE}
  raw : TRawBytes;
  {$ENDIF}
begin
  lockedList := Lock;
  Try
    {$IFDEF useAccountKeyStorage}
    Result := POrderedAccountKeyList(lockedList[index])^.accountKeyPtr^;
    {$ELSE}
    raw := POrderedAccountKeyList(lockedList[index]).rawaccountkey;
    Result := TAccountComp.RawString2Accountkey(raw);
    {$ENDIF}
  finally
    Unlock;
  end;
end;

function TOrderedAccountKeysList.GetAccountKeyList(index: Integer): TAccountsNumbersList;
var lockedList : TList<Pointer>;
  Lpk : TAccountKey;
begin
  lockedList := Lock;
  Try
    {$IFDEF USE_ABSTRACTMEM}
    Lpk := GetAccountKey(index);
    Result := FAccountList.FPCAbstractMem.AccountKeys.GetAccountsUsingThisKey( Lpk );
    {$ELSE}
    Result := POrderedAccountKeyList(lockedList[index]).accounts_number;
    {$ENDIF}
  finally
    Unlock;
  end;
end;

function TOrderedAccountKeysList.GetAccountsUsingThisKey(const AAccountKey: TAccountKey): TAccountsNumbersList;
var iPubKey : Integer;
begin
  iPubKey := IndexOfAccountKey(AAccountKey);
  if (iPubKey>=0) then begin
    Result := GetAccountKeyList(iPubKey);
  end else Result := Nil;
end;

function TOrderedAccountKeysList.IndexOfAccountKey(const AccountKey: TAccountKey): Integer;
var lockedList : TList<Pointer>;
begin
  lockedList := Lock;
  Try
    If Not Find(lockedList,AccountKey,Result) then Result := -1;
  finally
    Unlock;
  end;
end;

procedure TOrderedAccountKeysList.ClearAccountKeyChanges;
var i : Integer;
  lockedList : TList<Pointer>;
begin
  lockedList := Lock;
  Try
    for i:=0 to lockedList.Count-1 do begin
      POrderedAccountKeyList(lockedList[i])^.changes_counter:=0;
    end;
    FTotalChanges:=0;
  finally
    Unlock;
  end;
end;

{$IFnDEF USE_ABSTRACTMEM}
procedure TOrderedAccountKeysList.RemoveAccounts(const AccountKey: TAccountKey; const accounts: array of Cardinal);
Var P : POrderedAccountKeyList;
  i,j : Integer;
  lockedList : TList<Pointer>;
begin
  lockedList := Lock;
  Try
    if Not Find(lockedList,AccountKey,i) then exit; // Nothing to do
    P :=  POrderedAccountKeyList(lockedList[i]);
    inc(P^.changes_counter);
    inc(FTotalChanges);
    for j := Low(accounts) to High(accounts) do begin
      P^.accounts_number.Remove(accounts[j]);
    end;
    if (P^.accounts_number.Count=0) And (FAutoAddAll) then begin
      // Remove from list
      lockedList.Delete(i);
      // Free it
      P^.accounts_number.free;
      {$IFDEF useAccountKeyStorage}
      TAccountKeyStorage.KS.RemoveAccountKey(AccountKey);
      {$ENDIF}
      Dispose(P);
    end;
  finally
    Unlock;
  end;
end;
{$ENDIF}

procedure TOrderedAccountKeysList.RemoveAccountKey(const AccountKey: TAccountKey);
Var P : POrderedAccountKeyList;
  i : Integer;
  lockedList : TList<Pointer>;
begin
  lockedList := Lock;
  Try
    if Not Find(lockedList,AccountKey,i) then exit; // Nothing to do
    P :=  POrderedAccountKeyList(lockedList[i]);
    inc(P^.changes_counter);
    inc(FTotalChanges);
    // Remove from list
    lockedList.Delete(i);
    // Free it
    {$IFDEF useAccountKeyStorage}
    TAccountKeyStorage.KS.RemoveAccountKey(AccountKey);
    {$ENDIF}
    {$IFnDEF USE_ABSTRACTMEM}
    P^.accounts_number.free;
    {$ENDIF}
    Dispose(P);
  finally
    Unlock;
  end;
end;

{ TAccountPreviousBlockInfo }

Type PAccountPreviousBlockInfoData = ^TAccountPreviousBlockInfoData;

function TAccountPreviousBlockInfo.FindAccount(const account: Cardinal; var Index: Integer): Boolean;
var L, H, I: Integer;
  C : Int64;
  P : PAccountPreviousBlockInfoData;
begin
  Result := False;
  L := 0;
  H := FList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    P := FList[i];
    C := Int64(P^.Account) - Int64(account);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

function TAccountPreviousBlockInfo.GetData(index : Integer): TAccountPreviousBlockInfoData;
begin
  Result := PAccountPreviousBlockInfoData(FList[index])^;
end;

constructor TAccountPreviousBlockInfo.Create;
begin
  FList := TList<Pointer>.Create;
end;

destructor TAccountPreviousBlockInfo.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TAccountPreviousBlockInfo.UpdateIfLower(account, previous_updated_block: Cardinal);
Var P : PAccountPreviousBlockInfoData;
  i : Integer;
begin
  if (account>=CT_AccountsPerBlock) And (previous_updated_block=0) then Exit; // Only accounts 0..4 allow update on block 0

  if Not FindAccount(account,i) then begin
    New(P);
    P^.Account:=account;
    P^.Previous_updated_block:=previous_updated_block;
    FList.Insert(i,P);
  end else begin
    P := FList[i];
    If (P^.Previous_updated_block>previous_updated_block) then begin
      P^.Previous_updated_block:=previous_updated_block;
    end;
  end
end;

function TAccountPreviousBlockInfo.Add(account, previous_updated_block: Cardinal): Integer;
Var P : PAccountPreviousBlockInfoData;
begin
  if Not FindAccount(account,Result) then begin
    New(P);
    P^.Account:=account;
    P^.Previous_updated_block:=previous_updated_block;
    FList.Insert(Result,P);
  end else begin
    P := FList[Result];
    P^.Previous_updated_block:=previous_updated_block;
  end
end;

procedure TAccountPreviousBlockInfo.Remove(account: Cardinal);
Var i : Integer;
  P : PAccountPreviousBlockInfoData;
begin
  If FindAccount(account,i) then begin
    P := FList[i];
    FList.Delete(i);
    Dispose(P);
  end;
end;

procedure TAccountPreviousBlockInfo.Clear;
var P : PAccountPreviousBlockInfoData;
  i : Integer;
begin
  For i:=0 to FList.Count-1 do begin
    P := FList[i];
    Dispose(P);
  end;
  FList.Clear;
end;

procedure TAccountPreviousBlockInfo.CopyFrom(Sender: TAccountPreviousBlockInfo);
Var P : PAccountPreviousBlockInfoData;
  i : Integer;
begin
  if (Sender = Self) then Raise Exception.Create('ERROR DEV 20180312-4 Myself');
  Clear;
  For i:=0 to Sender.Count-1 do begin
    New(P);
    P^ := Sender.GetData(i);
    FList.Add(P);
  end;
end;

function TAccountPreviousBlockInfo.IndexOfAccount(account: Cardinal): Integer;
begin
  If Not FindAccount(account,Result) then Result := -1;
end;

function TAccountPreviousBlockInfo.GetPreviousUpdatedBlock(account: Cardinal; defaultValue : Cardinal): Cardinal;
var i : Integer;
begin
  i := IndexOfAccount(account);
  If i>=0 then Result := GetData(i).Previous_updated_block
  else Result := defaultValue;
end;

function TAccountPreviousBlockInfo.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TAccountPreviousBlockInfo.SaveToStream(stream: TStream);
var i : Integer;
  c : Cardinal;
  apbi : TAccountPreviousBlockInfoData;
begin
  c := Count;
  stream.Write(c,SizeOf(c)); // Save 4 bytes for count
  for i:=0 to Count-1 do begin
    apbi := GetData(i);
    stream.Write(apbi.Account,SizeOf(apbi.Account)); // 4 bytes for account
    stream.Write(apbi.Previous_updated_block,SizeOf(apbi.Previous_updated_block)); // 4 bytes for block number
  end;
end;

function TAccountPreviousBlockInfo.LoadFromStream(stream: TStream): Boolean;
Var lastAcc,nposStreamStart : Int64;
  c : Cardinal;
  i : Integer;
  apbi : TAccountPreviousBlockInfoData;
begin
  Result := False;
  clear;
  nposStreamStart:=stream.Position;
  Try
    lastAcc := -1;
    if (stream.Read(c,SizeOf(c))<SizeOf(c)) then Exit;
    for i:=1 to c do begin
      if stream.Read(apbi.Account,SizeOf(apbi.Account)) < SizeOf(apbi.Account) then Exit; // 4 bytes for account
      if stream.Read(apbi.Previous_updated_block,SizeOf(apbi.Previous_updated_block)) < SizeOf(apbi.Previous_updated_block) then Exit; // 4 bytes for block number
      if (lastAcc >= apbi.Account) then Exit;
      Add(apbi.Account,apbi.Previous_updated_block);
      lastAcc := apbi.Account;
    end;
    Result := True;
  finally
    if Not Result then stream.Position:=nposStreamStart;
  end;
end;

{ TAccount_Helper }

procedure TAccount_Helper.SerializeAccount(AStream: TStream; current_protocol : Word);
var LRaw : TRawBytes;
  LCardinal : Cardinal;
begin
  if current_protocol>=CT_PROTOCOL_5 then TAccountComp.SaveAccountToAStream(AStream,Self,current_protocol)
  else begin
    AStream.Write(Self.account,4);
    LRaw := TAccountComp.AccountInfo2RawString(Self.accountInfo);
    AStream.WriteBuffer(LRaw[Low(LRaw)],Length(LRaw));
    AStream.Write(Self.balance,8);
    LCardinal := Self.GetLastUpdatedBlock;
    AStream.Write(LCardinal,4);
    AStream.Write(Self.n_operation,4);
    if (current_protocol>=2) then begin
        // Use new Protocol 2 fields
      If Length(Self.name)>0 then begin
        AStream.WriteBuffer(Self.name[Low(Self.name)],Length(Self.name));
      end;
      AStream.Write(Self.account_type,2);
    end;
  end;
end;

initialization
  _INTERNAL_PascalCoinProtocol := Nil;
finalization
  FreeAndNil(_INTERNAL_PascalCoinProtocol);
end.
