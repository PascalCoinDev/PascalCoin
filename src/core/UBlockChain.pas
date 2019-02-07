unit UBlockChain;

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

uses
  Classes, UCrypto, UAccounts, ULog, UThread, SyncObjs, UBaseTypes, SysUtils,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF},
  UPCDataTypes;
{$I config.inc}

{

    Bank BlockChain:

    Safe Box content: (See Unit "UAccounts.pas" to see pascal code)
    +--------------+--------------------------------------------------+------------+------------+
    + BlockAccount + Each BlockAccount has N "Account"                +  Timestamp + Block Hash +
    +              +--------------------------------------------------+            +            +
    +              + Addr B0 + Public key +  Balance + updated + n_op +            +            +
    +              + Addr B1 + Public key +  Balance + updated + n_op +            +            +
    +              + ......                                           +            +            +
    +              + Addr B4 + Public key +  Balance + updated + n_op +            +            +
    +--------------+---------+----------------------------------------+------------+------------+
    +            0 +       0 + pk_aaaaaaa + 100.0000 +       0 +    0 + 1461701856 +   Sha256() +
    +              +       1 + pk_aaaaaaa +   0.0000 +       0 +    0 +            + = h1111111 +
    +              +       2 + pk_aaaaaaa +   0.0000 +       0 +    0 +            +            +
    +              +       3 + pk_aaaaaaa +   0.0000 +       0 +    0 +            +            +
    +              +       4 + pk_aaaaaaa +   0.0000 +       0 +    0 +            +            +
    +--------------+---------+----------------------------------------+------------+------------+
    +            1 +       5 + pk_bbbbbbb + 100.0000 +       0 +    0 + 1461702960 +   Sha256() +
    +              +       6 + pk_bbbbbbb +   0.0000 +       0 +    0 +            + = h2222222 +
    +              +       7 + pk_bbbbbbb +   0.0000 +       0 +    0 +            +            +
    +              +       8 + pk_bbbbbbb +   0.0000 +       0 +    0 +            +            +
    +              +       9 + pk_bbbbbbb +   0.0000 +       0 +    0 +            +            +
    +--------------+---------+----------------------------------------+------------+------------+
    +     ................                                                                      +
    +--------------+---------+----------------------------------------+------------+------------+
    +            5 +      25 + pk_bbbbbbb + 100.0000 +       0 +    0 + 1461713484 +   Sha256() +
    +              +      26 + pk_bbbbbbb +   0.0000 +       0 +    0 +            + = h3333333 +
    +              +      27 + pk_bbbbbbb +   0.0000 +       0 +    0 +            +            +
    +              +      28 + pk_bbbbbbb +   0.0000 +       0 +    0 +            +            +
    +              +      29 + pk_bbbbbbb +   0.0000 +       0 +    0 +            +            +
    +--------------+---------+----------------------------------------+------------+------------+
    +  Safe Box Hash  : Sha256(h1111111 + h2222222 + ... + h3333333) = sbh_A1                   +
    +-------------------------------------------------------------------------------------------+

    BlockChain:

    To generate a BlockChain (block X) we need the previous "Safe Box Hash"
    (the Safe Box Hash number X-1, generated when BlockChain X-1 was generated)
    Each BlockChain block generates a new "Safe Box" with a new "Safe Box Hash"

    With this method, Safe Box is unique after a BlockChain, so we can assume
    that a hard coded Safe Box X is the same that to load all previous BlockChain
    from 0 to X. Conclusion: It's not necessary historical operations (block chains)
    to work with Pascal Coin

    Some BlockChain fields:
    +-------+-----------------+----------+------+-----+-----+------------+--------+-------+---------------+---------------+-----------------+---------------+-----------------------+
    + Block + Account key     +  reward  + fee  + protocols + timestamp  + target + nonce + Miner Payload + safe box hash + operations hash + Proof of Work + Operations stream     +
    +-------+-----------------+----------+------+-----+-----+------------+--------+-------+---------------+---------------+-----------------+---------------+-----------------------+
    +     0 + (hard coded)    + 100.0000 +    0 +   1 +   0 + 1461701856 + trgt_1 +  ...  + (Hard coded)  +  (Hard coded) + Sha256(Operat.) + 000000C3F5... + Operations of block 0 +
    +-------+-----------------+----------+------+-----+-----+------------+--------+-------+---------------+---------------+-----------------+---------------+-----------------------+
    +     1 + hhhhhhhhhhhhhhh + 100.0000 +    0 +   1 +   0 + 1461701987 + trgt_1 +  ...  +      ...      + SFH block 0   + Sha256(Operat.) + 000000A987... + Operations of block 1 +
    +-------+-----------------+----------+------+-----+-----+------------+--------+-------+---------------+---------------+-----------------+---------------+-----------------------+
    +     2 + iiiiiiiiiiiiiii + 100.0000 + 0.43 +   1 +   0 + 1461702460 + trgt_1 +  ...  +      ...      + SFH block 1   + Sha256(Operat.) + 0000003A1C... + Operations of block 2 +
    +-------+-----------------+----------+------+-----+-----+------------+--------+-------+---------------+---------------+-----------------+---------------+-----------------------+
    +       .....                                                                                                                                                   +
    +-------+-----------------+----------+------+-----+-----+------------+--------+-------+---------------+---------------+-----------------+---------------+-----------------------+

    Considerations:
    - Account Key: Is a public key that will have all new generated Accounts of the Safe Box
    - Protocols are 2 values: First indicate protocol of this block, second future candidate protocol that is allowed by miner who made this. (For protocol upgrades)
    - Safe Box Has: Each Block of the Bloch Chain is made in base of a previous Safe Box. This value hard codes consistency
    - Operations Stream includes all the operations that will be made to the Safe Box after this block is generated. A hash value of Operations stream is "Operations Hash"

    Operations:

    Each Block of the Block Chain has its owns operations that will be used to change Safe Box after block is completed and included in BlockChain

    Operations of actual Protocol (version 1) can be one of this:
    - Transaction from 1 account to 1 account
    - Change AccountKey of an account
    - Recover balance from an unused account (lost keys)

    Each Operation has a Hash value that is used to generate "Operations Hash". Operations Hash is a Sha256 of all the Operations included
    inside it hashed like a Merkle Tree.

    In unit "UOpTransaction.pas" you can see how each Operation Works.

}

Type
  // Moved from UOpTransaction to here
  TOpChangeAccountInfoType = (public_key,account_name,account_type,list_for_public_sale,list_for_private_sale,delist);
  TOpChangeAccountInfoTypes = Set of TOpChangeAccountInfoType;

  // MultiOp... will allow a MultiOperation
  TMultiOpSender = Record
    Account : Cardinal;
    Amount : Int64;
    N_Operation : Cardinal;
    Payload : TRawBytes;
    Signature : TECDSA_SIG;
  end;
  TMultiOpSenders = Array of TMultiOpSender;
  TMultiOpReceiver = Record
    Account : Cardinal;
    Amount : Int64;
    Payload : TRawBytes;
  end;
  TMultiOpReceivers = Array of TMultiOpReceiver;
  TMultiOpChangeInfo = Record
    Account: Cardinal;
    N_Operation : Cardinal;
    Changes_type : TOpChangeAccountInfoTypes; // bits mask. $0001 = New account key , $0002 = New name , $0004 = New type
    New_Accountkey: TAccountKey;  // If (changes_mask and $0001)=$0001 then change account key
    New_Name: TRawBytes;          // If (changes_mask and $0002)=$0002 then change name
    New_Type: Word;               // If (changes_mask and $0004)=$0004 then change type
    Seller_Account : Int64;
    Account_Price : Int64;
    Locked_Until_Block : Cardinal;
    Fee: Int64;
    Signature: TECDSA_SIG;
  end;
  TMultiOpChangesInfo = Array of TMultiOpChangeInfo;

  TOperationResume = Record
    valid : Boolean;
    Block : Cardinal;
    NOpInsideBlock : Integer;
    OpType : Word;
    OpSubtype : Word;
    time : Cardinal;
    AffectedAccount : Cardinal;
    SignerAccount : Int64; // Is the account that executes this operation
    n_operation : Cardinal;
    DestAccount : Int64;   //
    SellerAccount : Int64; // Protocol 2 - only used when is a pay to transaction
    newKey : TAccountKey;
    OperationTxt : String;
    Amount : Int64;
    Fee : Int64;
    Balance : Int64;
    OriginalPayload : TRawBytes;
    PrintablePayload : String;
    OperationHash : TRawBytes;
    OperationHash_OLD : TRawBytes; // Will include old oeration hash value
    errors : String;
    // New on V3 for PIP-0017
    isMultiOperation : Boolean;
    Senders : TMultiOpSenders;
    Receivers : TMultiOpReceivers;
    Changers : TMultiOpChangesInfo;
  end;

  TPCBank = Class;
  TPCBankNotify = Class;
  TPCOperation = Class;
  TPCOperationClass = Class of TPCOperation;

  TOperationsResumeList = Class
  private
    FList : TPCThreadList<Pointer>;
    function GetOperationResume(index: Integer): TOperationResume;
  public
    Constructor Create;
    Destructor Destroy; override;
    Procedure Add(Const OperationResume : TOperationResume);
    Function Count : Integer;
    Procedure Delete(index : Integer);
    Procedure Clear;
    Property OperationResume[index : Integer] : TOperationResume read GetOperationResume; default;
  End;

  TOpReference = UInt64;
  TOpReferenceArray = Array of TopReference;

  { TPCOperation }

  TPCOperation = Class
  Private
  Protected
    FPrevious_Signer_updated_block: Cardinal;
    FPrevious_Destination_updated_block : Cardinal;
    FPrevious_Seller_updated_block : Cardinal;
    FHasValidSignature : Boolean;
    FUsedPubkeyForSignature : TECDSA_Public;
    FBufferedSha256 : TRawBytes;
    procedure InitializeData; virtual;
    function SaveOpToStream(Stream: TStream; SaveExtendedData : Boolean): Boolean; virtual; abstract;
    function LoadOpFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean; virtual; abstract;
    procedure FillOperationResume(Block : Cardinal; getInfoForAllAccounts : Boolean; Affected_account_number : Cardinal; var OperationResume : TOperationResume); virtual;
    Property Previous_Signer_updated_block : Cardinal read FPrevious_Signer_updated_block; // deprecated
    Property Previous_Destination_updated_block : Cardinal read FPrevious_Destination_updated_block; // deprecated
    Property Previous_Seller_updated_block : Cardinal read FPrevious_Seller_updated_block; // deprecated
    function IsValidECDSASignature(const PubKey: TECDSA_Public; current_protocol : Word; const Signature: TECDSA_SIG): Boolean;
    procedure CopyUsedPubkeySignatureFrom(SourceOperation : TPCOperation); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function GetBufferForOpHash(UseProtocolV2 : Boolean): TRawBytes; virtual;
    function DoOperation(AccountPreviousUpdatedBlock : TAccountPreviousBlockInfo; AccountTransaction : TPCSafeBoxTransaction; var errors: String): Boolean; virtual; abstract;
    procedure AffectedAccounts(list : TList<Cardinal>); virtual; abstract;
    class function OpType: Byte; virtual; abstract;
    Class Function OperationToOperationResume(Block : Cardinal; Operation : TPCOperation; getInfoForAllAccounts : Boolean; Affected_account_number : Cardinal; var OperationResume : TOperationResume) : Boolean; virtual;
    Function GetDigestToSign(current_protocol : Word) : TRawBytes; virtual; abstract;
    function OperationAmount : Int64; virtual; abstract;
    function OperationAmountByAccount(account : Cardinal) : Int64; virtual;
    function OperationFee: Int64; virtual; abstract;
    function OperationPayload : TRawBytes; virtual; abstract;
    function SignerAccount : Cardinal; virtual; abstract;
    procedure SignerAccounts(list : TList<Cardinal>); virtual;
    function IsSignerAccount(account : Cardinal) : Boolean; virtual;
    function IsAffectedAccount(account : Cardinal) : Boolean; virtual;
    function DestinationAccount : Int64; virtual;
    function SellerAccount : Int64; virtual;
    function N_Operation : Cardinal; virtual; abstract;
    function GetAccountN_Operation(account : Cardinal) : Cardinal; virtual;
    function SaveToNettransfer(Stream: TStream): Boolean;
    function LoadFromNettransfer(Stream: TStream): Boolean;
    function SaveToStorage(Stream: TStream): Boolean;
    function LoadFromStorage(Stream: TStream; LoadProtocolVersion : Word; APreviousUpdatedBlocks : TAccountPreviousBlockInfo): Boolean;
    Property HasValidSignature : Boolean read FHasValidSignature;
    Class function OperationHash_OLD(op : TPCOperation; Block : Cardinal) : TRawBytes;
    Class function OperationHashValid(op : TPCOperation; Block : Cardinal) : TRawBytes;
    class function IsValidOperationHash(const AOpHash : String) : Boolean;
    class function TryParseOperationHash(const AOpHash : String; var block, account, n_operation: Cardinal; var md160Hash : TRawBytes) : Boolean;
    Class function DecodeOperationHash(Const operationHash : TRawBytes; var block, account,n_operation : Cardinal; var md160Hash : TRawBytes) : Boolean;
    Class function EqualOperationHashes(Const operationHash1, operationHash2 : TRawBytes) : Boolean;
    Class function FinalOperationHashAsHexa(Const operationHash : TRawBytes) : String;
    class function OperationHashAsHexa(const operationHash : TRawBytes) : String;
    class function GetOpReferenceAccount(const opReference : TOpReference) : Cardinal;
    class function GetOpReferenceN_Operation(const opReference : TOpReference) : Cardinal;
    function Sha256 : TRawBytes;
    function GetOpReference : TOpReference;
    //
    function GetOperationStreamData : TBytes;
    class function GetOperationFromStreamData(StreamData : TBytes) : TPCOperation;
  End;

  TPCOperationStorage = Record
    ptrPCOperation : TPCOperation;
    locksCount : Integer;
  end;
  PPCOperationTStorage = ^TPCOperationStorage;

  { TPCOperationsStorage }

  // TPCOperationsStorage will be used as a global Operations storage useful when
  // operations are stored on TOperationsHashTree because will use only one instance
  // of operation used on multiple OperationsHashTree lists. For example when
  // propagating operations to connected nodes, will only use one instance
  TPCOperationsStorage = Class
  private
    FIntTotalNewOps : Integer;
    FIntTotalAdded : Integer;
    FIntTotalDeleted : Integer;
    FMaxLocksCount : Integer;
    FMaxLocksValue : Integer;
    FPCOperationsStorageList : TPCThreadList<Pointer>; // Lock thread to POperationTStorage list
    Function FindOrderedByPtrPCOperation(lockedThreadList : TList<Pointer>; const Value: TPCOperation; out Index: Integer): Boolean;
  protected
  public
    Constructor Create;
    Destructor Destroy; override;
    //
    function LockPCOperationsStorage : TList<Pointer>;
    procedure UnlockPCOperationsStorage;
    Function Count : Integer;
    procedure AddPCOperation(APCOperation : TPCOperation);
    procedure RemovePCOperation(APCOperation : TPCOperation);
    function FindPCOperation(APCOperation : TPCOperation) : Boolean;
    function FindPCOperationAndIncCounterIfFound(APCOperation : TPCOperation) : Boolean;
    class function PCOperationsStorage : TPCOperationsStorage;
    procedure GetStats(strings : TStrings);
  end;

  { TOperationsHashTree }

  TOperationsHashTree = Class
  private
    FListOrderedByAccountsData : TList<Pointer>;
    FListOrderedBySha256 : TList<Integer>; // Improvement TOperationsHashTree speed 2.1.6
    FListOrderedByOpReference : TList<Integer>;
    FHashTreeOperations : TPCThreadList<Pointer>; // Improvement TOperationsHashTree speed 2.1.6
    FHashTree: TRawBytes;
    FOnChanged: TNotifyEvent;
    FTotalAmount : Int64;
    FTotalFee : Int64;
    FMax0feeOperationsBySigner : Integer;
    function InternalCanAddOperationToHashTree(lockedThreadList : TList<Pointer>; op : TPCOperation) : Boolean;
    function InternalAddOperationToHashTree(list : TList<Pointer>; op : TPCOperation; CalcNewHashTree : Boolean) : Boolean;
    Function FindOrderedByOpReference(lockedThreadList : TList<Pointer>; const Value: TOpReference; var Index: Integer): Boolean;
    Function FindOrderedBySha(lockedThreadList : TList<Pointer>; const Value: TRawBytes; var Index: Integer): Boolean;
    Function FindOrderedByAccountData(lockedThreadList : TList<Pointer>; const account_number : Cardinal; var Index: Integer): Boolean;
    function GetHashTree: TRawBytes;
    procedure SetMax0feeOperationsBySigner(const Value: Integer);
  public
    Constructor Create;
    Destructor Destroy; Override;
    function CanAddOperationToHashTree(op : TPCOperation) : Boolean;
    function AddOperationToHashTree(op : TPCOperation) : Boolean;
    Procedure ClearHastThree;
    Property HashTree : TRawBytes read GetHashTree;
    Function OperationsCount : Integer;
    Function GetOperation(index : Integer) : TPCOperation;
    Function GetOperationsAffectingAccount(account_number : Cardinal; List : TList<Cardinal>) : Integer;
    Procedure CopyFromHashTree(Sender : TOperationsHashTree);
    Property TotalAmount : Int64 read FTotalAmount;
    Property TotalFee : Int64 read FTotalFee;
    function SaveOperationsHashTreeToStream(Stream: TStream; SaveToStorage : Boolean): Boolean;
    function LoadOperationsHashTreeFromStream(Stream: TStream; LoadingFromStorage : Boolean; LoadProtocolVersion : Word; PreviousUpdatedBlocks : TAccountPreviousBlockInfo; var errors : String): Boolean;
    function IndexOfOperation(op : TPCOperation) : Integer;
    function CountOperationsBySameSignerWithoutFee(account_number : Cardinal) : Integer;
    Procedure Delete(index : Integer);
    function IndexOfOpReference(const opReference : TOpReference) : Integer;
    procedure RemoveByOpReference(const opReference : TOpReference);
    Property OnChanged : TNotifyEvent read FOnChanged write FOnChanged;
    Property Max0feeOperationsBySigner : Integer Read FMax0feeOperationsBySigner write SetMax0feeOperationsBySigner;
    procedure MarkVerifiedECDSASignatures(operationsHashTreeToMark : TOperationsHashTree);
  End;

  { TPCOperationsComp }

  TPCOperationsComp = Class
  private
    FBank: TPCBank;
    FSafeBoxTransaction : TPCSafeBoxTransaction;
    FOperationBlock: TOperationBlock;
    FOperationsHashTree : TOperationsHashTree;
    FDigest_Part1 : TRawBytes;
    FDigest_Part2_Payload : TRawBytes;
    FDigest_Part3 : TRawBytes;
    FIsOnlyOperationBlock: Boolean;
    FStreamPoW : TMemoryStream;
    FDisableds : Integer;
    FOperationsLock : TPCCriticalSection;
    FPreviousUpdatedBlocks : TAccountPreviousBlockInfo; // New Protocol V3 struct to store previous updated blocks
    function GetOperation(index: Integer): TPCOperation;
    procedure SetBank(const value: TPCBank);
    procedure SetnOnce(const value: Cardinal);
    procedure Settimestamp(const value: Cardinal);
    function GetnOnce: Cardinal;
    function Gettimestamp: Cardinal;
    procedure SetAccountKey(const value: TAccountKey);
    function GetAccountKey: TAccountKey;
    Procedure Calc_Digest_Parts;
    Procedure Calc_Digest_Part3;
    Procedure CalcProofOfWork(fullcalculation : Boolean; var PoW: TRawBytes);
    function GetBlockPayload: TRawBytes;
    procedure SetBlockPayload(const Value: TRawBytes);
    procedure OnOperationsHashTreeChanged(Sender : TObject);
  protected
    function SaveBlockToStreamExt(save_only_OperationBlock : Boolean; Stream: TStream; SaveToStorage : Boolean): Boolean;
    function LoadBlockFromStreamExt(Stream: TStream; LoadingFromStorage : Boolean; var errors: String): Boolean;
  public
    Constructor Create(ABank: TPCBank);
    Destructor Destroy; Override;
    Procedure CopyFromExceptAddressKey(Operations : TPCOperationsComp);
    Procedure CopyFrom(Operations : TPCOperationsComp);
    Function AddOperation(Execute : Boolean; op: TPCOperation; var errors: String): Boolean;
    Function AddOperations(operations: TOperationsHashTree; var errors: String): Integer;
    Property Operation[index: Integer]: TPCOperation read GetOperation;
    Property bank: TPCBank read FBank write SetBank;
    Procedure Clear(DeleteOperations : Boolean);
    Function Count: Integer;
    Property OperationBlock: TOperationBlock read FOperationBlock;
    Class Function OperationBlockToText(const OperationBlock: TOperationBlock) : String;
    Class Function SaveOperationBlockToStream(Const OperationBlock: TOperationBlock; Stream: TStream) : Boolean;
    Property AccountKey: TAccountKey read GetAccountKey write SetAccountKey;
    Property nonce: Cardinal read GetnOnce write SetnOnce;
    Property timestamp: Cardinal read Gettimestamp write Settimestamp;
    Property BlockPayload : TRawBytes read GetBlockPayload write SetBlockPayload;
    function Update_And_RecalcPOW(newNOnce, newTimestamp : Cardinal; newBlockPayload : TRawBytes) : Boolean;
    procedure UpdateTimestamp;
    function SaveBlockToStorage(Stream: TStream): Boolean;
    function SaveBlockToStream(save_only_OperationBlock : Boolean; Stream: TStream): Boolean;
    function LoadBlockFromStorage(Stream: TStream; var errors: String): Boolean;
    function LoadBlockFromStream(Stream: TStream; var errors: String): Boolean;
    //
    Function GetMinerRewardPseudoOperation : TOperationResume;
    Function ValidateOperationBlock(var errors : String) : Boolean;
    Property IsOnlyOperationBlock : Boolean read FIsOnlyOperationBlock;
    Procedure Lock;
    Procedure Unlock;
    //
    Procedure SanitizeOperations;

    Class Function RegisterOperationClass(OpClass: TPCOperationClass): Boolean;
    Class Function IndexOfOperationClass(OpClass: TPCOperationClass): Integer;
    Class Function IndexOfOperationClassByOpType(OpType: Cardinal): Integer;
    Class Function GetOperationClassByOpType(OpType: Cardinal): TPCOperationClass;
    Class Function GetFirstBlock : TOperationBlock;
    Class Function EqualsOperationBlock(Const OperationBlock1,OperationBlock2 : TOperationBlock):Boolean;
    //
    Property SafeBoxTransaction : TPCSafeBoxTransaction read FSafeBoxTransaction;
    Property OperationsHashTree : TOperationsHashTree read FOperationsHashTree;
    Property PoW_Digest_Part1 : TRawBytes read FDigest_Part1;
    Property PoW_Digest_Part2_Payload : TRawBytes read FDigest_Part2_Payload;
    Property PoW_Digest_Part3 : TRawBytes read FDigest_Part3;
    //
    Property PreviousUpdatedBlocks : TAccountPreviousBlockInfo read FPreviousUpdatedBlocks; // New Protocol V3 struct to store previous updated blocks
  End;

  TPCBankLog = procedure(sender: TPCBank; Operations: TPCOperationsComp; Logtype: TLogType ; const Logtxt: String) of object;

  TPCBankNotify = Class(TComponent)
  private
    FOnNewBlock: TNotifyEvent;
    FBank: TPCBank;
    procedure SetBank(const Value: TPCBank);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); Override;
    Procedure NotifyNewBlock;
  public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Property Bank : TPCBank read FBank write SetBank;
    Property OnNewBlock : TNotifyEvent read FOnNewBlock write FOnNewBlock;
  End;

  TOrphan = RawByteString;

  { TStorage }

  TStorage = Class(TComponent)
  private
    FOrphan: TOrphan;
    FBank : TPCBank;
    FReadOnly: Boolean;
    procedure SetBank(const Value: TPCBank);
  protected
    FIsMovingBlockchain : Boolean;
    procedure SetOrphan(const Value: TOrphan); virtual;
    procedure SetReadOnly(const Value: Boolean); virtual;
    Function DoLoadBlockChain(Operations : TPCOperationsComp; Block : Cardinal) : Boolean; virtual; abstract;
    Function DoSaveBlockChain(Operations : TPCOperationsComp) : Boolean; virtual; abstract;
    Function DoMoveBlockChain(StartBlock : Cardinal; Const DestOrphan : TOrphan; DestStorage : TStorage) : Boolean; virtual; abstract;
    Function DoSaveBank : Boolean; virtual; abstract;
    Function DoRestoreBank(max_block : Int64; restoreProgressNotify : TProgressNotify) : Boolean; virtual; abstract;
    Procedure DoDeleteBlockChainBlocks(StartingDeleteBlock : Cardinal); virtual; abstract;
    Function DoBlockExists(Block : Cardinal) : Boolean; virtual; abstract;
    function GetFirstBlockNumber: Int64; virtual; abstract;
    function GetLastBlockNumber: Int64; virtual; abstract;
    function DoInitialize:Boolean; virtual; abstract;
    Function DoCreateSafeBoxStream(blockCount : Cardinal) : TStream; virtual; abstract;
    Procedure DoEraseStorage; virtual; abstract;
    Procedure DoSavePendingBufferOperations(OperationsHashTree : TOperationsHashTree); virtual; abstract;
    Procedure DoLoadPendingBufferOperations(OperationsHashTree : TOperationsHashTree); virtual; abstract;
  public
    Function LoadBlockChainBlock(Operations : TPCOperationsComp; Block : Cardinal) : Boolean;
    Function SaveBlockChainBlock(Operations : TPCOperationsComp) : Boolean;
    Function MoveBlockChainBlocks(StartBlock : Cardinal; Const DestOrphan : TOrphan; DestStorage : TStorage) : Boolean;
    Procedure DeleteBlockChainBlocks(StartingDeleteBlock : Cardinal);
    Function SaveBank(forceSave : Boolean) : Boolean;
    Function RestoreBank(max_block : Int64; restoreProgressNotify : TProgressNotify = Nil) : Boolean;
    Constructor Create(AOwner : TComponent); Override;
    Property Orphan : TOrphan read FOrphan write SetOrphan;
    Property ReadOnly : Boolean read FReadOnly write SetReadOnly;
    Property Bank : TPCBank read FBank write SetBank;
    Procedure CopyConfiguration(Const CopyFrom : TStorage); virtual;
    Property FirstBlock : Int64 read GetFirstBlockNumber;
    Property LastBlock : Int64 read GetLastBlockNumber;
    Function Initialize : Boolean;
    Function CreateSafeBoxStream(blockCount : Cardinal) : TStream;
    Function HasUpgradedToVersion2 : Boolean; virtual; abstract;
    Procedure CleanupVersion1Data; virtual; abstract;
    Procedure EraseStorage;
    Procedure SavePendingBufferOperations(OperationsHashTree : TOperationsHashTree);
    Procedure LoadPendingBufferOperations(OperationsHashTree : TOperationsHashTree);
    Function BlockExists(Block : Cardinal) : Boolean;
  End;

  TStorageClass = Class of TStorage;

  { TPCBank }

  TPCBank = Class(TComponent)
  private
    FStorage : TStorage;
    FSafeBox: TPCSafeBox;
    FLastBlockCache : TPCOperationsComp;
    FLastOperationBlock: TOperationBlock;
    FIsRestoringFromFile: Boolean;
    FUpgradingToV2: Boolean;
    FOnLog: TPCBankLog;
    FBankLock: TPCCriticalSection;
    FNotifyList : TList<TPCBankNotify>;
    FStorageClass: TStorageClass;
    function GetStorage: TStorage;
    procedure SetStorageClass(const Value: TStorageClass);
  public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Function BlocksCount: Cardinal;
    Function AccountsCount : Cardinal;
    procedure AssignTo(Dest: TPersistent); Override;
    function GetActualTargetSecondsAverage(BackBlocks : Cardinal): Real;
    function GetTargetSecondsAverage(FromBlock,BackBlocks : Cardinal): Real;
    function LoadBankFromStream(Stream : TStream; useSecureLoad : Boolean; checkSafeboxHash : TRawBytes; previousCheckedSafebox : TPCSafebox; progressNotify : TProgressNotify; var errors : String) : Boolean;
    Procedure Clear;
    Function LoadOperations(Operations : TPCOperationsComp; Block : Cardinal) : Boolean;
    Property SafeBox : TPCSafeBox read FSafeBox;
    Function AddNewBlockChainBlock(Operations: TPCOperationsComp; MaxAllowedTimestamp : Cardinal; var newBlock: TBlockAccount; var errors: String): Boolean;
    Procedure DiskRestoreFromOperations(max_block : Int64; restoreProgressNotify : TProgressNotify = Nil);
    Procedure UpdateValuesFromSafebox;
    Procedure NewLog(Operations: TPCOperationsComp; Logtype: TLogType; const Logtxt: String);
    Property OnLog: TPCBankLog read FOnLog write FOnLog;
    Property LastOperationBlock : TOperationBlock read FLastOperationBlock; // TODO: Use
    Property Storage : TStorage read GetStorage;
    Property StorageClass : TStorageClass read FStorageClass write SetStorageClass;
    Function IsReady(Var CurrentProcess : String) : Boolean;
    Property LastBlockFound : TPCOperationsComp read FLastBlockCache;
    Property UpgradingToV2 : Boolean read FUpgradingToV2;
  End;

Const
  CT_TOperationResume_NUL : TOperationResume = (valid:false;Block:0;NOpInsideBlock:-1;OpType:0;OpSubtype:0;time:0;AffectedAccount:0;SignerAccount:-1;n_operation:0;DestAccount:-1;SellerAccount:-1;newKey:(EC_OpenSSL_NID:0;x:Nil;y:Nil);OperationTxt:'';Amount:0;Fee:0;Balance:0;OriginalPayload:Nil;PrintablePayload:'';OperationHash:Nil;OperationHash_OLD:Nil;errors:'';isMultiOperation:False;Senders:Nil;Receivers:Nil;changers:Nil);
  CT_TMultiOpSender_NUL : TMultiOpSender =  (Account:0;Amount:0;N_Operation:0;Payload:Nil;Signature:(r:Nil;s:Nil));
  CT_TMultiOpReceiver_NUL : TMultiOpReceiver = (Account:0;Amount:0;Payload:Nil);
  CT_TMultiOpChangeInfo_NUL : TMultiOpChangeInfo = (Account:0;N_Operation:0;Changes_type:[];New_Accountkey:(EC_OpenSSL_NID:0;x:Nil;y:Nil);New_Name:Nil;New_Type:0;Seller_Account:-1;Account_Price:-1;Locked_Until_Block:0;Fee:0;Signature:(r:Nil;s:Nil));
  CT_TOpChangeAccountInfoType_Txt : Array[Low(TOpChangeAccountInfoType)..High(TOpChangeAccountInfoType)] of String = ('public_key','account_name','account_type','list_for_public_sale','list_for_private_sale','delist');

implementation

uses
  Variants,
  UTime, UConst, UOpTransaction;

{ TPCOperationsStorage }

var
   _PCOperationsStorage : TPCOperationsStorage;

function TPCOperationsStorage.FindOrderedByPtrPCOperation(lockedThreadList: TList<Pointer>; const Value: TPCOperation; out Index: Integer): Boolean;
var L, H, I: Integer;
  C : PtrInt;
begin
  Result := False;
  L := 0;
  H := lockedThreadList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := PtrInt(PPCOperationTStorage(lockedThreadList[I])^.ptrPCOperation) - PtrInt(Value);
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

constructor TPCOperationsStorage.Create;
begin
  FPCOperationsStorageList := TPCThreadList<Pointer>.Create(ClassName);
  FIntTotalNewOps := 0;
  FIntTotalAdded := 0;
  FIntTotalDeleted := 0;
  FMaxLocksCount := 0;
  FMaxLocksValue := 0;
end;

destructor TPCOperationsStorage.Destroy;
Var list : TList<Pointer>;
  P : PPCOperationTStorage;
  i : Integer;
  pc : TPCOperation;
begin
  list := LockPCOperationsStorage;
  try
    for i:=0 to list.Count-1 do begin
      P := list[i];
      pc := P^.ptrPCOperation;
      P^.ptrPCOperation := Nil;
      P^.locksCount:=-1;
      pc.Free;
      Dispose(P);
    end;
    inc(FIntTotalDeleted,list.Count);
  finally
    list.Clear;
    UnlockPCOperationsStorage;
  end;
  FreeAndNil(FPCOperationsStorageList);
  inherited Destroy;
end;

function TPCOperationsStorage.LockPCOperationsStorage: TList<Pointer>;
begin
  Result := FPCOperationsStorageList.LockList;
end;

procedure TPCOperationsStorage.UnlockPCOperationsStorage;
begin
  FPCOperationsStorageList.UnlockList;
end;

function TPCOperationsStorage.Count: Integer;
var list : TList<Pointer>;
begin
  list := LockPCOperationsStorage;
  try
    Result := list.Count;
  finally
    UnlockPCOperationsStorage;
  end;
end;

procedure TPCOperationsStorage.AddPCOperation(APCOperation: TPCOperation);
var P : PPCOperationTStorage;
  list : TList<Pointer>;
  iPos : Integer;
begin
  list := LockPCOperationsStorage;
  try
    if FindOrderedByPtrPCOperation(list,APCOperation,iPos) then begin
      P := list[iPos];
    end else begin
      New(P);
      P^.locksCount:=0;
      P^.ptrPCOperation := APCOperation;
      list.Insert(iPos,P);
      inc(FIntTotalNewOps);
    end;
    inc(P^.locksCount);
    inc(FIntTotalAdded);
    if (P^.locksCount>FMaxLocksValue) then begin
      FMaxLocksValue:=P^.locksCount;
      FMaxLocksCount:=0;
    end;
    inc(FMaxLocksCount);
  finally
    UnlockPCOperationsStorage;
  end;
end;

procedure TPCOperationsStorage.RemovePCOperation(APCOperation: TPCOperation);
var P : PPCOperationTStorage;
  list : TList<Pointer>;
  iPos : Integer;
begin
  list := LockPCOperationsStorage;
  try
    if FindOrderedByPtrPCOperation(list,APCOperation,iPos) then begin
      P := list[iPos];
      Dec(P^.locksCount);
      if (P^.locksCount<=0) then begin
        // Remove
        list.Delete(iPos);
        P^.ptrPCOperation := Nil;
        Dispose(P);
        APCOperation.Free;
      end;
      inc(FIntTotalDeleted);
    end else begin
      TLog.NewLog(lterror,ClassName,'ERROR DEV 20181218-2 Operation not found in storage to remove: '+APCOperation.ToString);
    end;
  finally
    UnlockPCOperationsStorage;
  end;
end;

function TPCOperationsStorage.FindPCOperation(APCOperation: TPCOperation): Boolean;
var list : TList<Pointer>;
  iPos : Integer;
begin
  list := LockPCOperationsStorage;
  Try
    Result := FindOrderedByPtrPCOperation(list,APCOperation,iPos);
  finally
    UnlockPCOperationsStorage;
  end;
end;

function TPCOperationsStorage.FindPCOperationAndIncCounterIfFound(APCOperation: TPCOperation): Boolean;
var list : TList<Pointer>;
  iPos : Integer;
begin
  list := LockPCOperationsStorage;
  Try
    Result := FindOrderedByPtrPCOperation(list,APCOperation,iPos);
    if Result then begin
      Inc(PPCOperationTStorage(list[iPos])^.locksCount);
      inc(FIntTotalAdded);
      if (PPCOperationTStorage(list[iPos])^.locksCount>FMaxLocksValue) then begin
        FMaxLocksValue:=PPCOperationTStorage(list[iPos])^.locksCount;
        FMaxLocksCount:=0;
      end;
      inc(FMaxLocksCount);
    end;
  finally
    UnlockPCOperationsStorage;
  end;
end;

class function TPCOperationsStorage.PCOperationsStorage: TPCOperationsStorage;
begin
  Result := _PCOperationsStorage;
end;

procedure TPCOperationsStorage.GetStats(strings: TStrings);
var list : TList<Pointer>;
  i : Integer;
  P : PPCOperationTStorage;
begin
  list := LockPCOperationsStorage;
  try
    strings.Add(Format('%s Operations:%d NewAdded:%d Added:%d Deleted:%d',[ClassName,list.Count,FIntTotalNewOps,FIntTotalAdded,FIntTotalDeleted]));
    strings.Add(Format('MaxLocks:%d MaxLocksCount:%d',[FMaxLocksValue,FMaxLocksCount]));
    for i:=0 to list.Count-1 do begin
      P := PPCOperationTStorage(list[i]);
      strings.Add(Format('%d %s',[P^.locksCount,P^.ptrPCOperation.ToString]));
    end;
  finally
    UnlockPCOperationsStorage;
  end;
end;

{ TPCBank }

function TPCBank.AccountsCount: Cardinal;
begin
  Result := FSafeBox.AccountsCount;
end;

function TPCBank.AddNewBlockChainBlock(Operations: TPCOperationsComp; MaxAllowedTimestamp : Cardinal; var newBlock: TBlockAccount; var errors: String): Boolean;
Var i : Integer;
begin
  TPCThread.ProtectEnterCriticalSection(Self,FBankLock);
  Try
    Result := False;
    errors := '';
    Operations.Lock; // New Protection
    Try
      If Not Operations.ValidateOperationBlock(errors) then begin
        exit;
      end;
      if (Operations.OperationBlock.block > 0) then begin
        if ((MaxAllowedTimestamp>0) And (Operations.OperationBlock.timestamp>MaxAllowedTimestamp)) then begin
          errors := 'Invalid timestamp (Future time: New timestamp '+Inttostr(Operations.OperationBlock.timestamp)+' > max allowed '+inttostr(MaxAllowedTimestamp)+')';
          exit;
        end;
      end;
      // Ok, include!
      // WINNER !!!
      // Congrats!

      if Not Operations.SafeBoxTransaction.Commit(Operations.OperationBlock,errors) then begin
        exit;
      end;

      newBlock := SafeBox.Block(SafeBox.BlocksCount-1);

      // Initialize values
      FLastOperationBlock := Operations.OperationBlock;
      // log it!
      NewLog(Operations, ltupdate,
        Format('New block height:%d nOnce:%d timestamp:%d Operations:%d Fee:%d SafeBoxBalance:%d=%d PoW:%s Operations previous Safe Box hash:%s Future old Safe Box hash for next block:%s',
          [ Operations.OperationBlock.block,Operations.OperationBlock.nonce,Operations.OperationBlock.timestamp,
            Operations.Count,
            Operations.OperationBlock.fee,
            SafeBox.TotalBalance,
            Operations.SafeBoxTransaction.TotalBalance,
            TCrypto.ToHexaString(Operations.OperationBlock.proof_of_work),
            TCrypto.ToHexaString(Operations.OperationBlock.initial_safe_box_hash),
            TCrypto.ToHexaString(SafeBox.SafeBoxHash)]));
      // Save Operations to disk
      if Not FIsRestoringFromFile then begin
        Storage.SaveBlockChainBlock(Operations);
      end;
      FLastBlockCache.CopyFrom(Operations);
      Operations.Clear(true);
      Result := true;
    Finally
      if Not Result then begin
        NewLog(Operations, lterror, 'Invalid new block '+inttostr(Operations.OperationBlock.block)+': ' + errors+ ' > '+TPCOperationsComp.OperationBlockToText(Operations.OperationBlock));
      end;
      Operations.Unlock;
    End;
  Finally
    FBankLock.Release;
  End;
  if Result then begin
    for i := 0 to FNotifyList.Count - 1 do begin
      TPCBankNotify(FNotifyList.Items[i]).NotifyNewBlock;
    end;
  end;
end;

procedure TPCBank.AssignTo(Dest: TPersistent);
var d : TPCBank;
begin
  if (Not (Dest is TPCBank)) then begin
    inherited;
    exit;
  end;
  if (Self=Dest) then exit;

  d := TPCBank(Dest);
  d.SafeBox.CopyFrom(SafeBox);
  d.FLastOperationBlock := FLastOperationBlock;
  d.FIsRestoringFromFile := FIsRestoringFromFile;
  d.FLastBlockCache.CopyFrom( FLastBlockCache );
end;

function TPCBank.BlocksCount: Cardinal;
begin
  Result := SafeBox.BlocksCount;
end;

procedure TPCBank.Clear;
begin
  SafeBox.Clear;
  FLastOperationBlock := TPCOperationsComp.GetFirstBlock;
  FLastOperationBlock.initial_safe_box_hash := TPCSafeBox.InitialSafeboxHash; // Genesis hash
  FLastBlockCache.Clear(true);
  {$IFDEF HIGHLOG}NewLog(Nil, ltdebug, 'Clear Bank');{$ENDIF}
end;

constructor TPCBank.Create(AOwner: TComponent);
begin
  inherited;
  FStorage := Nil;
  FStorageClass := Nil;
  FBankLock := TPCCriticalSection.Create('TPCBank_BANKLOCK');
  FIsRestoringFromFile := False;
  FOnLog := Nil;
  FSafeBox := TPCSafeBox.Create;
  FNotifyList := TList<TPCBankNotify>.Create;
  FLastBlockCache := TPCOperationsComp.Create(Nil);
  FIsRestoringFromFile:=False;
  FUpgradingToV2:=False;
  Clear;
end;

destructor TPCBank.Destroy;
var step : String;
begin
  Try
    step := 'Deleting critical section';
    FreeAndNil(FBankLock);
    step := 'Clear';
    Clear;
    step := 'Destroying LastBlockCache';
    FreeAndNil(FLastBlockCache);
    step := 'Destroying SafeBox';
    FreeAndNil(FSafeBox);
    step := 'Destroying NotifyList';
    FreeAndNil(FNotifyList);
    step := 'Destroying Storage';
    FreeAndNil(FStorage);
    step := 'inherited';
    inherited;
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,Classname,'Error destroying Bank step: '+step+' Errors ('+E.ClassName+'): ' +E.Message);
      Raise;
    end;
  End;
end;

procedure TPCBank.DiskRestoreFromOperations(max_block : Int64; restoreProgressNotify : TProgressNotify = Nil);
Var
  errors: String;
  newBlock: TBlockAccount;
  Operations: TPCOperationsComp;
  n : Int64;
  tc : TTickCount;
begin
  if FIsRestoringFromFile then begin
    TLog.NewLog(lterror,Classname,'Is Restoring!!!');
    raise Exception.Create('Is restoring!');
  end;
  tc := TPlatform.GetTickCount;
  TPCThread.ProtectEnterCriticalSection(Self,FBankLock);
  try
    FUpgradingToV2 := NOT Storage.HasUpgradedToVersion2;
    FIsRestoringFromFile := true;
    try
      Clear;
      Storage.Initialize;
      If (max_block<Storage.LastBlock) then n := max_block
      else n := Storage.LastBlock;
      Storage.RestoreBank(n,restoreProgressNotify);
      // Restore last blockchain
      if (BlocksCount>0) And (SafeBox.CurrentProtocol=CT_PROTOCOL_1) then begin
        if Not Storage.LoadBlockChainBlock(FLastBlockCache,BlocksCount-1) then begin
          NewLog(nil,lterror,'Cannot find blockchain '+inttostr(BlocksCount-1)+' so cannot accept bank current block '+inttostr(BlocksCount));
          Clear;
        end else begin
          FLastOperationBlock := FLastBlockCache.OperationBlock;
        end;
      end;
      NewLog(Nil, ltinfo,'Start restoring from disk operations (Max '+inttostr(max_block)+') BlockCount: '+inttostr(BlocksCount)+' Orphan: ' +Storage.Orphan);
      Operations := TPCOperationsComp.Create(Self);
      try
        while ((BlocksCount<=max_block)) do begin
          if Storage.BlockExists(BlocksCount) then begin
            if Storage.LoadBlockChainBlock(Operations,BlocksCount) then begin
              SetLength(errors,0);
              if Not AddNewBlockChainBlock(Operations,0,newBlock,errors) then begin
                NewLog(Operations, lterror,'Error restoring block: ' + Inttostr(BlocksCount)+ ' Errors: ' + errors);
                Storage.DeleteBlockChainBlocks(BlocksCount);
                break;
              end else begin
                // To prevent continuous saving...
                if ((BlocksCount+(CT_BankToDiskEveryNBlocks*2)) >= Storage.LastBlock ) or
                   ((BlocksCount MOD (CT_BankToDiskEveryNBlocks*10))=0) then begin
                  Storage.SaveBank(False);
                end;
                if (Assigned(restoreProgressNotify)) And (TPlatform.GetElapsedMilliseconds(tc)>1000) then begin
                  tc := TPlatform.GetTickCount;
                  restoreProgressNotify(Self,Format('Reading blockchain block %d/%d',[Operations.OperationBlock.block,Storage.LastBlock]),BlocksCount,Storage.LastBlock);
                end;
              end;
            end else break;
          end else break;
        end;
        if FUpgradingToV2 then Storage.CleanupVersion1Data;
      finally
        Operations.Free;
      end;
      NewLog(Nil, ltinfo,'End restoring from disk operations (Max '+inttostr(max_block)+') Orphan: ' + Storage.Orphan+' Restored '+Inttostr(BlocksCount)+' blocks');
    finally
      FIsRestoringFromFile := False;
      FUpgradingToV2 := false;
    end;
  finally
    FBankLock.Release;
  end;
end;

procedure TPCBank.UpdateValuesFromSafebox;
Var aux : String;
  i : Integer;
begin
  { Will update current Bank state based on Safebox state
    Used when commiting a Safebox or rolling back }
  Try
    TPCThread.ProtectEnterCriticalSection(Self,FBankLock);
    try
      FLastBlockCache.Clear(True);
      FLastOperationBlock := TPCOperationsComp.GetFirstBlock;
      FLastOperationBlock.initial_safe_box_hash := TPCSafeBox.InitialSafeboxHash; // Genesis hash
      If FSafeBox.BlocksCount>0 then begin
        Storage.Initialize;
        If Storage.LoadBlockChainBlock(FLastBlockCache,FSafeBox.BlocksCount-1) then begin
          FLastOperationBlock := FLastBlockCache.OperationBlock;
        end else begin
          aux := 'Cannot read last operations block '+IntToStr(FSafeBox.BlocksCount-1)+' from blockchain';
          TLog.NewLog(lterror,ClassName,aux);
          Raise Exception.Create(aux);
        end;
      end;
      TLog.NewLog(ltinfo,ClassName,Format('Updated Bank with Safebox values. Current block:%d ',[FLastOperationBlock.block]));
    finally
      FBankLock.Release;
    end;
  finally
    for i := 0 to FNotifyList.Count - 1 do begin
      TPCBankNotify(FNotifyList.Items[i]).NotifyNewBlock;
    end;
  end;
end;

function TPCBank.GetActualTargetSecondsAverage(BackBlocks: Cardinal): Real;
Var ts1, ts2: Int64;
begin
  if BlocksCount>BackBlocks then begin
    ts1 := SafeBox.Block(BlocksCount-1).blockchainInfo.timestamp;
    ts2 := SafeBox.Block(BlocksCount-BackBlocks-1).blockchainInfo.timestamp;
  end else if (BlocksCount>1) then begin
    ts1 := SafeBox.Block(BlocksCount-1).blockchainInfo.timestamp;
    ts2 := SafeBox.Block(0).blockchainInfo.timestamp;
    BackBlocks := BlocksCount-1;
  end else begin
    Result := 0;
    exit;
  end;
  Result := (ts1 - ts2) / BackBlocks;
end;

function TPCBank.GetTargetSecondsAverage(FromBlock, BackBlocks: Cardinal): Real;
Var ts1, ts2: Int64;
begin
  If FromBlock>=BlocksCount then begin
    Result := 0;
    exit;
  end;
  if FromBlock>BackBlocks then begin
    ts1 := SafeBox.Block(FromBlock-1).blockchainInfo.timestamp;
    ts2 := SafeBox.Block(FromBlock-BackBlocks-1).blockchainInfo.timestamp;
  end else if (FromBlock>1) then begin
    ts1 := SafeBox.Block(FromBlock-1).blockchainInfo.timestamp;
    ts2 := SafeBox.Block(0).blockchainInfo.timestamp;
    BackBlocks := FromBlock-1;
  end else begin
    Result := 0;
    exit;
  end;
  Result := (ts1 - ts2) / BackBlocks;
end;

function TPCBank.GetStorage: TStorage;
begin
  if Not Assigned(FStorage) then begin
    if Not Assigned(FStorageClass) then raise Exception.Create('StorageClass not defined');
    FStorage := FStorageClass.Create(Self);
    FStorage.Bank := Self;
  end;
  Result := FStorage;
end;

function TPCBank.IsReady(Var CurrentProcess: String): Boolean;
begin
  Result := false;
  CurrentProcess := '';
  if FIsRestoringFromFile then begin
    if FUpgradingToV2 then
      CurrentProcess := 'Migrating to version 2 format'
    else
      CurrentProcess := 'Restoring from file'
  end else Result := true;
end;

function TPCBank.LoadBankFromStream(Stream: TStream; useSecureLoad : Boolean; checkSafeboxHash : TRawBytes; previousCheckedSafebox : TPCSafebox; progressNotify : TProgressNotify; var errors: String): Boolean;
Var LastReadBlock : TBlockAccount;
  i : Integer;
  auxSB : TPCSafeBox;
begin
  auxSB := Nil;
  Try
    If useSecureLoad then begin
      // When on secure load will load Stream in a separate SafeBox, changing only real SafeBox if successfully
      auxSB := TPCSafeBox.Create;
      Result := auxSB.LoadSafeBoxFromStream(Stream,true,checkSafeboxHash,progressNotify,previousCheckedSafebox,LastReadBlock,errors);
      If Not Result then Exit;
    end;
    TPCThread.ProtectEnterCriticalSection(Self,FBankLock);
    try
      If Assigned(auxSB) then begin
        SafeBox.CopyFrom(auxSB);
      end else begin
        Result := SafeBox.LoadSafeBoxFromStream(Stream,False,checkSafeboxHash,progressNotify,previousCheckedSafebox,LastReadBlock,errors);
      end;
      If Not Result then exit;
      If SafeBox.BlocksCount>0 then FLastOperationBlock := SafeBox.Block(SafeBox.BlocksCount-1).blockchainInfo
      else begin
        FLastOperationBlock := TPCOperationsComp.GetFirstBlock;
        FLastOperationBlock.initial_safe_box_hash := TPCSafeBox.InitialSafeboxHash; // Genesis hash
      end;
    finally
      FBankLock.Release;
    end;
    for i := 0 to FNotifyList.Count - 1 do begin
      TPCBankNotify(FNotifyList.Items[i]).NotifyNewBlock;
    end;
  finally
    If Assigned(auxSB) then auxSB.Free;
  end;
end;

function TPCBank.LoadOperations(Operations: TPCOperationsComp; Block: Cardinal): Boolean;
begin
  TPCThread.ProtectEnterCriticalSection(Self,FBankLock);
  try
    if (Block>0) AND (Block=FLastBlockCache.OperationBlock.block) then begin
      // Same as cache, sending cache
      Operations.CopyFrom(FLastBlockCache);
      Result := true;
    end else begin
      Result := Storage.LoadBlockChainBlock(Operations,Block);
    end;
  finally
    FBankLock.Release;
  end;
end;

procedure TPCBank.NewLog(Operations: TPCOperationsComp; Logtype: TLogType; const Logtxt: String);
var s : String;
begin
  if Assigned(Operations) then s := Operations.ClassName
  else s := Classname;
  TLog.NewLog(Logtype,s,Logtxt);
  if Assigned(FOnLog) then
    FOnLog(Self, Operations, Logtype, Logtxt);
end;

procedure TPCBank.SetStorageClass(const Value: TStorageClass);
begin
  if FStorageClass=Value then exit;
  FStorageClass := Value;
  if Assigned(FStorage) then FreeAndNil(FStorage);
end;

{ TPCOperationsComp }

var
  _OperationsClass: Array of TPCOperationClass;

function TPCOperationsComp.AddOperation(Execute: Boolean; op: TPCOperation; var errors: String): Boolean;
var i : Integer;
  auxs : String;
Begin
  Lock;
  Try
    errors := '';
    Result := False;
    if Execute then begin
      if (FBank = Nil) then begin
        errors := 'No Bank';
        exit;
      end;
      if (FBank.BlocksCount<>OperationBlock.block) then begin
        errors := 'Bank blockcount<>OperationBlock.Block';
        exit;
      end;
      // Only process when in current address, prevent do it when reading operations from file
      if FOperationsHashTree.CanAddOperationToHashTree(op) then begin
        Result := op.DoOperation(FPreviousUpdatedBlocks, FSafeBoxTransaction, errors);
      end else begin
        errors := 'Cannot add operation. Limits reached';
        Exit;
      end;
    end else Result := true;
    if Result then begin
      if FOperationsHashTree.AddOperationToHashTree(op) then begin
        if FIsOnlyOperationBlock then begin
          // Clear fee values and put to False
          FIsOnlyOperationBlock := False;
          FOperationBlock.fee := op.OperationFee;
        end else begin
          FOperationBlock.fee := FOperationBlock.fee + op.OperationFee;
        end;
        FOperationBlock.operations_hash := FOperationsHashTree.HashTree;
        if FDisableds<=0 then Calc_Digest_Parts;
      end else begin
        errors := 'Cannot add operation. Limits reached';
        if (Execute) then begin
          // Undo execute
          TLog.NewLog(lterror,ClassName,Format('Undo operation.DoExecute due limits reached. Executing %d operations',[FOperationsHashTree.OperationsCount]));
          FPreviousUpdatedBlocks.Clear;
          FSafeBoxTransaction.Rollback;
          for i := 0 to FOperationsHashTree.OperationsCount-1 do FOperationsHashTree.GetOperation(i).DoOperation(FPreviousUpdatedBlocks, FSafeBoxTransaction, auxs);
        end;
        Result := False;
      end;
    end;
  finally
    Unlock;
  end;
End;

function TPCOperationsComp.AddOperations(operations: TOperationsHashTree; var errors: String): Integer;
Var i : Integer;
  e : String;
begin
  Lock;
  try
    Result := 0;
    errors := '';
    if operations=FOperationsHashTree then exit;
    inc(FDisableds);
    try
      for i := 0 to operations.OperationsCount - 1 do begin
        if not AddOperation(true,operations.GetOperation(i),e) then begin
          if (errors<>'') then errors := errors+' ';
          errors := errors + 'Op'+inttostr(i+1)+'/'+inttostr(operations.OperationsCount)+':'+e;
        end else inc(Result);
      end;
    finally
      Dec(FDisableds);
      Calc_Digest_Parts;
    end;
  finally
    Unlock;
  end;
end;

procedure TPCOperationsComp.CalcProofOfWork(fullcalculation: Boolean; var PoW: TRawBytes);
begin
  if fullcalculation then begin
    Calc_Digest_Parts;
  end;
  FStreamPoW.Position := 0;
  FStreamPoW.WriteBuffer(FDigest_Part1[Low(FDigest_Part1)],Length(FDigest_Part1));
  FStreamPoW.WriteBuffer(FDigest_Part2_Payload[Low(FDigest_Part2_Payload)],Length(FDigest_Part2_Payload));
  FStreamPoW.WriteBuffer(FDigest_Part3[Low(FDigest_Part3)],Length(FDigest_Part3));
  FStreamPoW.Write(FOperationBlock.timestamp,4);
  FStreamPoW.Write(FOperationBlock.nonce,4);
  if CT_ACTIVATE_RANDOMHASH_V4 AND (FOperationBlock.protocol_version >= CT_PROTOCOL_4) then
    TCrypto.DoRandomHash(FStreamPoW.Memory,length(FDigest_Part1)+length(FDigest_Part2_Payload)+length(FDigest_Part3)+8,PoW)
  else
    TCrypto.DoDoubleSha256(FStreamPoW.Memory,length(FDigest_Part1)+length(FDigest_Part2_Payload)+length(FDigest_Part3)+8,PoW);
end;

procedure TPCOperationsComp.Calc_Digest_Parts;
begin
  TPascalCoinProtocol.CalcProofOfWork_Part1(FOperationBlock,FDigest_Part1);
  FDigest_Part2_Payload := FOperationBlock.block_payload;
  Calc_Digest_Part3;
end;

procedure TPCOperationsComp.Calc_Digest_Part3;
begin
  FOperationBlock.operations_hash:=FOperationsHashTree.HashTree;
  TPascalCoinProtocol.CalcProofOfWork_Part3(FOperationBlock,FDigest_Part3);
end;

procedure TPCOperationsComp.Clear(DeleteOperations : Boolean);
var resetNewTarget : Boolean;
begin
  Lock;
  Try
    if DeleteOperations then begin
      FOperationsHashTree.ClearHastThree;
      FPreviousUpdatedBlocks.Clear;
      if Assigned(FSafeBoxTransaction) then
        FSafeBoxTransaction.CleanTransaction;
    end;

    // Note:
    // This function does not initializes "account_key" nor "block_payload" fields

    FOperationBlock.timestamp := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
    if Assigned(FBank) then begin
      resetNewTarget := False;
      FOperationBlock.protocol_version := FBank.SafeBox.CurrentProtocol;
      If (FOperationBlock.protocol_version=CT_PROTOCOL_1) And (FBank.SafeBox.CanUpgradeToProtocol(CT_PROTOCOL_2)) then begin
        FOperationBlock.protocol_version := CT_PROTOCOL_2; // If minting... upgrade to Protocol 2
      end else if (FOperationBlock.protocol_version=CT_PROTOCOL_2) And (FBank.SafeBox.CanUpgradeToProtocol(CT_PROTOCOL_3)) then begin
        FOperationBlock.protocol_version := CT_PROTOCOL_3; // If minting... upgrade to Protocol 3
      end else if (FOperationBlock.protocol_version=CT_PROTOCOL_3) And (FBank.SafeBox.CanUpgradeToProtocol(CT_PROTOCOL_4)) then begin
        FOperationBlock.protocol_version := CT_PROTOCOL_4; // If minting... upgrade to Protocol 4
        {$IFDEF ACTIVATE_RANDOMHASH_V4}
        resetNewTarget := True; // RandomHash algo will reset new target on V4
        {$ENDIF}
      end;
      if (FOperationBlock.protocol_version>=CT_PROTOCOL_4) then begin
        FOperationsHashTree.Max0feeOperationsBySigner := 1; // Limit to 1 0-fee operation by signer
      end else FOperationsHashTree.Max0feeOperationsBySigner := -1;
      FOperationBlock.block := FBank.BlocksCount;
      FOperationBlock.reward := TPascalCoinProtocol.GetRewardForNewLine(FBank.BlocksCount);
      if (resetNewTarget) then begin
        FOperationBlock.compact_target := TPascalCoinProtocol.ResetTarget(FOperationBlock.compact_target,FOperationBlock.protocol_version);
      end else begin
        FOperationBlock.compact_target := FBank.Safebox.GetActualCompactTargetHash(FOperationBlock.protocol_version);
      end;
      FOperationBlock.initial_safe_box_hash := FBank.SafeBox.SafeBoxHash;
      If FBank.LastOperationBlock.timestamp>FOperationBlock.timestamp then
        FOperationBlock.timestamp := FBank.LastOperationBlock.timestamp;
    end else begin
      FOperationBlock.block := 0;
      FOperationBlock.reward := TPascalCoinProtocol.GetRewardForNewLine(0);
      FOperationBlock.compact_target := CT_MinCompactTarget_v1;
      FOperationBlock.initial_safe_box_hash := TPCSafeBox.InitialSafeboxHash; // Nothing for first line
      FOperationBlock.protocol_version := CT_PROTOCOL_1;
      FOperationsHashTree.Max0feeOperationsBySigner := -1;
    end;
    FOperationBlock.operations_hash := FOperationsHashTree.HashTree;
    FOperationBlock.fee := 0;
    FOperationBlock.nonce := 0;
    FOperationBlock.proof_of_work:=Nil;
    FOperationBlock.protocol_available := CT_BlockChain_Protocol_Available;
    FIsOnlyOperationBlock := false;
  Finally
    try
      Calc_Digest_Parts; // Does not need to recalc PoW
    finally
      Unlock;
    end;
  End;
end;

procedure TPCOperationsComp.CopyFrom(Operations: TPCOperationsComp);
begin
  if Self=Operations then exit;
  Lock;
  Operations.Lock;
  Try
    FOperationBlock := Operations.FOperationBlock;
    FIsOnlyOperationBlock := Operations.FIsOnlyOperationBlock;
    FOperationsHashTree.CopyFromHashTree(Operations.FOperationsHashTree);
    if Assigned(FSafeBoxTransaction) And Assigned(Operations.FSafeBoxTransaction) then begin
      FSafeBoxTransaction.CopyFrom(Operations.FSafeBoxTransaction);
    end;
    FPreviousUpdatedBlocks.CopyFrom(Operations.FPreviousUpdatedBlocks);
    FDigest_Part1 := Operations.FDigest_Part1;
    FDigest_Part2_Payload := Operations.FDigest_Part2_Payload;
    FDigest_Part3 := Operations.FDigest_Part3;
  finally
    Operations.Unlock;
    Unlock;
  end;
end;

procedure TPCOperationsComp.CopyFromExceptAddressKey(Operations: TPCOperationsComp);
var lastopb : TOperationBlock;
begin
  Lock;
  Try
    if Self=Operations then exit;
    lastopb := FOperationBlock;
    FOperationBlock := Operations.FOperationBlock;
    FOperationBlock.account_key := lastopb.account_key; // Except AddressKey
    FOperationBlock.compact_target := FBank.Safebox.GetActualCompactTargetHash(FOperationBlock.protocol_version);
    FIsOnlyOperationBlock := Operations.FIsOnlyOperationBlock;
    FOperationsHashTree.CopyFromHashTree(Operations.FOperationsHashTree);
    FOperationBlock.operations_hash := FOperationsHashTree.HashTree;
    if Assigned(FSafeBoxTransaction) And Assigned(Operations.FSafeBoxTransaction) then begin
      FSafeBoxTransaction.CopyFrom(Operations.FSafeBoxTransaction);
    end;
    FPreviousUpdatedBlocks.CopyFrom(Operations.FPreviousUpdatedBlocks);
    // Recalc all
    Calc_Digest_Parts; // Does not need to recalc PoW
  finally
    Unlock;
  end;
end;

function TPCOperationsComp.Count: Integer;
begin
  Result := FOperationsHashTree.OperationsCount;
end;

constructor TPCOperationsComp.Create(ABank: TPCBank);
begin
  FOperationsLock := TPCCriticalSection.Create('TPCOperationsComp_OPERATIONSLOCK');
  FDisableds := 0;
  FStreamPoW := TMemoryStream.Create;
  FStreamPoW.Position := 0;
  FOperationsHashTree := TOperationsHashTree.Create;
  FOperationsHashTree.OnChanged:= OnOperationsHashTreeChanged;
  FBank := Nil;
  FOperationBlock := GetFirstBlock;
  FSafeBoxTransaction := Nil;
  FPreviousUpdatedBlocks := TAccountPreviousBlockInfo.Create;
  if Assigned(ABank) then begin
    SetBank( TPCBank(ABank) );
  end else Clear(true);
end;

destructor TPCOperationsComp.Destroy;
begin
  FOperationsLock.Acquire;
  try
    Clear(true);
    FreeAndNil(FOperationsHashTree);
    if Assigned(FSafeBoxTransaction) then begin
      FreeAndNil(FSafeBoxTransaction);
    end;
    FreeAndNil(FStreamPoW);
    FreeAndNil(FPreviousUpdatedBlocks);
  finally
    FreeAndNil(FOperationsLock);
  end;
  inherited;
end;

class function TPCOperationsComp.EqualsOperationBlock(const OperationBlock1,
  OperationBlock2: TOperationBlock): Boolean;
begin

  Result := (OperationBlock1.block=OperationBlock2.block)
           And (TAccountComp.EqualAccountKeys(OperationBlock1.account_key,OperationBlock2.account_key))
           And (OperationBlock1.reward=OperationBlock2.reward)
           And (OperationBlock1.fee=OperationBlock2.fee)
           And (OperationBlock1.protocol_version=OperationBlock2.protocol_version)
           And (OperationBlock1.protocol_available=OperationBlock2.protocol_available)
           And (OperationBlock1.timestamp=OperationBlock2.timestamp)
           And (OperationBlock1.compact_target=OperationBlock2.compact_target)
           And (OperationBlock1.nonce=OperationBlock2.nonce)
           And (TBaseType.Equals(OperationBlock1.block_payload,OperationBlock2.block_payload))
           And (TBaseType.Equals(OperationBlock1.initial_safe_box_hash,OperationBlock2.initial_safe_box_hash))
           And (TBaseType.Equals(OperationBlock1.operations_hash,OperationBlock2.operations_hash))
           And (TBaseType.Equals(OperationBlock1.proof_of_work,OperationBlock2.proof_of_work));
end;

function TPCOperationsComp.GetAccountKey: TAccountKey;
begin
  Result := FOperationBlock.account_key;
end;

function TPCOperationsComp.GetBlockPayload: TRawBytes;
begin
  Result := FOperationBlock.block_payload;
end;

class function TPCOperationsComp.GetFirstBlock: TOperationBlock;
begin
  Result := CT_OperationBlock_NUL;
end;

function TPCOperationsComp.GetnOnce: Cardinal;
begin
  Result := FOperationBlock.nonce;
end;

function TPCOperationsComp.GetOperation(index: Integer): TPCOperation;
begin
  Result := FOperationsHashTree.GetOperation(index);
end;

class function TPCOperationsComp.GetOperationClassByOpType(OpType: Cardinal): TPCOperationClass;
Var i : Integer;
begin
  i := IndexOfOperationClassByOpType(OpType);
  if i<0 then result := Nil
  else Result := TPCOperationClass( _OperationsClass[i] );
end;

function TPCOperationsComp.Gettimestamp: Cardinal;
begin
  Result := FOperationBlock.timestamp;
end;


class function TPCOperationsComp.IndexOfOperationClass(OpClass: TPCOperationClass): Integer;
begin
  for Result := low(_OperationsClass) to high(_OperationsClass) do
  begin
    if (_OperationsClass[Result] = OpClass) then
      exit;
  end;
  Result := -1;
end;

class function TPCOperationsComp.IndexOfOperationClassByOpType(OpType: Cardinal): Integer;
begin
  for Result := low(_OperationsClass) to high(_OperationsClass) do
  begin
    if (_OperationsClass[Result].OpType = OpType) then
      exit;
  end;
  Result := -1;
end;

function TPCOperationsComp.LoadBlockFromStorage(Stream: TStream; var errors: String): Boolean;
begin
  Result := LoadBlockFromStreamExt(Stream,true,errors);
end;

function TPCOperationsComp.LoadBlockFromStream(Stream: TStream; var errors: String): Boolean;
begin
  Result := LoadBlockFromStreamExt(Stream,false,errors);
end;

function TPCOperationsComp.LoadBlockFromStreamExt(Stream: TStream; LoadingFromStorage: Boolean; var errors: String): Boolean;
Var i : Integer;
  lastfee : UInt64;
  soob : Byte;
  raw: TRawBytes;
  load_protocol_version : Word;
begin
  Lock;
  Try
    Clear(true);
    Result := False;
    //
    errors := '';
    if (Stream.Size - Stream.Position < 5) then begin
      errors := 'Invalid protocol structure. Check application version!';
      exit;
    end;
    soob := 255;
    Stream.Read(soob,1);
    // About soob var:
    // In build prior to 1.0.4 soob only can have 2 values: 0 or 1
    // In build 1.0.4 soob can has 2 more values: 2 or 3
    // In build 2.0 soob can has 1 more value: 4
    // In build 3.0 soob can have value: 5
    // In future, old values 0 and 1 will no longer be used!
    // - Value 0 and 2 means that contains also operations
    // - Value 1 and 3 means that only contains operationblock info
    // - Value 2 and 3 means that contains protocol info prior to block number
    // - Value 4 means that is loading from storage using protocol v2 (so, includes always operations)
    // - Value 5 means that is loading from storage using TAccountPreviousBlockInfo
    load_protocol_version := CT_PROTOCOL_1;
    if (soob in [0,2]) then FIsOnlyOperationBlock:=false
    else if (soob in [1,3]) then FIsOnlyOperationBlock:=true
    else if (soob in [4]) then begin
      FIsOnlyOperationBlock:=false;
      load_protocol_version := CT_PROTOCOL_2;
    end else if (soob in [5]) then begin
      FIsOnlyOperationBlock:=False;
      load_protocol_version := CT_PROTOCOL_3;
    end else begin
      errors := 'Invalid value in protocol header! Found:'+inttostr(soob)+' - Check if your application version is Ok';
      exit;
    end;

    if (soob in [2,3,4,5]) then begin
      Stream.Read(FOperationBlock.protocol_version, Sizeof(FOperationBlock.protocol_version));
      Stream.Read(FOperationBlock.protocol_available, Sizeof(FOperationBlock.protocol_available));
    end else begin
      // We assume that protocol_version is 1 and protocol_available is 0
      FOperationBlock.protocol_version := 1;
      FOperationBlock.protocol_available := 0;
    end;

    if Stream.Read(FOperationBlock.block, Sizeof(FOperationBlock.block))<0 then exit;

    if TStreamOp.ReadAnsiString(Stream, raw) < 0 then exit;
    FOperationBlock.account_key := TAccountComp.RawString2Accountkey(raw);
    if Stream.Read(FOperationBlock.reward, Sizeof(FOperationBlock.reward)) < 0 then exit;
    if Stream.Read(FOperationBlock.fee, Sizeof(FOperationBlock.fee)) < 0 then exit;
    if Stream.Read(FOperationBlock.timestamp, Sizeof(FOperationBlock.timestamp)) < 0 then exit;
    if Stream.Read(FOperationBlock.compact_target, Sizeof(FOperationBlock.compact_target)) < 0 then exit;
    if Stream.Read(FOperationBlock.nonce, Sizeof(FOperationBlock.nonce)) < 0 then exit;
    if TStreamOp.ReadAnsiString(Stream, FOperationBlock.block_payload) < 0 then exit;
    if TStreamOp.ReadAnsiString(Stream, FOperationBlock.initial_safe_box_hash) < 0 then exit;
    if TStreamOp.ReadAnsiString(Stream, FOperationBlock.operations_hash) < 0 then exit;
    if TStreamOp.ReadAnsiString(Stream, FOperationBlock.proof_of_work) < 0 then exit;
    If FIsOnlyOperationBlock then begin
      Result := true;
      exit;
    end;
    // Fee will be calculated for each operation. Set it to 0 and check later for integrity
    lastfee := OperationBlock.fee;
    FOperationBlock.fee := 0;
    if FOperationBlock.protocol_version>=CT_PROTOCOL_4 then begin
      FOperationsHashTree.Max0feeOperationsBySigner := 1;
    end else FOperationsHashTree.Max0feeOperationsBySigner := -1;
    Result := FOperationsHashTree.LoadOperationsHashTreeFromStream(Stream,LoadingFromStorage,load_protocol_version,FPreviousUpdatedBlocks,errors);
    if not Result then begin
      exit;
    end;
    If load_protocol_version>=CT_PROTOCOL_3 then begin
      Result := FPreviousUpdatedBlocks.LoadFromStream(Stream);
      If Not Result then begin
        errors := 'Invalid PreviousUpdatedBlock stream';
        Exit;
      end;
    end;
    //
    FOperationBlock.fee := FOperationsHashTree.TotalFee;
    FOperationBlock.operations_hash := FOperationsHashTree.HashTree;
    Calc_Digest_Parts;
    // Validation control:
    if (lastfee<>OperationBlock.fee) then begin
      errors := 'Corrupted operations fee old:'+inttostr(lastfee)+' new:'+inttostr(OperationBlock.fee);
      for i := 0 to FOperationsHashTree.OperationsCount - 1 do begin
        errors := errors + ' Op'+inttostr(i+1)+':'+FOperationsHashTree.GetOperation(i).ToString;
      end;
      Result := false;
      exit;
    end;
    Result := true;
  finally
    Unlock;
  end;
end;

class function TPCOperationsComp.OperationBlockToText(const OperationBlock: TOperationBlock): String;
begin
  Result := Format('Block:%d Timestamp:%d Reward:%d Fee:%d Target:%d PoW:%s Payload:%s Nonce:%d OperationsHash:%s SBH:%s',[operationBlock.block,
    operationblock.timestamp,operationblock.reward,operationblock.fee, OperationBlock.compact_target, TCrypto.ToHexaString(operationblock.proof_of_work),
    OperationBlock.block_payload.ToPrintable,OperationBlock.nonce,TCrypto.ToHexaString(OperationBlock.operations_hash),
    TCrypto.ToHexaString(OperationBlock.initial_safe_box_hash)]);
end;

class function TPCOperationsComp.RegisterOperationClass(OpClass: TPCOperationClass): Boolean;
Var
  i: Integer;
begin
  i := IndexOfOperationClass(OpClass);
  if i >= 0 then
    exit;
  SetLength(_OperationsClass, Length(_OperationsClass) + 1);
  _OperationsClass[ high(_OperationsClass)] := OpClass;
end;

procedure TPCOperationsComp.SanitizeOperations;
  { This function check operationblock with bank and updates itself if necessary
    Then checks if operations are ok, and deletes old ones.
    Finally calculates new operation pow
    It's used when a new account has beed found by other chanels (miners o nodes...)
    }
Var i,n,lastn, iUndo : Integer;
  op : TPCOperation;
  errors, auxs : String;
  aux,aux2 : TOperationsHashTree;
  resetNewTarget : Boolean;
begin
  Lock;
  Try
    FOperationBlock.timestamp := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
    if Assigned(FBank) then begin
      resetNewTarget := False;
      FOperationBlock.protocol_version := FBank.SafeBox.CurrentProtocol;
      If (FOperationBlock.protocol_version=CT_PROTOCOL_1) And (FBank.SafeBox.CanUpgradeToProtocol(CT_PROTOCOL_2)) then begin
        TLog.NewLog(ltinfo,ClassName,'New miner protocol version to 2 at sanitize');
        FOperationBlock.protocol_version := CT_PROTOCOL_2;
      end else if (FOperationBlock.protocol_version=CT_PROTOCOL_2) And (FBank.SafeBox.CanUpgradeToProtocol(CT_PROTOCOL_3)) then begin
        TLog.NewLog(ltinfo,ClassName,'New miner protocol version to 3 at sanitize');
        FOperationBlock.protocol_version := CT_PROTOCOL_3;
      end else if (FOperationBlock.protocol_version=CT_PROTOCOL_3) And (FBank.SafeBox.CanUpgradeToProtocol(CT_PROTOCOL_4)) then begin
        TLog.NewLog(ltinfo,ClassName,'New miner protocol version to 4 at sanitize');
        FOperationBlock.protocol_version := CT_PROTOCOL_4;
        {$IFDEF ACTIVATE_RANDOMHASH_V4}
        resetNewTarget := True; // RandomHash algo will reset new target on V4
        {$ENDIF}
      end;
      FOperationBlock.block := FBank.BlocksCount;

      FOperationBlock.reward := TPascalCoinProtocol.GetRewardForNewLine(FBank.BlocksCount);
      if (resetNewTarget) then begin
        FOperationBlock.compact_target := TPascalCoinProtocol.ResetTarget(FOperationBlock.compact_target,FOperationBlock.protocol_version);
      end else begin
        FOperationBlock.compact_target := FBank.Safebox.GetActualCompactTargetHash(FOperationBlock.protocol_version);
      end;
      FOperationBlock.initial_safe_box_hash := FBank.SafeBox.SafeBoxHash;
      If FBank.LastOperationBlock.timestamp>FOperationBlock.timestamp then
        FOperationBlock.timestamp := FBank.LastOperationBlock.timestamp;
    end else begin
      FOperationBlock.block := 0;
      FOperationBlock.reward := TPascalCoinProtocol.GetRewardForNewLine(0);
      FOperationBlock.compact_target := CT_MinCompactTarget_v1;
      FOperationBlock.initial_safe_box_hash := TPCSafeBox.InitialSafeboxHash;
      FOperationBlock.protocol_version := CT_PROTOCOL_1;
    end;
    FOperationBlock.proof_of_work:=Nil;
    FOperationBlock.protocol_available := CT_BlockChain_Protocol_Available;
    n := 0;
    FOperationBlock.fee := 0;
    //
    SafeBoxTransaction.CleanTransaction;
    FPreviousUpdatedBlocks.Clear;
    aux := TOperationsHashTree.Create;
    Try
      if (FOperationBlock.protocol_version>=CT_PROTOCOL_4) then begin
        aux.Max0feeOperationsBySigner := 1;
      end else aux.Max0feeOperationsBySigner := -1;
      lastn := FOperationsHashTree.OperationsCount;
      for i:=0 to lastn-1 do begin
        op := FOperationsHashTree.GetOperation(i);
        if (aux.CanAddOperationToHashTree(op)) then begin
          if (op.DoOperation(FPreviousUpdatedBlocks, SafeBoxTransaction,errors)) then begin
            if aux.AddOperationToHashTree(op) then begin
              inc(n);
              inc(FOperationBlock.fee,op.OperationFee);
              {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,Classname,'Sanitizing (pos:'+inttostr(i+1)+'/'+inttostr(lastn)+'): '+op.ToString){$ENDIF};
            end else begin
              TLog.NewLog(lterror,ClassName,Format('Undo operation.DoExecute at Sanitize due limits reached. Executing %d operations',[aux.OperationsCount]));
              FPreviousUpdatedBlocks.Clear;
              FSafeBoxTransaction.Rollback;
              for iUndo := 0 to aux.OperationsCount-1 do aux.GetOperation(iUndo).DoOperation(FPreviousUpdatedBlocks, FSafeBoxTransaction, auxs);
            end;
          end;
        end;
      end;
    Finally
      aux2 := FOperationsHashTree;
      FOperationsHashTree := aux;
      aux2.Free;
      FOperationBlock.operations_hash := FOperationsHashTree.HashTree;
    End;
  Finally
    Calc_Digest_Parts; // Does not need to recalc PoW
    Unlock;
  End;
  if (n>0) then begin
    TLog.NewLog(ltdebug,Classname,Format('Sanitize operations (before %d - after %d)',[lastn,n]));
  end;
end;

function TPCOperationsComp.SaveBlockToStorage(Stream: TStream): Boolean;
begin
  Result := SaveBlockToStreamExt(false,Stream,true);
end;

function TPCOperationsComp.SaveBlockToStream(save_only_OperationBlock : Boolean; Stream: TStream): Boolean;
begin
  Result := SaveBlockToStreamExt(save_only_OperationBlock,Stream,false);
end;

function TPCOperationsComp.SaveBlockToStreamExt(save_only_OperationBlock: Boolean; Stream: TStream; SaveToStorage: Boolean): Boolean;
Var soob : Byte;
begin
  Lock;
  Try
    if save_only_OperationBlock then begin
      {Old versions:
      if (FOperationBlock.protocol_version=1) And (FOperationBlock.protocol_available=0) then soob := 1
      else soob := 3;}
      soob := 3;
    end else begin
      {Old versions:
      if (FOperationBlock.protocol_version=1) And (FOperationBlock.protocol_available=0) then soob := 0
      else soob := 2;}
      soob := 2;
      if (SaveToStorage) then begin
        {Old versions:
        // Introduced on protocol v2: soob = 4 when saving to storage
        soob := 4;}
        // Introduced on protocol v3: soob = 5 when saving to storage
        soob := 5; // V3 will always save PreviousUpdatedBlocks
      end;
    end;
    Stream.Write(soob,1);
    if (soob>=2) then begin
      Stream.Write(FOperationBlock.protocol_version, Sizeof(FOperationBlock.protocol_version));
      Stream.Write(FOperationBlock.protocol_available, Sizeof(FOperationBlock.protocol_available));
    end;
    //
    Stream.Write(FOperationBlock.block, Sizeof(FOperationBlock.block));
    //
    TStreamOp.WriteAnsiString(Stream,TAccountComp.AccountKey2RawString(FOperationBlock.account_key));
    Stream.Write(FOperationBlock.reward, Sizeof(FOperationBlock.reward));
    Stream.Write(FOperationBlock.fee, Sizeof(FOperationBlock.fee));
    Stream.Write(FOperationBlock.timestamp, Sizeof(FOperationBlock.timestamp));
    Stream.Write(FOperationBlock.compact_target, Sizeof(FOperationBlock.compact_target));
    Stream.Write(FOperationBlock.nonce, Sizeof(FOperationBlock.nonce));
    TStreamOp.WriteAnsiString(Stream, FOperationBlock.block_payload);
    TStreamOp.WriteAnsiString(Stream, FOperationBlock.initial_safe_box_hash);
    TStreamOp.WriteAnsiString(Stream, FOperationBlock.operations_hash);
    TStreamOp.WriteAnsiString(Stream, FOperationBlock.proof_of_work);
    { Basic size calculation:
    protocols : 2 words = 4 bytes
    block : 4 bytes
    Account_key (VARIABLE LENGTH) at least 2 + 34 + 34 for secp256k1 key = 70 bytes
    reward, fee, timestamp, compact_target, nonce = 8+8+4+4+4 = 28 bytes
    payload (VARIABLE LENGTH) minimum 2 bytes... but usually 40 by average = 40 bytes
    sbh, operations_hash, pow ( 32 + 32 + 32 ) =  96 bytes
    Total, by average: 242 bytes
    }
    if (Not save_only_OperationBlock) then begin
      Result := FOperationsHashTree.SaveOperationsHashTreeToStream(Stream,SaveToStorage);
      If (Result) And (SaveToStorage) And (soob=5) then begin
        FPreviousUpdatedBlocks.SaveToStream(Stream);
      end;
    end else Result := true;
  finally
    Unlock;
  end;
end;

class function TPCOperationsComp.SaveOperationBlockToStream(const OperationBlock: TOperationBlock; Stream: TStream): Boolean;
Var soob : Byte;
begin
  soob := 3;
  Stream.Write(soob,1);
  Stream.Write(OperationBlock.protocol_version, Sizeof(OperationBlock.protocol_version));
  Stream.Write(OperationBlock.protocol_available, Sizeof(OperationBlock.protocol_available));
  //
  Stream.Write(OperationBlock.block, Sizeof(OperationBlock.block));
  //
  TStreamOp.WriteAnsiString(Stream,TAccountComp.AccountKey2RawString(OperationBlock.account_key));
  Stream.Write(OperationBlock.reward, Sizeof(OperationBlock.reward));
  Stream.Write(OperationBlock.fee, Sizeof(OperationBlock.fee));
  Stream.Write(OperationBlock.timestamp, Sizeof(OperationBlock.timestamp));
  Stream.Write(OperationBlock.compact_target, Sizeof(OperationBlock.compact_target));
  Stream.Write(OperationBlock.nonce, Sizeof(OperationBlock.nonce));
  TStreamOp.WriteAnsiString(Stream, OperationBlock.block_payload);
  TStreamOp.WriteAnsiString(Stream, OperationBlock.initial_safe_box_hash);
  TStreamOp.WriteAnsiString(Stream, OperationBlock.operations_hash);
  TStreamOp.WriteAnsiString(Stream, OperationBlock.proof_of_work);
  Result := true;
end;

function TPCOperationsComp.Update_And_RecalcPOW(newNOnce, newTimestamp: Cardinal; newBlockPayload: TRawBytes) : Boolean;
Var i : Integer;
  _changedPayload : Boolean;
begin
  Lock;
  Try
    If newBlockPayload<>FOperationBlock.block_payload then begin
      _changedPayload := TPascalCoinProtocol.IsValidMinerBlockPayload(newBlockPayload);
    end else _changedPayload:=False;
    If (_changedPayload) Or (newNOnce<>FOperationBlock.nonce) Or (newTimestamp<>FOperationBlock.timestamp) then begin
      If _changedPayload then FOperationBlock.block_payload:=newBlockPayload;
      FOperationBlock.nonce:=newNOnce;
      FOperationBlock.timestamp:=newTimestamp;
      CalcProofOfWork(_changedPayload,FOperationBlock.proof_of_work);
      Result := True;
    end else Result := False;
  finally
    Unlock;
  end;
end;

procedure TPCOperationsComp.SetAccountKey(const value: TAccountKey);
begin
  Lock;
  Try
    if TAccountComp.AccountKey2RawString(value)=TAccountComp.AccountKey2RawString(FOperationBlock.account_key) then exit;
    FOperationBlock.account_key := value;
    Calc_Digest_Parts;
  finally
    Unlock;
  end;
end;

procedure TPCOperationsComp.SetBank(const value: TPCBank);
begin
  if FBank = value then exit;
  if Assigned(FBank) then begin
     FreeAndNil(FSafeBoxTransaction);
  end;
  FBank := value;
  if Assigned(value) then begin
    FSafeBoxTransaction := TPCSafeBoxTransaction.Create(FBank.SafeBox);
  end;
  Clear(true);
end;

procedure TPCOperationsComp.SetBlockPayload(const Value: TRawBytes);
begin
  Update_And_RecalcPOW(FOperationBlock.nonce,FOperationBlock.timestamp,Value);
end;

procedure TPCOperationsComp.OnOperationsHashTreeChanged(Sender: TObject);
begin
  FOperationBlock.operations_hash:=FOperationsHashTree.HashTree;
  Calc_Digest_Part3;
end;

procedure TPCOperationsComp.SetnOnce(const value: Cardinal);
begin
  Update_And_RecalcPOW(value,FOperationBlock.timestamp,FOperationBlock.block_payload);
end;

procedure TPCOperationsComp.Settimestamp(const value: Cardinal);
begin
  Update_And_RecalcPOW(FOperationBlock.nonce,value,FOperationBlock.block_payload);
end;

procedure TPCOperationsComp.UpdateTimestamp;
Var ts : Cardinal;
begin
  Lock;
  Try
    ts := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
    if Assigned(FBank) then begin
      If FBank.FLastOperationBlock.timestamp>ts then ts := FBank.FLastOperationBlock.timestamp;
    end;
    timestamp := ts;
  finally
    Unlock;
  end;
end;

function TPCOperationsComp.GetMinerRewardPseudoOperation : TOperationResume;
begin
   Result := CT_TOperationResume_NUL;
   Result.valid := true;
   Result.Block := FOperationBlock.block;
   Result.time := self.OperationBlock.timestamp;
   Result.AffectedAccount := FOperationBlock.block * CT_AccountsPerBlock;
   Result.Amount := self.OperationBlock.reward;
   Result.Fee := self.OperationBlock.fee;
   Result.Balance := Result.Amount+Result.Fee;
   Result.OperationTxt := 'Miner reward';
end;

function TPCOperationsComp.ValidateOperationBlock(var errors : String): Boolean;
Var i : Integer;
begin
  errors := '';
  Result := False;
  Lock;
  Try
    If Not Assigned(SafeBoxTransaction) then begin
      errors := 'ERROR DEV 20170523-1';
      exit;
    end;
    If Not Assigned(SafeBoxTransaction.FreezedSafeBox) then begin
      errors := 'ERROR DEV 20170523-2';
      exit;
    end;
    // Check OperationBlock info:
    If not SafeBoxTransaction.FreezedSafeBox.IsValidNewOperationsBlock(OperationBlock,True,errors) then exit;
    // Execute SafeBoxTransaction operations:
    SafeBoxTransaction.Rollback;
    FPreviousUpdatedBlocks.Clear;
    for i := 0 to Count - 1 do begin
      If Not Operation[i].DoOperation(FPreviousUpdatedBlocks, SafeBoxTransaction,errors) then begin
        errors := 'Error executing operation '+inttostr(i+1)+'/'+inttostr(Count)+': '+errors;
        exit;
      end;
    end;
    // Check OperationsHash value is valid
    // New Build 2.1.7 use safe BinStrComp
    if TBaseType.BinStrComp(FOperationsHashTree.HashTree,OperationBlock.operations_hash)<>0 then begin
      errors := 'Invalid Operations Hash '+TCrypto.ToHexaString(OperationBlock.operations_hash)+'<>'+TCrypto.ToHexaString(FOperationsHashTree.HashTree);
      exit;
    end;
    // Check OperationBlock with SafeBox info:
    if (SafeBoxTransaction.FreezedSafeBox.TotalBalance<>(SafeBoxTransaction.TotalBalance+SafeBoxTransaction.TotalFee)) then begin
      errors := Format('Invalid integrity balance at SafeBox. Actual Balance:%d  New Balance:(%d + fee %d = %d)',
        [SafeBoxTransaction.FreezedSafeBox.TotalBalance,
          SafeBoxTransaction.TotalBalance,
          SafeBoxTransaction.TotalFee,
          SafeBoxTransaction.TotalBalance+SafeBoxTransaction.TotalFee]);
      exit;
    end;
    // Check fee value
    if (SafeBoxTransaction.TotalFee<>OperationBlock.fee) then begin
      errors := Format('Invalid fee integrity at SafeBoxTransaction. New Balance:(%d + fee %d = %d)  OperationBlock.fee:%d',
        [
          SafeBoxTransaction.TotalBalance,
          SafeBoxTransaction.TotalFee,
          SafeBoxTransaction.TotalBalance+SafeBoxTransaction.TotalFee,
          OperationBlock.fee]);
      exit;
    end;

    Result := true;
  finally
    Unlock;
  end;
end;

procedure TPCOperationsComp.Lock;
begin
  FOperationsLock.Acquire;
end;

procedure TPCOperationsComp.Unlock;
begin
  FOperationsLock.Release;
end;

{ TPCBankNotify }

constructor TPCBankNotify.Create(AOwner: TComponent);
begin
  inherited;
  FBank := Nil;
end;

destructor TPCBankNotify.Destroy;
begin
  Bank := Nil;
  inherited;
end;

procedure TPCBankNotify.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (operation=opremove) then if AComponent=FBank then FBank:=nil;
end;

procedure TPCBankNotify.NotifyNewBlock;
begin
  if Assigned(FOnNewBlock) Then FOnNewBlock(Bank);
end;

procedure TPCBankNotify.SetBank(const Value: TPCBank);
begin
  if Assigned(FBank) then begin
    FBank.FNotifyList.Remove(Self);
    FBank.RemoveFreeNotification(Self);
  end;
  FBank := Value;
  if Assigned(FBank) then begin
    FBank.FreeNotification(Self);
    FBank.FNotifyList.Add(Self);
  end;
end;

{ TOperationsHashTree }

Type TOperationHashTreeReg = Record
       Op : TPCOperation;
     end;
     POperationHashTreeReg = ^TOperationHashTreeReg;
     TOperationsHashAccountsData = Record
       account_number : Cardinal;
       account_count : Integer;
       account_without_fee : Integer;
     end;
     POperationsHashAccountsData = ^TOperationsHashAccountsData;

function TOperationsHashTree.AddOperationToHashTree(op: TPCOperation) : Boolean;
Var l : TList<Pointer>;
begin
  l := FHashTreeOperations.LockList;
  try
    Result := InternalAddOperationToHashTree(l,op,True);
  finally
    FHashTreeOperations.UnlockList;
  end;
end;

function TOperationsHashTree.CanAddOperationToHashTree(op: TPCOperation): Boolean;
Var lockedList : TList<Pointer>;
begin
  lockedList := FHashTreeOperations.LockList;
  Try
    Result := InternalCanAddOperationToHashTree(lockedList,op);
  Finally
    FHashTreeOperations.UnlockList;
  End;
end;

procedure TOperationsHashTree.ClearHastThree;
var l : TList<Pointer>;
  i : Integer;
  P : POperationHashTreeReg;
  PaccData : POperationsHashAccountsData;
begin
  l := FHashTreeOperations.LockList;
  try
    FTotalAmount := 0;
    FTotalFee := 0;
    Try
      for i := 0 to l.Count - 1 do begin
        P := l[i];
        _PCOperationsStorage.RemovePCOperation(P^.Op);
        Dispose(P);
      end;
      for i:=0 to FListOrderedByAccountsData.Count-1 do begin
        PaccData := FListOrderedByAccountsData[i];
        Dispose(PaccData);
      end;
    Finally
      l.Clear;
      FListOrderedBySha256.Clear;
      FListOrderedByAccountsData.Clear;
      FListOrderedByOpReference.Clear;
      FHashTree:=Nil;
    End;
    If Assigned(FOnChanged) then FOnChanged(Self);
  finally
    FHashTreeOperations.UnlockList;
  end;
end;

procedure TOperationsHashTree.CopyFromHashTree(Sender: TOperationsHashTree);
Var i : Integer;
  lme, lsender : TList<Pointer>;
  PSender : POperationHashTreeReg;
  lastNE : TNotifyEvent;
begin
  if (Sender = Self) then begin
    exit;
  end;
  lme := FHashTreeOperations.LockList;
  try
    lastNE := FOnChanged;
    FOnChanged := Nil;
    try
      ClearHastThree;
      FMax0feeOperationsBySigner := Sender.Max0feeOperationsBySigner;
      lsender := Sender.FHashTreeOperations.LockList;
      try
        for i := 0 to lsender.Count - 1 do begin
          PSender := lsender[i];
          InternalAddOperationToHashTree(lme,PSender^.Op,False);
        end;
        // Improvement TOperationsHashTree speed 2.1.6
        // FHashTree value updated now, not on every for cycle
        FHashTree:=Sender.FHashTree;
      finally
        Sender.FHashTreeOperations.UnlockList;
      end;
    finally
      FOnChanged := lastNE;
    end;
    If Assigned(FOnChanged) then FOnChanged(Self);
  finally
    FHashTreeOperations.UnlockList;
  end;
end;

constructor TOperationsHashTree.Create;
begin
  FOnChanged:=Nil;
  FListOrderedBySha256 := TList<Integer>.Create;
  FListOrderedByAccountsData := TList<Pointer>.Create;
  FListOrderedByOpReference := TList<Integer>.Create;
  FTotalAmount := 0;
  FTotalFee := 0;
  FHashTree := Nil;
  FMax0feeOperationsBySigner := -1; // Unlimited by default
  FHashTreeOperations := TPCThreadList<Pointer>.Create('TOperationsHashTree_HashTreeOperations');
end;

procedure TOperationsHashTree.Delete(index: Integer);
Var l : TList<Pointer>;
  P : POperationHashTreeReg;
  i,iDel,iValuePosDeleted : Integer;
  PaccData : POperationsHashAccountsData;
begin
  l := FHashTreeOperations.LockList;
  try
    P := l[index];
    // Delete from Ordered by OpReference
    if Not FindOrderedByOpReference(l,P^.Op.GetOpReference,iDel) then begin
      TLog.NewLog(ltError,ClassName,'DEV ERROR 20180927-1 Operation not found in ordered by reference list: '+P^.Op.ToString);
    end else begin
      iValuePosDeleted := PtrInt(FListOrderedByOpReference[iDel]);
      if (iValuePosDeleted<>index) then begin
        if (POperationHashTreeReg(l[iValuePosDeleted])^.Op.GetOpReference <> P^.Op.GetOpReference) then
          TLog.NewLog(lterror,ClassName,Format('DEV ERROR 20180928-2 [%d]=%d <> %d',[iDel,iValuePosDeleted,index]));
      end;
      FListOrderedByOpReference.Delete(iDel);
    end;
    // Decrease FListOrderedByOpReference values > index
    for i := 0 to FListOrderedByOpReference.Count - 1 do begin
      if PtrInt(FListOrderedByOpReference[i])>index then begin
        FListOrderedByOpReference[i] := ( (FListOrderedByOpReference[i]) - 1 );
      end;
    end;

    // Delete from Ordered
    If Not FindOrderedBySha(l,P^.Op.Sha256,iDel) then begin
      TLog.NewLog(ltError,ClassName,'DEV ERROR 20180213-1 Operation not found in ordered list: '+P^.Op.ToString);
    end else begin
      iValuePosDeleted := PtrInt(FListOrderedBySha256[iDel]);
      if (iValuePosDeleted<>index) then
        TLog.NewLog(lterror,ClassName,Format('DEV ERROR 20180928-3 [%d]=%d <> %d',[iDel,iValuePosDeleted,index]));

      FListOrderedBySha256.Delete(iDel);
    end;
    // Decrease FListOrderedBySha256 values > index
    for i := 0 to FListOrderedBySha256.Count - 1 do begin
      if PtrInt(FListOrderedBySha256[i])>index then begin
        FListOrderedBySha256[i] := ( (FListOrderedBySha256[i]) - 1 );
      end;
    end;

    // Delete from account Data
    If Not FindOrderedByAccountData(l,P^.Op.SignerAccount,i) then begin
      TLog.NewLog(ltError,ClassName,Format('DEV ERROR 20180213-3 account %d not found in ordered list: %s',[P^.Op.SignerAccount,P^.Op.ToString]));
    end else begin
      PaccData := POperationsHashAccountsData( FListOrderedByAccountsData[i] );
      Dec(PaccData.account_count);
      If (P^.Op.OperationFee=0) then Dec(PaccData.account_without_fee);
      If (PaccData.account_count<=0) then begin
        Dispose(PaccData);
        FListOrderedByAccountsData.Delete(i);
      end;
    end;

    l.Delete(index);
    _PCOperationsStorage.RemovePCOperation(P^.Op);
    Dispose(P);
    // Recalc operations hash
    FTotalAmount := 0;
    FTotalFee := 0;
    FHashTree := Nil; // Init to future recalc
    for i := 0 to l.Count - 1 do begin
      P := l[i];
      // Include to hash tree
      inc(FTotalAmount,P^.Op.OperationAmount);
      inc(FTotalFee,P^.Op.OperationFee);
    end;
    If Assigned(FOnChanged) then FOnChanged(Self);
  finally
    FHashTreeOperations.UnlockList;
  end;
end;

destructor TOperationsHashTree.Destroy;
begin
  FOnChanged := Nil;
  ClearHastThree;
  FreeAndNil(FHashTreeOperations);
  FHashTree := Nil;
  FreeAndNil(FListOrderedBySha256);
  FreeAndNil(FListOrderedByAccountsData);
  FreeAndNil(FListOrderedByOpReference);
  inherited;
end;

function TOperationsHashTree.GetHashTree: TRawBytes;
Var l : TList<Pointer>;
  i : Integer;
  P : POperationHashTreeReg;
  tmpRaw : TRawBytes;
begin
  if Length(FHashTree)<>32 then begin
    l := FHashTreeOperations.LockList;
    Try
      TCrypto.DoSha256(Nil,FHashTree);
      for i := 0 to l.Count - 1 do begin
        P := l[i];
        // Include to hash tree
        // TCrypto.DoSha256(FHashTree+P^.Op.Sha256,FHashTree);  COMPILER BUG 2.1.6: Using FHashTree as a "out" param can be initialized prior to be updated first parameter!
        TBaseType.Concat(FHashTree,P^.Op.Sha256,tmpRaw);
        FHashTree := TCrypto.DoSha256(tmpRaw);
      end;
    Finally
      FHashTreeOperations.UnlockList;
    End;
  end;
  Result := FHashTree;
end;

function TOperationsHashTree.GetOperation(index: Integer): TPCOperation;
Var l : TList<Pointer>;
begin
  l := FHashTreeOperations.LockList;
  try
    Result := POperationHashTreeReg(l[index])^.Op;
  finally
    FHashTreeOperations.UnlockList;
  end;
end;

function TOperationsHashTree.GetOperationsAffectingAccount(account_number: Cardinal; List: TList<Cardinal>): Integer;
  // This function retrieves operations from HashTree that affeccts to an account_number
Var l : TList<Pointer>;
  intl : TList<Cardinal>;
  i,j : Integer;
begin
  List.Clear;
  l := FHashTreeOperations.LockList;
  try
    intl := TList<Cardinal>.Create;
    try
      for i := 0 to l.Count - 1 do begin
        intl.Clear;
        POperationHashTreeReg(l[i])^.Op.AffectedAccounts(intl);
        if intl.IndexOf(account_number)>=0 then List.Add(Cardinal(i));
      end;
    finally
      intl.Free;
    end;
    Result := List.Count;
  finally
    FHashTreeOperations.UnlockList;
  end;
end;

function TOperationsHashTree.IndexOfOperation(op: TPCOperation): Integer;
Var iPosInOrdered : Integer;
  l : TList<Pointer>;
  OpSha256 : TRawBytes;
begin
  OpSha256 := op.Sha256;
  l := FHashTreeOperations.LockList;
  Try
    // Improvement TOperationsHashTree speed 2.1.5.1
    // Use ordered search
    If FindOrderedBySha(l,OpSha256,iPosInOrdered) then begin
      Result := PtrInt(FListOrderedBySha256.Items[iPosInOrdered]);
    end else Result := -1;
  Finally
    FHashTreeOperations.UnlockList;
  End;
end;

function TOperationsHashTree.IndexOfOpReference(const opReference: TOpReference): Integer;
Var l : TList<Pointer>;
begin
  l := FHashTreeOperations.LockList;
  Try
    if not FindOrderedByOpReference(l,opReference,Result) then Result := -1
    else Result := PtrInt(FListOrderedByOpReference.Items[Result]);
  Finally
    FHashTreeOperations.UnlockList;
  End;
end;

function TOperationsHashTree.CountOperationsBySameSignerWithoutFee(account_number: Cardinal): Integer;
Var l : TList<Pointer>;
  i : Integer;
begin
  Result := 0;
  l := FHashTreeOperations.LockList;
  Try
    // Improvement TOperationsHashTree speed 2.1.5.1
    // Use ordered accounts Data search
    If FindOrderedByAccountData(l,account_number,i) then begin
      Result := POperationsHashAccountsData(FListOrderedByAccountsData[i])^.account_without_fee;
    end else Result := 0;
  Finally
    FHashTreeOperations.UnlockList;
  End;
end;

function TOperationsHashTree.InternalAddOperationToHashTree(list: TList<Pointer>; op: TPCOperation; CalcNewHashTree : Boolean) : Boolean;
Var msCopy : TMemoryStream;
  hForNewHash : TRawBytes;
  P : POperationHashTreeReg;
  PaccData : POperationsHashAccountsData;
  i,npos,iListSigners : Integer;
  listSigners : TList<Cardinal>;
begin
  if Not InternalCanAddOperationToHashTree(list,op) then begin
    Result := False;
    Exit;
  end else Result := True; // Will add:
    New(P);
    if Not _PCOperationsStorage.FindPCOperationAndIncCounterIfFound(op) then begin
      msCopy := TMemoryStream.Create;
      try
        P^.Op := TPCOperation( op.NewInstance );
        P^.Op.InitializeData;
        op.SaveOpToStream(msCopy,true);
        msCopy.Position := 0;
        P^.Op.LoadOpFromStream(msCopy, true);
        P^.Op.FPrevious_Signer_updated_block := op.Previous_Signer_updated_block;
        P^.Op.FPrevious_Destination_updated_block := op.FPrevious_Destination_updated_block;
        P^.Op.FPrevious_Seller_updated_block := op.FPrevious_Seller_updated_block;
        P^.Op.FHasValidSignature := op.FHasValidSignature; // Improvement speed v4.0.2 reusing previously signed value
        P^.Op.FUsedPubkeyForSignature := op.FUsedPubkeyForSignature;
        P^.Op.FBufferedSha256:=op.FBufferedSha256;
        P^.Op.CopyUsedPubkeySignatureFrom(op);
        _PCOperationsStorage.AddPCOperation(P^.Op);
      finally
        msCopy.Free;
      end;
    end else P^.Op := op; // Use same!

    // Improvement TOperationsHashTree speed 2.1.6
    // Include to hash tree (Only if CalcNewHashTree=True)
    If (CalcNewHashTree) And (Length(FHashTree)=32) then begin
      // TCrypto.DoSha256(FHashTree+op.Sha256,FHashTree);  COMPILER BUG 2.1.6: Using FHashTree as a "out" param can be initialized prior to be updated first parameter!
      TBaseType.Concat(FHashTree,op.Sha256,hForNewHash);
      TCrypto.DoSha256(hForNewHash,FHashTree);
    end;
    npos := list.Add(P);
    //
    if Not FindOrderedByOpReference(list,op.GetOpReference,i) then begin
      FListOrderedByOpReference.Insert(i, npos);
    end; // TODO: Do not allow duplicate OpReferences?

    // Improvement: Will allow to add duplicate Operations, so add only first to orderedBySha
    If Not FindOrderedBySha(list,op.Sha256,i) then begin
      // Protection: Will add only once
      FListOrderedBySha256.Insert(i, npos);
    end;
    // Improvement TOperationsHashTree speed 2.1.6
    // Mantain an ordered Accounts list with data
    listSigners := TList<Cardinal>.Create;
    try
      op.SignerAccounts(listSigners);
      for iListSigners:=0 to listSigners.Count-1 do begin
        If Not FindOrderedByAccountData(list,listSigners[iListSigners],i) then begin
          New(PaccData);
          PaccData^.account_number:=listSigners[iListSigners];
          PaccData^.account_count:=0;
          PaccData^.account_without_fee:=0;
          FListOrderedByAccountsData.Insert(i,PaccData);
        end else PaccData := FListOrderedByAccountsData[i];
        Inc(PaccData^.account_count);
        If op.OperationFee=0 then begin
          Inc(PaccData^.account_without_fee);
        end;
      end;
    finally
      listSigners.Free;
    end;
  inc(FTotalAmount,op.OperationAmount);
  inc(FTotalFee,op.OperationFee);
  If Assigned(FOnChanged) then FOnChanged(Self);
end;

function TOperationsHashTree.InternalCanAddOperationToHashTree(lockedThreadList : TList<Pointer>; op: TPCOperation): Boolean;
Var PaccData : POperationsHashAccountsData;
  iListSigners,iFound : Integer;
  listSigners : TList<Cardinal>;
begin
  Result := False;
  // Protections:
  // Protect 0-fee operations
  if (op.OperationFee=0) And (FMax0feeOperationsBySigner>=0) then begin
    if (FMax0feeOperationsBySigner=0) then Exit // Not allowed 0-fee operations!
    else if (FMax0feeOperationsBySigner>0) then begin
      listSigners := TList<Cardinal>.Create;
      try
        op.SignerAccounts(listSigners);
        for iListSigners:=0 to listSigners.Count-1 do begin
          If FindOrderedByAccountData(lockedThreadList,(listSigners[iListSigners]),iFound) then begin
            PaccData := FListOrderedByAccountsData[iFound];
            if (PaccData^.account_without_fee>=FMax0feeOperationsBySigner) then Exit; // Limit 0-fee reached
          end;
        end;
      finally
        listSigners.Free;
      end;
    end;
  end;
  Result := True;
end;

function TOperationsHashTree.FindOrderedBySha(lockedThreadList : TList<Pointer>; const Value: TRawBytes; var Index: Integer): Boolean;
var L, H, I : Integer;
  iLockedThreadListPos : PtrInt;
  C : Int64;
  P : POperationHashTreeReg;
begin
  Result := False;
  L := 0;
  H := FListOrderedBySha256.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    iLockedThreadListPos := PtrInt(FListOrderedBySha256[I]);
    C := TBaseType.BinStrComp(POperationHashTreeReg(lockedThreadList[iLockedThreadListPos])^.Op.Sha256,Value);
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

function TOperationsHashTree.FindOrderedByAccountData(lockedThreadList: TList<Pointer>; const account_number: Cardinal; var Index: Integer): Boolean;
var L, H, I : Integer;
  C : Int64;
begin
  Result := False;
  L := 0;
  H := FListOrderedByAccountsData.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Int64(POperationsHashAccountsData(FListOrderedByAccountsData[I])^.account_number) - Int64(account_number);
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

function TOperationsHashTree.FindOrderedByOpReference(lockedThreadList: TList<Pointer>; const Value: TOpReference; var Index: Integer): Boolean;
var L, H, I : Integer;
  iLockedThreadListPos : PtrInt;
  C : Int64;
  P : POperationHashTreeReg;
begin
  Result := False;
  L := 0;
  H := FListOrderedByOpReference.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    iLockedThreadListPos := PtrInt(FListOrderedByOpReference[I]);
    C := Int64(POperationHashTreeReg(lockedThreadList[iLockedThreadListPos])^.Op.GetOpReference) - Int64(Value);
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

function TOperationsHashTree.LoadOperationsHashTreeFromStream(Stream: TStream; LoadingFromStorage : Boolean; LoadProtocolVersion : Word; PreviousUpdatedBlocks : TAccountPreviousBlockInfo; var errors: String): Boolean;
Var c, i: Cardinal;
  OpType: Cardinal;
  bcop: TPCOperation;
  j: Integer;
  OpClass: TPCOperationClass;
  lastNE : TNotifyEvent;
begin
  Result := false;
  //
  If Stream.Read(c, 4)<4 then begin
    errors := 'Cannot read operations count';
    exit;
  end;
  lastNE := FOnChanged;
  FOnChanged:=Nil;
  try
    // c = operations count
    for i := 1 to c do begin
      if Stream.Size - Stream.Position < 4 then begin
        errors := 'Invalid operation structure ' + inttostr(i) + '/' + inttostr(c);
        exit;
      end;
      Stream.Read(OpType, 4);
      j := TPCOperationsComp.IndexOfOperationClassByOpType(OpType);
      if j >= 0 then
        OpClass := _OperationsClass[j]
      else
        OpClass := Nil;
      if Not Assigned(OpClass) then begin
        errors := 'Invalid operation structure ' + inttostr(i) + '/' + inttostr(c) + ' optype not valid:' + InttoHex(OpType, 4);
        exit;
      end;
      bcop := OpClass.Create;
      Try
        if LoadingFromStorage then begin
          If not bcop.LoadFromStorage(Stream,LoadProtocolVersion,PreviousUpdatedBlocks) then begin
            errors := 'Invalid operation load from storage ' + inttostr(i) + '/' + inttostr(c)+' Class:'+OpClass.ClassName;
            exit;
          end;
        end else if not bcop.LoadFromNettransfer(Stream) then begin
          errors := 'Invalid operation load from stream ' + inttostr(i) + '/' + inttostr(c)+' Class:'+OpClass.ClassName;
          exit;
        end;
        AddOperationToHashTree(bcop);
      Finally
        FreeAndNil(bcop);
      end;
    end;
  finally
    FOnChanged := lastNE;
  end;
  If Assigned(FOnChanged) then FOnChanged(Self);
  errors := '';
  Result := true;
end;

procedure TOperationsHashTree.MarkVerifiedECDSASignatures(operationsHashTreeToMark: TOperationsHashTree);
var i, iPosInMyList, nMarkedAsGood, nAlreadyMarked : Integer;
  opToMark, opInMyList : TPCOperation;
  myList, listToMark : TList<Pointer>;
begin
  // Introduced on Build 4.0.2 to increase speed
  // Will search each "operationsHashTreeToMark" operation on my current list. If found, will set same FHasValidSignature in order to mark operation in "operationsHashTreeToMark" as verified
  If Self=operationsHashTreeToMark then Exit;
  nMarkedAsGood := 0;
  nAlreadyMarked := 0;
  myList := FHashTreeOperations.LockList;
  try
    if myList.Count<=0 then Exit; // Nothing to search...
    listToMark := operationsHashTreeToMark.FHashTreeOperations.LockList;
    Try
      if listToMark.Count<=0 then Exit; // Nothing to search...
      for i:=0 to listToMark.Count-1 do begin
        opToMark := POperationHashTreeReg(listToMark[i])^.Op;
        if Not opToMark.FHasValidSignature then begin
          // Check if found
          iPosInMyList := Self.IndexOfOperation(opToMark);
          if (iPosInMyList>=0) then begin
            opInMyList := POperationHashTreeReg(myList[iPosInMyList])^.Op;
            if (opInMyList.FHasValidSignature) then begin
              if (opToMark.FHasValidSignature) then inc(nAlreadyMarked)
              else begin
                opToMark.FHasValidSignature:=True;
                opToMark.FUsedPubkeyForSignature:=opInMyList.FUsedPubkeyForSignature;
                opToMark.CopyUsedPubkeySignatureFrom(opInMyList);
                inc(nMarkedAsGood);
              end;
            end;
          end;
        end;
      end;
      TLog.NewLog(ltdebug,ClassName,Format('Marked %d/%d operations as ValidSignature (%d before) from MemPool with %d operations',[nMarkedAsGood,listToMark.Count,nAlreadyMarked,myList.Count]));
    finally
      operationsHashTreeToMark.FHashTreeOperations.UnlockList;
    end;
  finally
    FHashTreeOperations.UnlockList;
  end;
end;

function TOperationsHashTree.OperationsCount: Integer;
Var l : TList<Pointer>;
begin
  l := FHashTreeOperations.LockList;
  try
    Result := l.Count;
  finally
    FHashTreeOperations.UnlockList;
  end;
end;

procedure TOperationsHashTree.RemoveByOpReference(const opReference: TOpReference);
var i : Integer;
  l : TList<Pointer>;
  iLockedThreadListPos : PtrInt;
begin
  l := FHashTreeOperations.LockList;
  Try
    if FindOrderedByOpReference(l,opReference,i) then begin
      iLockedThreadListPos := PtrInt(FListOrderedByOpReference[i]);
      Delete(iLockedThreadListPos);
    end;
  Finally
    FHashTreeOperations.UnlockList;
  End;
end;

function TOperationsHashTree.SaveOperationsHashTreeToStream(Stream: TStream; SaveToStorage: Boolean): Boolean;
Var c, i, OpType: Cardinal;
  bcop: TPCOperation;
  l : TList<Pointer>;
begin
  l := FHashTreeOperations.LockList;
  Try
    c := l.Count;
    Stream.Write(c, 4);
    // c = operations count
    for i := 1 to c do begin
      bcop := GetOperation(i - 1);
      OpType := bcop.OpType;
      Stream.write(OpType, 4);
      if SaveToStorage then bcop.SaveToStorage(Stream)
      else bcop.SaveToNettransfer(Stream);
    end;
    Result := true;
  Finally
    FHashTreeOperations.UnlockList;
  End;
end;

procedure TOperationsHashTree.SetMax0feeOperationsBySigner(const Value: Integer);
var nv : Integer;
begin
  if Value<0 then nv:=-1
  else nv := Value;
  if nv=FMax0feeOperationsBySigner then Exit;
  FMax0feeOperationsBySigner := nv;
  ClearHastThree;
end;

{ TStorage }

function TStorage.BlockExists(Block: Cardinal): Boolean;
begin
  Result := DoBlockExists(Block);
end;

procedure TStorage.CopyConfiguration(const CopyFrom: TStorage);
begin
  Orphan := CopyFrom.Orphan;
end;

constructor TStorage.Create(AOwner: TComponent);
begin
  inherited;
  FOrphan := '';
  FReadOnly := false;
  FIsMovingBlockchain := False;
end;

procedure TStorage.DeleteBlockChainBlocks(StartingDeleteBlock: Cardinal);
begin
  if ReadOnly then raise Exception.Create('Cannot delete blocks because is ReadOnly');
  DoDeleteBlockChainBlocks(StartingDeleteBlock);
end;

function TStorage.Initialize: Boolean;
begin
  Result := DoInitialize;
end;

function TStorage.CreateSafeBoxStream(blockCount: Cardinal): TStream;
begin
  Result := DoCreateSafeBoxStream(blockCount);
end;

procedure TStorage.EraseStorage;
begin
  TLog.NewLog(ltInfo,ClassName,'Executing EraseStorage');
  DoEraseStorage;
end;

procedure TStorage.SavePendingBufferOperations(OperationsHashTree : TOperationsHashTree);
begin
  DoSavePendingBufferOperations(OperationsHashTree);
end;

procedure TStorage.LoadPendingBufferOperations(OperationsHashTree : TOperationsHashTree);
begin
  DoLoadPendingBufferOperations(OperationsHashTree);
end;

function TStorage.LoadBlockChainBlock(Operations: TPCOperationsComp; Block: Cardinal): Boolean;
begin
  if (Block<FirstBlock) Or (Block>LastBlock) then result := false
  else Result := DoLoadBlockChain(Operations,Block);
end;

function TStorage.MoveBlockChainBlocks(StartBlock: Cardinal; const DestOrphan: TOrphan; DestStorage : TStorage): Boolean;
begin
  if Assigned(DestStorage) then begin
    if DestStorage.ReadOnly then raise Exception.Create('Cannot move blocks because is ReadOnly');
  end else if ReadOnly then raise Exception.Create('Cannot move blocks from myself because is ReadOnly');
  Result := DoMoveBlockChain(StartBlock,DestOrphan,DestStorage);
end;

function TStorage.RestoreBank(max_block: Int64; restoreProgressNotify : TProgressNotify = Nil): Boolean;
begin
  Result := DoRestoreBank(max_block,restoreProgressNotify);
end;

function TStorage.SaveBank(forceSave : Boolean): Boolean;
begin
  Result := true;
  If FIsMovingBlockchain then Exit;
  if (Not forceSave) AND (Not TPCSafeBox.MustSafeBoxBeSaved(Bank.BlocksCount)) then exit; // No save
  Try
    Result := DoSaveBank;
    FBank.SafeBox.CheckMemory;
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,Classname,'Error saving Bank: '+E.Message);
      Raise;
    end;
  End;
end;

function TStorage.SaveBlockChainBlock(Operations: TPCOperationsComp): Boolean;
begin
  Try
    if ReadOnly then raise Exception.Create('Cannot save because is ReadOnly');
    Result := DoSaveBlockChain(Operations);
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,Classname,'Error saving block chain: '+E.Message);
      Raise;
    end;
  End;
end;

procedure TStorage.SetBank(const Value: TPCBank);
begin
  FBank := Value;
end;

procedure TStorage.SetOrphan(const Value: TOrphan);
begin
  FOrphan := Value;
end;

procedure TStorage.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;

{ TPCOperation }

constructor TPCOperation.Create;
begin
  FHasValidSignature := False;
  FBufferedSha256:=Nil;
  FUsedPubkeyForSignature := CT_TECDSA_Public_Nul;
  InitializeData;
end;

destructor TPCOperation.Destroy;
begin
  inherited Destroy;
end;

function TPCOperation.GetBufferForOpHash(UseProtocolV2: Boolean): TRawBytes;
Var ms : TMemoryStream;
begin
  // Protocol v2 change:
  // In previous builds (previous to 2.0) there was a distinct method to
  // save data for ophash and for calculate Sha256 value on merkle tree
  //
  // Starting in v2 we will use only 1 method to do both calcs
  // We will use "UseProtocolV2" bool value to indicate which method
  // want to calc.
  // Note: This method will be overrided by OpTransaction, OpChange and OpRecover only
  if (UseProtocolV2) then begin
    ms := TMemoryStream.Create;
    try
      SaveOpToStream(ms,False);
      ms.Position := 0;
      SetLength(Result,ms.Size);
      ms.ReadBuffer(Result[Low(Result)],ms.Size);
    finally
      ms.Free;
    end;
  end else Raise Exception.Create('ERROR DEV 20170426-1'); // This should never happen, if good coded
end;

class function TPCOperation.GetOperationFromStreamData(StreamData : TBytes): TPCOperation;
  // Loads an TPCOperation saved using "GetOperationStreamData"
  // 1 byte for OpType
  // N bytes for Operation specific data (saved at SaveOpToStream)
Var stream : TStream;
  b : Byte;
  j: Integer;
  OpClass: TPCOperationClass;
  auxOp: TPCOperation;
begin
  Result := Nil;
  stream := TMemoryStream.Create;
  Try
    stream.WriteBuffer(StreamData[0],Length(StreamData)); // Fixed bug 4.0.0
    stream.Position := 0;
    stream.Read(b,1);
    j := TPCOperationsComp.IndexOfOperationClassByOpType(b);
    if j >= 0 then
      OpClass := _OperationsClass[j]
    else Exit;
    auxOp := OpClass.Create;
    if auxOp.LoadOpFromStream(stream,False) then Result := auxOp
    else auxOp.Free;
  Finally
    stream.Free;
  End;
end;

function TPCOperation.GetOperationStreamData: TBytes;
  // OperationStreamData fills an array of bytes with info needed to store an operation
  // 1 byte for OpType
  // N bytes for Operation specific data (saved at SaveOpToStream)
var stream : TStream;
  b : Byte;
begin
  stream := TMemoryStream.Create;
  Try
    b := OpType;
    stream.Write(b,1);
    SaveOpToStream(stream,False);
    SetLength(Result,stream.Size);
    stream.Position := 0;
    stream.ReadBuffer(Result[0],stream.Size); // Fixed bug 4.0.0
  Finally
    stream.Free;
  End;
end;

function TPCOperation.GetOpReference: TOpReference;
  // Described on PIP-0015 by Herman Schoenfeld
  // Will return a 64 bit value composed by SignerAccount (first 4 bytes) and n_Operation (last 4 bytes)
  // Will allow to quick search an Operation in a TOperationsHashTree object
begin
  Result := ((UInt64(SignerAccount) SHL 32) OR UInt64(N_Operation));
end;

class function TPCOperation.GetOpReferenceAccount(const opReference: TOpReference): Cardinal;
begin
  Result := Cardinal(opReference SHR 32);
end;

class function TPCOperation.GetOpReferenceN_Operation(const opReference: TOpReference): Cardinal;
begin
  Result := Cardinal(opReference);
end;

procedure TPCOperation.SignerAccounts(list: TList<Cardinal>);
begin
  list.Clear;
  list.Add(SignerAccount);
end;

class function TPCOperation.DecodeOperationHash(const operationHash: TRawBytes;
  var block, account, n_operation: Cardinal; var md160Hash : TRawBytes) : Boolean;
  { Decodes a previously generated OperationHash }
var ms : TMemoryStream;
begin
  Result := false;
  block :=0;
  account :=0;
  n_operation :=0;
  md160Hash := Nil;
  if length(operationHash)<>32 then exit;
  ms := TMemoryStream.Create;
  try
    ms.Write(operationHash[Low(operationHash)],Length(operationHash));
    ms.position := 0;
    ms.Read(block,4);
    ms.Read(account,4);
    ms.Read(n_operation,4);
    SetLength(md160Hash, 20);
    ms.ReadBuffer(md160Hash[Low(md160Hash)], 20);
    Result := true;
  finally
    ms.free;
  end;
end;

class function TPCOperation.IsValidOperationHash(const AOpHash : String) : Boolean;
var block, account, n_operation: Cardinal; md160Hash : TRawBytes;
begin
  Result := TryParseOperationHash(AOpHash, block, account, n_operation, md160Hash);
end;

class function TPCOperation.TryParseOperationHash(const AOpHash : String; var block, account, n_operation: Cardinal; var md160Hash : TRawBytes) : Boolean;
var
  ophash : TRawBytes;
begin
  ophash := TCrypto.HexaToRaw(Trim(AOpHash));
  if Length(ophash) = 0 then
    Exit(false);
  If not TPCOperation.DecodeOperationHash(ophash,block,account,n_operation,md160Hash) then
    Exit(false);
  Result := true;
end;

class function TPCOperation.EqualOperationHashes(const operationHash1,operationHash2: TRawBytes): Boolean;
  // operationHash1 and operationHash2 must be in RAW format (Not hexadecimal string!)
var b0,b1,b2,r1,r2 : TRawBytes;
begin
  // First 4 bytes of OpHash are block number. If block=0 then is an unknown block, otherwise must match
  b1 := copy(operationHash1,Low(operationHash1),4);
  b2 := copy(operationHash2,Low(operationHash2),4);
  r1 := copy(operationHash1,4,Length(operationHash1)-4);
  r2 := copy(operationHash2,4,Length(operationHash2)-4);
  b0 := TCrypto.HexaToRaw('00000000');
  Result := (TBaseType.BinStrComp(r1,r2)=0) // Both right parts must be equal
    AND ((TBaseType.BinStrComp(b1,b0)=0) Or (TBaseType.BinStrComp(b2,b0)=0) Or (TBaseType.BinStrComp(b1,b2)=0)); // b is 0 value or b1=b2 (b = block number)
end;

class function TPCOperation.FinalOperationHashAsHexa(const operationHash: TRawBytes): String;
begin
  Result := TCrypto.ToHexaString(Copy(operationHash,4,28));
end;

class function TPCOperation.OperationHashAsHexa(const operationHash: TRawBytes): String;
begin
  Result := TCrypto.ToHexaString(operationHash);
end;

procedure TPCOperation.InitializeData;
begin
  FPrevious_Signer_updated_block := 0;
  FPrevious_Destination_updated_block := 0;
  FPrevious_Seller_updated_block := 0;
  FHasValidSignature := false;
  FUsedPubkeyForSignature:=CT_TECDSA_Public_Nul;
  FBufferedSha256 := Nil;
end;

procedure TPCOperation.FillOperationResume(Block: Cardinal; getInfoForAllAccounts : Boolean; Affected_account_number: Cardinal; var OperationResume: TOperationResume);
begin
  //
end;

function TPCOperation.IsValidECDSASignature(const PubKey: TECDSA_Public; current_protocol: Word; const Signature: TECDSA_SIG): Boolean;
begin
  // Will reuse FHasValidSignature if checked previously and was True
  // Introduced on Build 4.0.2 to increase speed using MEMPOOL verified operations instead of verify again everytime
  if (FHasValidSignature) then begin
    If Not TAccountComp.EqualAccountKeys(PubKey,FUsedPubkeyForSignature) then begin
      TLog.NewLog(lterror,ClassName,Format('Detected incorrect previous use of signature used pubkey:%s current pubkey:%s',[TAccountComp.AccountPublicKeyExport(FUsedPubkeyForSignature),TAccountComp.AccountPublicKeyExport(PubKey)]));
      FHasValidSignature := False;
      FUsedPubkeyForSignature := CT_TECDSA_Public_Nul;
    end;
  end;
  if (Not FHasValidSignature) then begin
    FHasValidSignature := TCrypto.ECDSAVerify(PubKey,GetDigestToSign(current_protocol),Signature);
    If FHasValidSignature then begin;
      FUsedPubkeyForSignature := PubKey;
    end;
  end;
  Result := FHasValidSignature;
end;

procedure TPCOperation.CopyUsedPubkeySignatureFrom(SourceOperation: TPCOperation);
begin
  //
end;

function TPCOperation.LoadFromNettransfer(Stream: TStream): Boolean;
begin
  Result := LoadOpFromStream(Stream, False);
end;

function TPCOperation.LoadFromStorage(Stream: TStream; LoadProtocolVersion:Word; APreviousUpdatedBlocks : TAccountPreviousBlockInfo): Boolean;
begin
  Result := false;
  If LoadOpFromStream(Stream, LoadProtocolVersion>=CT_PROTOCOL_2) then begin
    If LoadProtocolVersion<CT_PROTOCOL_3 then begin
      if Stream.Size - Stream.Position<8 then exit;
      Stream.Read(FPrevious_Signer_updated_block,Sizeof(FPrevious_Signer_updated_block));
      Stream.Read(FPrevious_Destination_updated_block,Sizeof(FPrevious_Destination_updated_block));
      if (LoadProtocolVersion=CT_PROTOCOL_2) then begin
        Stream.Read(FPrevious_Seller_updated_block,Sizeof(FPrevious_Seller_updated_block));
      end;
      if Assigned(APreviousUpdatedBlocks) then begin
        // Add to previous list!
        if SignerAccount>=0 then
          APreviousUpdatedBlocks.UpdateIfLower(SignerAccount,FPrevious_Signer_updated_block);
        if DestinationAccount>=0 then
          APreviousUpdatedBlocks.UpdateIfLower(DestinationAccount,FPrevious_Destination_updated_block);
        if SellerAccount>=0 then
          APreviousUpdatedBlocks.UpdateIfLower(SellerAccount,FPrevious_Seller_updated_block);
      end;
    end;
    Result := true;
  end;
end;

class function TPCOperation.OperationHash_OLD(op: TPCOperation; Block : Cardinal): TRawBytes;
  { OperationHash is a 32 bytes value.
    First 4 bytes (0..3) are Block in little endian
    Next 4 bytes (4..7) are Account in little endian
    Next 4 bytes (8..11) are N_Operation in little endian
    Next 20 bytes (12..31) are a RipeMD160 of the operation buffer to hash
    //
    This format is easy to undecode because include account and n_operation
   }
var ms : TMemoryStream;
  r : TRawBytes;
  _a,_o : Cardinal;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(Block,4);
    _a := op.SignerAccount;
    _o := op.N_Operation;
    ms.Write(_a,4);
    ms.Write(_o,4);
    // BUG IN PREVIOUS VERSIONS: (1.5.5 and prior)
    // Function DoRipeMD160 returned a 40 bytes value, because data was converted in hexa string!
    // So, here we used only first 20 bytes, and WHERE HEXA values, so only 16 diff values per 2 byte!
    ms.WriteBuffer(TCrypto.DoRipeMD160_HEXASTRING(op.GetBufferForOpHash(False))[Low(TRawBytes)],20);
    SetLength(Result,ms.size);
    ms.Position:=0;
    ms.Read(Result[Low(Result)],ms.size);
  finally
    ms.Free;
  end;
end;

class function TPCOperation.OperationHashValid(op: TPCOperation; Block : Cardinal): TRawBytes;
  { OperationHash is a 32 bytes value.
    First 4 bytes (0..3) are Block in little endian
    Next 4 bytes (4..7) are Account in little endian
    Next 4 bytes (8..11) are N_Operation in little endian
    Next 20 bytes (12..31) are a RipeMD160 of the SAME data used to calc Sha256
    //
    This format is easy to undecode because include account and n_operation
   }
var ms : TMemoryStream;
  r : TRawBytes;
  _a,_o : Cardinal;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(Block,4); // Save block (4 bytes)
    _a := op.SignerAccount;
    _o := op.N_Operation;
    ms.Write(_a,4);    // Save Account (4 bytes)
    ms.Write(_o,4);    // Save N_Operation (4 bytes)
    ms.WriteBuffer(TCrypto.DoRipeMD160AsRaw(op.GetBufferForOpHash(True))[Low(TRawBytes)],20); // Calling GetBufferForOpHash(TRUE) is the same than data used for Sha256
    SetLength(Result,ms.size);
    ms.Position:=0;
    ms.Read(Result[Low(Result)],ms.size);
  finally
    ms.Free;
  end;
end;

class function TPCOperation.OperationToOperationResume(Block : Cardinal; Operation: TPCOperation; getInfoForAllAccounts : Boolean; Affected_account_number: Cardinal; var OperationResume: TOperationResume): Boolean;
Var s : String;
begin
  OperationResume := CT_TOperationResume_NUL;
  OperationResume.Block:=Block;
  If Operation.SignerAccount=Affected_account_number then begin
    OperationResume.Fee := (-1)*Int64(Operation.OperationFee);
  end;
  OperationResume.AffectedAccount := Affected_account_number;
  OperationResume.OpType:=Operation.OpType;
  OperationResume.SignerAccount := Operation.SignerAccount;
  OperationResume.n_operation := Operation.N_Operation;
  Result := false;
  case Operation.OpType of
    CT_Op_Transaction : Begin
      // Assume that Operation is TOpTransaction
      OperationResume.DestAccount:=TOpTransaction(Operation).Data.target;
      if (TOpTransaction(Operation).Data.opTransactionStyle = transaction_with_auto_buy_account) then begin
        if TOpTransaction(Operation).Data.sender=Affected_account_number then begin
          OperationResume.OpSubtype := CT_OpSubtype_BuyTransactionBuyer;
          OperationResume.OperationTxt := 'Tx-Out (PASA '+TAccountComp.AccountNumberToAccountTxtNumber(TOpTransaction(Operation).Data.target)+' Purchase) '+TAccountComp.FormatMoney(TOpTransaction(Operation).Data.amount)+' PASC from '+
            TAccountComp.AccountNumberToAccountTxtNumber(TOpTransaction(Operation).Data.sender)+' to '+TAccountComp.AccountNumberToAccountTxtNumber(TOpTransaction(Operation).Data.target);
          If (TOpTransaction(Operation).Data.sender=TOpTransaction(Operation).Data.SellerAccount) then begin
            // Valid calc when sender is the same than seller
            OperationResume.Amount := (Int64(TOpTransaction(Operation).Data.amount) - (TOpTransaction(Operation).Data.AccountPrice)) * (-1);
          end else OperationResume.Amount := Int64(TOpTransaction(Operation).Data.amount) * (-1);
          Result := true;
        end else if TOpTransaction(Operation).Data.target=Affected_account_number then begin
          OperationResume.OpSubtype := CT_OpSubtype_BuyTransactionTarget;
          OperationResume.OperationTxt := 'Tx-In (PASA '+TAccountComp.AccountNumberToAccountTxtNumber(TOpTransaction(Operation).Data.target)+' Purchase) '+TAccountComp.FormatMoney(TOpTransaction(Operation).Data.amount)+' PASC from '+
            TAccountComp.AccountNumberToAccountTxtNumber(TOpTransaction(Operation).Data.sender)+' to '+TAccountComp.AccountNumberToAccountTxtNumber(TOpTransaction(Operation).Data.target);
          OperationResume.Amount := Int64(TOpTransaction(Operation).Data.amount) - Int64(TOpTransaction(Operation).Data.AccountPrice);
          OperationResume.Fee := 0;
          Result := true;
        end else if TOpTransaction(Operation).Data.SellerAccount=Affected_account_number then begin
          OperationResume.OpSubtype := CT_OpSubtype_BuyTransactionSeller;
          OperationResume.OperationTxt := 'Tx-In Sold account '+TAccountComp.AccountNumberToAccountTxtNumber(TOpTransaction(Operation).Data.target)+' price '+TAccountComp.FormatMoney(TOpTransaction(Operation).Data.AccountPrice)+' PASC';
          OperationResume.Amount := TOpTransaction(Operation).Data.AccountPrice;
          OperationResume.Fee := 0;
          Result := true;
        end else exit;
      end else begin
        if TOpTransaction(Operation).Data.sender=Affected_account_number then begin
          OperationResume.OpSubtype := CT_OpSubtype_TransactionSender;
          OperationResume.OperationTxt := 'Tx-Out '+TAccountComp.FormatMoney(TOpTransaction(Operation).Data.amount)+' PASC from '+TAccountComp.AccountNumberToAccountTxtNumber(TOpTransaction(Operation).Data.sender)+' to '+TAccountComp.AccountNumberToAccountTxtNumber(TOpTransaction(Operation).Data.target);
          OperationResume.Amount := Int64(TOpTransaction(Operation).Data.amount) * (-1);
          Result := true;
        end else if TOpTransaction(Operation).Data.target=Affected_account_number then begin
          OperationResume.OpSubtype := CT_OpSubtype_TransactionReceiver;
          OperationResume.OperationTxt := 'Tx-In '+TAccountComp.FormatMoney(TOpTransaction(Operation).Data.amount)+' PASC from '+TAccountComp.AccountNumberToAccountTxtNumber(TOpTransaction(Operation).Data.sender)+' to '+TAccountComp.AccountNumberToAccountTxtNumber(TOpTransaction(Operation).Data.target);
          OperationResume.Amount := TOpTransaction(Operation).Data.amount;
          OperationResume.Fee := 0;
          Result := true;
        end else exit;
      end;
    End;
    CT_Op_Changekey : Begin
      OperationResume.OpSubtype := CT_OpSubtype_ChangeKey;
      OperationResume.newKey := TOpChangeKey(Operation).Data.new_accountkey;
      OperationResume.DestAccount := TOpChangeKey(Operation).Data.account_target;
      OperationResume.OperationTxt := 'Change Key to '+TAccountComp.GetECInfoTxt( OperationResume.newKey.EC_OpenSSL_NID );
      Result := true;
    End;
    CT_Op_ChangeKeySigned : Begin
      OperationResume.OpSubtype := CT_OpSubtype_ChangeKeySigned;
      OperationResume.newKey := TOpChangeKeySigned(Operation).Data.new_accountkey;
      OperationResume.DestAccount := TOpChangeKeySigned(Operation).Data.account_target;
      OperationResume.OperationTxt := 'Change '+TAccountComp.AccountNumberToAccountTxtNumber(OperationResume.DestAccount)+' account key to '+TAccountComp.GetECInfoTxt( OperationResume.newKey.EC_OpenSSL_NID );
      Result := true;
    end;
    CT_Op_Recover : Begin
      OperationResume.OpSubtype := CT_OpSubtype_Recover;
      OperationResume.OperationTxt := 'Recover founds';
      Result := true;
    End;
    CT_Op_ListAccountForSale : Begin
      If TOpListAccount(Operation).IsPrivateSale then begin
        OperationResume.OpSubtype := CT_OpSubtype_ListAccountForPrivateSale;
        OperationResume.OperationTxt := 'List account '+TAccountComp.AccountNumberToAccountTxtNumber(TOpListAccount(Operation).Data.account_target)+' for private sale price '+
          TAccountComp.FormatMoney(TOpListAccount(Operation).Data.account_price)+' PASC pay to '+TAccountComp.AccountNumberToAccountTxtNumber(TOpListAccount(Operation).Data.account_to_pay);
      end else begin
        OperationResume.OpSubtype := CT_OpSubtype_ListAccountForPublicSale;
        OperationResume.OperationTxt := 'List account '+TAccountComp.AccountNumberToAccountTxtNumber(TOpListAccount(Operation).Data.account_target)+' for sale price '+
          TAccountComp.FormatMoney(TOpListAccount(Operation).Data.account_price)+' PASC pay to '+TAccountComp.AccountNumberToAccountTxtNumber(TOpListAccount(Operation).Data.account_to_pay);
      end;
      OperationResume.newKey := TOpListAccount(Operation).Data.new_public_key;
      OperationResume.SellerAccount := Operation.SellerAccount;
      Result := true;
    End;
    CT_Op_DelistAccount : Begin
      OperationResume.OpSubtype := CT_OpSubtype_DelistAccount;
      OperationResume.OperationTxt := 'Delist account '+TAccountComp.AccountNumberToAccountTxtNumber(TOpDelistAccountForSale(Operation).Data.account_target)+' for sale';
      Result := true;
    End;
    CT_Op_BuyAccount : Begin
      OperationResume.DestAccount:=TOpBuyAccount(Operation).Data.target;
      if TOpBuyAccount(Operation).Data.sender=Affected_account_number then begin
        OperationResume.OpSubtype := CT_OpSubtype_BuyAccountBuyer;
        OperationResume.OperationTxt := 'Buy account '+TAccountComp.AccountNumberToAccountTxtNumber(TOpBuyAccount(Operation).Data.target)+' for '+TAccountComp.FormatMoney(TOpBuyAccount(Operation).Data.AccountPrice)+' PASC';
        OperationResume.Amount := Int64(TOpBuyAccount(Operation).Data.amount) * (-1);
        Result := true;
      end else if TOpBuyAccount(Operation).Data.target=Affected_account_number then begin
        OperationResume.OpSubtype := CT_OpSubtype_BuyAccountTarget;
        OperationResume.OperationTxt := 'Purchased account '+TAccountComp.AccountNumberToAccountTxtNumber(TOpBuyAccount(Operation).Data.target)+' by '+
          TAccountComp.AccountNumberToAccountTxtNumber(TOpBuyAccount(Operation).Data.sender)+' for '+TAccountComp.FormatMoney(TOpBuyAccount(Operation).Data.AccountPrice)+' PASC';
        OperationResume.Amount := Int64(TOpBuyAccount(Operation).Data.amount) - Int64(TOpBuyAccount(Operation).Data.AccountPrice);
        OperationResume.Fee := 0;
        Result := true;
      end else if TOpBuyAccount(Operation).Data.SellerAccount=Affected_account_number then begin
        OperationResume.OpSubtype := CT_OpSubtype_BuyAccountSeller;
        OperationResume.OperationTxt := 'Sold account '+TAccountComp.AccountNumberToAccountTxtNumber(TOpBuyAccount(Operation).Data.target)+' by '+
          TAccountComp.AccountNumberToAccountTxtNumber(TOpBuyAccount(Operation).Data.sender)+' for '+TAccountComp.FormatMoney(TOpBuyAccount(Operation).Data.AccountPrice)+' PASC';
        OperationResume.Amount := TOpBuyAccount(Operation).Data.AccountPrice;
        OperationResume.Fee := 0;
        Result := true;
      end else exit;
    End;
    CT_Op_ChangeAccountInfo : Begin
      OperationResume.DestAccount := Operation.DestinationAccount;
      s := '';
      if (public_key in TOpChangeAccountInfo(Operation).Data.changes_type) then begin
        s := 'key';
      end;
      if (account_name in TOpChangeAccountInfo(Operation).Data.changes_type) then begin
        if s<>'' then s:=s+',';
        s := s + 'name';
      end;
      if (account_type in TOpChangeAccountInfo(Operation).Data.changes_type) then begin
        if s<>'' then s:=s+',';
        s := s + 'type';
      end;
      OperationResume.OperationTxt:= 'Changed '+s+' of account '+TAccountComp.AccountNumberToAccountTxtNumber(Operation.DestinationAccount);
      OperationResume.OpSubtype:=CT_OpSubtype_ChangeAccountInfo;
      Result := True;
    end;
    CT_Op_MultiOperation : Begin
      OperationResume.isMultiOperation:=True;
      OperationResume.OperationTxt := Operation.ToString;
      OperationResume.Amount := Operation.OperationAmountByAccount(Affected_account_number);
      OperationResume.Fee := 0;
      Result := True;
    end;
    CT_Op_Data : Begin
      Result := True;
    end
  else Exit;
  end;
  OperationResume.OriginalPayload := Operation.OperationPayload;
  If TCrypto.IsHumanReadable(OperationResume.OriginalPayload) then OperationResume.PrintablePayload := OperationResume.OriginalPayload.ToPrintable
  else OperationResume.PrintablePayload := TCrypto.ToHexaString(OperationResume.OriginalPayload);
  OperationResume.OperationHash:=TPCOperation.OperationHashValid(Operation,Block);
  if (Block>0) And (Block<CT_Protocol_Upgrade_v2_MinBlock) then begin
    OperationResume.OperationHash_OLD:=TPCOperation.OperationHash_OLD(Operation,Block);
  end;
  OperationResume.valid := true;
  Operation.FillOperationResume(Block,getInfoForAllAccounts,Affected_account_number,OperationResume);
end;

function TPCOperation.IsSignerAccount(account: Cardinal): Boolean;
begin
  Result := SignerAccount = account;
end;

function TPCOperation.IsAffectedAccount(account: Cardinal): Boolean;
Var l : TList<Cardinal>;
begin
  l := TList<Cardinal>.Create;
  Try
    AffectedAccounts(l);
    Result := (l.IndexOf(account)>=0);
  finally
    l.Free;
  end;
end;

function TPCOperation.DestinationAccount: Int64;
begin
  Result := -1;
end;

function TPCOperation.SellerAccount: Int64;
begin
  Result := -1;
end;

function TPCOperation.GetAccountN_Operation(account: Cardinal): Cardinal;
begin
  If (SignerAccount = account) then Result := N_Operation
  else Result := 0;
end;

function TPCOperation.SaveToNettransfer(Stream: TStream): Boolean;
begin
  Result := SaveOpToStream(Stream,False);
end;

function TPCOperation.SaveToStorage(Stream: TStream): Boolean;
begin
  Result := SaveOpToStream(Stream,True);
end;

function TPCOperation.Sha256: TRawBytes;
begin
  If Length(FBufferedSha256)=0 then begin
    FBufferedSha256 := TCrypto.DoSha256(GetBufferForOpHash(true));
  end;
  Result := FBufferedSha256;
end;

function TPCOperation.OperationAmountByAccount(account: Cardinal): Int64;
begin
  Result := 0;
end;

{ TOperationsResumeList }

Type POperationResume = ^TOperationResume;

procedure TOperationsResumeList.Add(const OperationResume: TOperationResume);
Var P : POperationResume;
begin
  New(P);
  P^ := OperationResume;
  FList.Add(P);
end;

procedure TOperationsResumeList.Clear;
Var P : POperationResume;
  i : Integer;
  l : TList<Pointer>;
begin
  l := FList.LockList;
  try
    for i := 0 to l.Count - 1 do begin
      P := l[i];
      Dispose(P);
    end;
    l.Clear;
  finally
    FList.UnlockList;
  end;
end;

function TOperationsResumeList.Count: Integer;
Var l : TList<Pointer>;
begin
  l := FList.LockList;
  Try
    Result := l.Count;
  Finally
    FList.UnlockList;
  End;
end;

constructor TOperationsResumeList.Create;
begin
  FList := TPCThreadList<Pointer>.Create('TOperationsResumeList_List');
end;

procedure TOperationsResumeList.Delete(index: Integer);
Var P : POperationResume;
  l : TList<Pointer>;
begin
  l := FList.LockList;
  Try
    P := l[index];
    l.Delete(index);
    Dispose(P);
  Finally
    FList.UnlockList;
  End;
end;

destructor TOperationsResumeList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TOperationsResumeList.GetOperationResume(index: Integer): TOperationResume;
Var l : TList<Pointer>;
begin
  l := FList.LockList;
  try
    if index<l.Count then Result := POperationResume(l[index])^
    else Result := CT_TOperationResume_NUL;
  finally
    FList.UnlockList;
  end;
end;

initialization
  SetLength(_OperationsClass, 0);
  RegisterOperationsClass;
  _PCOperationsStorage := TPCOperationsStorage.Create;
finalization
  FreeAndNil(_PCOperationsStorage);
end.

