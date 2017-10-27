unit UBlockChain;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

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
  Classes, UCrypto, UAccounts, ULog, UThread, SyncObjs;
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
  TPCBank = Class;
  TPCBankNotify = Class;
  TPCOperation = Class;
  TPCOperationClass = Class of TPCOperation;

  TOperationResume = Record
    valid : Boolean;
    Block : Cardinal;
    NOpInsideBlock : Integer;
    OpType : Word;
    OpSubtype : Word;
    time : Cardinal;
    AffectedAccount : Cardinal;
    SignerAccount : Int64; // Is the account that executes this operation
    DestAccount : Int64;   //
    SellerAccount : Int64; // Protocol 2 - only used when is a pay to transaction
    newKey : TAccountKey;
    OperationTxt : AnsiString;
    Amount : Int64;
    Fee : Int64;
    Balance : Int64;
    OriginalPayload : TRawBytes;
    PrintablePayload : AnsiString;
    OperationHash : TRawBytes;
    OperationHash_OLD : TRawBytes; // Will include old oeration hash value
    errors : AnsiString;
  end;

  TOperationsResumeList = Class
  private
    FList : TPCThreadList;
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

  { TPCOperation }

  TPCOperation = Class
  Private
    Ftag: integer;
  Protected
    FPrevious_Signer_updated_block: Cardinal;
    FPrevious_Destination_updated_block : Cardinal;
    FPrevious_Seller_updated_block : Cardinal;
    FHasValidSignature : Boolean;
    FBufferedSha256 : TRawBytes;
    procedure InitializeData; virtual;
    function SaveOpToStream(Stream: TStream; SaveExtendedData : Boolean): Boolean; virtual; abstract;
    function LoadOpFromStream(Stream: TStream; LoadExtendedData : Boolean): Boolean; virtual; abstract;
  public
    constructor Create; virtual;
    function GetBufferForOpHash(UseProtocolV2 : Boolean): TRawBytes; virtual;
    function DoOperation(AccountTransaction : TPCSafeBoxTransaction; var errors: AnsiString): Boolean; virtual; abstract;
    procedure AffectedAccounts(list : TList); virtual; abstract;
    class function OpType: Byte; virtual; abstract;
    Class Function OperationToOperationResume(Block : Cardinal; Operation : TPCOperation; Affected_account_number : Cardinal; var OperationResume : TOperationResume) : Boolean;
    function OperationAmount : Int64; virtual; abstract;
    function OperationFee: UInt64; virtual; abstract;
    function OperationPayload : TRawBytes; virtual; abstract;
    function SignerAccount : Cardinal; virtual; abstract;
    function DestinationAccount : Int64; virtual;
    function SellerAccount : Int64; virtual;
    function N_Operation : Cardinal; virtual; abstract;
    Property tag : integer read Ftag Write Ftag;
    function SaveToNettransfer(Stream: TStream): Boolean;
    function LoadFromNettransfer(Stream: TStream): Boolean;
    function SaveToStorage(Stream: TStream): Boolean;
    function LoadFromStorage(Stream: TStream; LoadProtocolV2 : Boolean): Boolean;
    Property Previous_Signer_updated_block : Cardinal read FPrevious_Signer_updated_block;
    Property Previous_Destination_updated_block : Cardinal read FPrevious_Destination_updated_block;
    Property Previous_Seller_updated_block : Cardinal read FPrevious_Seller_updated_block;
    Property HasValidSignature : Boolean read FHasValidSignature;
    Class function OperationHash_OLD(op : TPCOperation; Block : Cardinal) : TRawBytes;
    Class function OperationHashValid(op : TPCOperation; Block : Cardinal) : TRawBytes;
    Class function DecodeOperationHash(Const operationHash : TRawBytes; var block, account,n_operation : Cardinal) : Boolean;
    Class function FinalOperationHashAsHexa(Const operationHash : TRawBytes) : AnsiString;
    function Sha256 : TRawBytes;
  End;

  { TOperationsHashTree }

  TOperationsHashTree = Class
  private
    FHashTreeOperations : TPCThreadList;
    FHashTree: TRawBytes;
    FOnChanged: TNotifyEvent;
    FTotalAmount : Int64;
    FTotalFee : Int64;
    Procedure InternalAddOperationToHashTree(list : TList; op : TPCOperation);
  public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure AddOperationToHashTree(op : TPCOperation);
    Procedure ClearHastThree;
    Property HashTree : TRawBytes read FHashTree;
    Function OperationsCount : Integer;
    Function GetOperation(index : Integer) : TPCOperation;
    Function GetOperationsAffectingAccount(account_number : Cardinal; List : TList) : Integer;
    Procedure CopyFromHashTree(Sender : TOperationsHashTree);
    Property TotalAmount : Int64 read FTotalAmount;
    Property TotalFee : Int64 read FTotalFee;
    function SaveOperationsHashTreeToStream(Stream: TStream; SaveToStorage : Boolean): Boolean;
    function LoadOperationsHashTreeFromStream(Stream: TStream; LoadingFromStorage, LoadProtocolV2 : Boolean; var errors : AnsiString): Boolean;
    function IndexOfOperation(op : TPCOperation) : Integer;
    function CountOperationsBySameSignerWithoutFee(account_number : Cardinal) : Integer;
    Procedure Delete(index : Integer);
    Property OnChanged : TNotifyEvent read FOnChanged write FOnChanged;
  End;

  { TPCOperationsComp }

  TPCOperationsComp = Class(TComponent)
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
    procedure Notification(AComponent: TComponent; Operation: TOperation); Override;
    function SaveBlockToStreamExt(save_only_OperationBlock : Boolean; Stream: TStream; SaveToStorage : Boolean): Boolean;
    function LoadBlockFromStreamExt(Stream: TStream; LoadingFromStorage : Boolean; var errors: AnsiString): Boolean;
  public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Procedure CopyFromExceptAddressKey(Operations : TPCOperationsComp);
    Procedure CopyFrom(Operations : TPCOperationsComp);
    Function AddOperation(Execute : Boolean; op: TPCOperation; var errors: AnsiString): Boolean;
    Function AddOperations(operations: TOperationsHashTree; var errors: AnsiString): Integer;
    Property Operation[index: Integer]: TPCOperation read GetOperation;
    Property bank: TPCBank read FBank write SetBank;
    Procedure Clear(DeleteOperations : Boolean);
    Function Count: Integer;
    Property OperationBlock: TOperationBlock read FOperationBlock;
    Class Function OperationBlockToText(OperationBlock: TOperationBlock) : AnsiString;
    Class Function SaveOperationBlockToStream(Const OperationBlock: TOperationBlock; Stream: TStream) : Boolean;
    Property AccountKey: TAccountKey read GetAccountKey write SetAccountKey;
    Property nonce: Cardinal read GetnOnce write SetnOnce;
    Property timestamp: Cardinal read Gettimestamp write Settimestamp;
    Property BlockPayload : TRawBytes read GetBlockPayload write SetBlockPayload;
    procedure UpdateTimestamp;
    function SaveBlockToStorage(Stream: TStream): Boolean;
    function SaveBlockToStream(save_only_OperationBlock : Boolean; Stream: TStream): Boolean;
    function LoadBlockFromStorage(Stream: TStream; var errors: AnsiString): Boolean;
    function LoadBlockFromStream(Stream: TStream; var errors: AnsiString): Boolean;
    //
    Function ValidateOperationBlock(var errors : AnsiString) : Boolean;
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
  End;

  TPCBankLog = procedure(sender: TPCBank; Operations: TPCOperationsComp; Logtype: TLogType ; Logtxt: AnsiString) of object;

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

  TOrphan = AnsiString;

  { TStorage }

  TStorage = Class(TComponent)
  private
    FOrphan: TOrphan;
    FBank : TPCBank;
    FReadOnly: Boolean;
    procedure SetBank(const Value: TPCBank);
  protected
    procedure SetOrphan(const Value: TOrphan); virtual;
    procedure SetReadOnly(const Value: Boolean); virtual;
    Function DoLoadBlockChain(Operations : TPCOperationsComp; Block : Cardinal) : Boolean; virtual; abstract;
    Function DoSaveBlockChain(Operations : TPCOperationsComp) : Boolean; virtual; abstract;
    Function DoMoveBlockChain(StartBlock : Cardinal; Const DestOrphan : TOrphan; DestStorage : TStorage) : Boolean; virtual; abstract;
    Function DoSaveBank : Boolean; virtual; abstract;
    Function DoRestoreBank(max_block : Int64) : Boolean; virtual; abstract;
    Procedure DoDeleteBlockChainBlocks(StartingDeleteBlock : Cardinal); virtual; abstract;
    Function BlockExists(Block : Cardinal) : Boolean; virtual; abstract;
    function GetFirstBlockNumber: Int64; virtual; abstract;
    function GetLastBlockNumber: Int64; virtual; abstract;
    function DoInitialize:Boolean; virtual; abstract;
    Function DoCreateSafeBoxStream(blockCount : Cardinal) : TStream; virtual; abstract;
    Procedure DoEraseStorage; virtual; abstract;
  public
    Function LoadBlockChainBlock(Operations : TPCOperationsComp; Block : Cardinal) : Boolean;
    Function SaveBlockChainBlock(Operations : TPCOperationsComp) : Boolean;
    Function MoveBlockChainBlocks(StartBlock : Cardinal; Const DestOrphan : TOrphan; DestStorage : TStorage) : Boolean;
    Procedure DeleteBlockChainBlocks(StartingDeleteBlock : Cardinal);
    Function SaveBank : Boolean;
    Function RestoreBank(max_block : Int64) : Boolean;
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
    FNotifyList : TList;
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
    function LoadBankFromStream(Stream : TStream; useSecureLoad : Boolean; var errors : AnsiString) : Boolean;
    Procedure Clear;
    Function LoadOperations(Operations : TPCOperationsComp; Block : Cardinal) : Boolean;
    Property SafeBox : TPCSafeBox read FSafeBox;
    Function AddNewBlockChainBlock(Operations: TPCOperationsComp; MaxAllowedTimestamp : Cardinal; var newBlock: TBlockAccount; var errors: AnsiString): Boolean;
    Procedure DiskRestoreFromOperations(max_block : Int64);
    Procedure NewLog(Operations: TPCOperationsComp; Logtype: TLogType; Logtxt: AnsiString);
    Property OnLog: TPCBankLog read FOnLog write FOnLog;
    Property LastOperationBlock : TOperationBlock read FLastOperationBlock; // TODO: Use
    Property Storage : TStorage read GetStorage;
    Property StorageClass : TStorageClass read FStorageClass write SetStorageClass;
    Function IsReady(Var CurrentProcess : AnsiString) : Boolean;
    Property LastBlockFound : TPCOperationsComp read FLastBlockCache;
    Property UpgradingToV2 : Boolean read FUpgradingToV2;
  End;

Const
  CT_TOperationResume_NUL : TOperationResume = (valid:false;Block:0;NOpInsideBlock:-1;OpType:0;OpSubtype:0;time:0;AffectedAccount:0;SignerAccount:-1;DestAccount:-1;SellerAccount:-1;newKey:(EC_OpenSSL_NID:0;x:'';y:'');OperationTxt:'';Amount:0;Fee:0;Balance:0;OriginalPayload:'';PrintablePayload:'';OperationHash:'';OperationHash_OLD:'';errors:'');

implementation

uses
  {Messages, }
  SysUtils, Variants, {Graphics,}
  {Controls, Forms,}
  Dialogs, {StdCtrls,}
  UTime, UConst, UOpTransaction;

{ TPCBank }

function TPCBank.AccountsCount: Cardinal;
begin
  Result := FSafeBox.AccountsCount;
end;

function TPCBank.AddNewBlockChainBlock(Operations: TPCOperationsComp; MaxAllowedTimestamp : Cardinal; var newBlock: TBlockAccount; var errors: AnsiString): Boolean;
Var
  buffer, pow: AnsiString;
  i : Integer;
begin
  TPCThread.ProtectEnterCriticalSection(Self,FBankLock);
  Try
    Result := False;
    errors := '';
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
      if Not Result then
        NewLog(Operations, lterror, 'Invalid new block '+inttostr(Operations.OperationBlock.block)+': ' + errors);
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
  FLastOperationBlock.initial_safe_box_hash := TCrypto.DoSha256(CT_Genesis_Magic_String_For_Old_Block_Hash); // Genesis hash
  FLastBlockCache.Clear(true);
  NewLog(Nil, ltupdate, 'Clear Bank');
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
  FNotifyList := TList.Create;
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

procedure TPCBank.DiskRestoreFromOperations(max_block : Int64);
Var
  errors: AnsiString;
  newBlock: TBlockAccount;
  Operations: TPCOperationsComp;
  n : Int64;
begin
  if FIsRestoringFromFile then begin
    TLog.NewLog(lterror,Classname,'Is Restoring!!!');
    raise Exception.Create('Is restoring!');
  end;
  TPCThread.ProtectEnterCriticalSection(Self,FBankLock);
  try
    FUpgradingToV2 := NOT Storage.HasUpgradedToVersion2;
    FIsRestoringFromFile := true;
    try
      Clear;
      Storage.Initialize;
      If (max_block<Storage.LastBlock) then n := max_block
      else n := Storage.LastBlock;
      Storage.RestoreBank(n);
      // Restore last blockchain
      if (BlocksCount>0) And (SafeBox.CurrentProtocol=CT_PROTOCOL_1) then begin
        if Not Storage.LoadBlockChainBlock(FLastBlockCache,BlocksCount-1) then begin
          NewLog(nil,lterror,'Cannot find blockchain '+inttostr(BlocksCount-1)+' so cannot accept bank current block '+inttostr(BlocksCount));
          Clear;
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
                {$IFDEF TESTNET}
                Storage.SaveBank;
                {$ELSE}
                If (BlocksCount MOD (CT_BankToDiskEveryNBlocks*10))=0 then begin
                  Storage.SaveBank;
                end;
                {$ENDIF}
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

function TPCBank.IsReady(var CurrentProcess: AnsiString): Boolean;
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

function TPCBank.LoadBankFromStream(Stream: TStream; useSecureLoad : Boolean; var errors: AnsiString): Boolean;
Var LastReadBlock : TBlockAccount;
  i : Integer;
  auxSB : TPCSafeBox;
begin
  auxSB := Nil;
  Try
    If useSecureLoad then begin
      // When on secure load will load Stream in a separate SafeBox, changing only real SafeBox if successfully
      auxSB := TPCSafeBox.Create;
      Result := auxSB.LoadSafeBoxFromStream(Stream,true,LastReadBlock,errors);
      If Not Result then Exit;
    end;
    TPCThread.ProtectEnterCriticalSection(Self,FBankLock);
    try
      If Assigned(auxSB) then begin
        SafeBox.CopyFrom(auxSB);
      end else begin
        Result := SafeBox.LoadSafeBoxFromStream(Stream,false,LastReadBlock,errors);
      end;
      If Not Result then exit;
      If SafeBox.BlocksCount>0 then FLastOperationBlock := SafeBox.Block(SafeBox.BlocksCount-1).blockchainInfo
      else begin
        FLastOperationBlock := TPCOperationsComp.GetFirstBlock;
        FLastOperationBlock.initial_safe_box_hash := TCrypto.DoSha256(CT_Genesis_Magic_String_For_Old_Block_Hash); // Genesis hash
      end;
    finally
      FBankLock.Release; // AntonB If fast close Wallet after open this Error string
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

procedure TPCBank.NewLog(Operations: TPCOperationsComp; Logtype: TLogType; Logtxt: AnsiString);
var s : AnsiString;
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

function TPCOperationsComp.AddOperation(Execute: Boolean; op: TPCOperation; var errors: AnsiString): Boolean;
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
      Result := op.DoOperation(SafeBoxTransaction, errors);
    end else Result := true;
    if Result then begin
      FOperationsHashTree.AddOperationToHashTree(op);
      FOperationBlock.fee := FOperationBlock.fee + op.OperationFee;
      FOperationBlock.operations_hash := FOperationsHashTree.HashTree;
      if FDisableds<=0 then Calc_Digest_Parts;
    end;
  finally
    Unlock;
  end;
End;


function TPCOperationsComp.AddOperations(operations: TOperationsHashTree; var errors: AnsiString): Integer;
Var i : Integer;
  e : AnsiString;
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
  FStreamPoW.WriteBuffer(FDigest_Part1[1],length(FDigest_Part1));
  FStreamPoW.WriteBuffer(FDigest_Part2_Payload[1],length(FDigest_Part2_Payload));
  FStreamPoW.WriteBuffer(FDigest_Part3[1],length(FDigest_Part3));
  FStreamPoW.Write(FOperationBlock.timestamp,4);
  FStreamPoW.Write(FOperationBlock.nonce,4);
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
begin
  Lock;
  Try
    if DeleteOperations then begin
      FOperationsHashTree.ClearHastThree;
      if Assigned(FSafeBoxTransaction) then
        FSafeBoxTransaction.CleanTransaction;
    end;

    // Note:
    // This function does not initializes "account_key" nor "block_payload" fields

    FOperationBlock.timestamp := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
    if Assigned(FBank) then begin
      FOperationBlock.protocol_version := bank.SafeBox.CurrentProtocol;
      If (FOperationBlock.protocol_version=CT_PROTOCOL_1) And (FBank.SafeBox.CanUpgradeToProtocol2) then begin
        FOperationBlock.protocol_version := CT_PROTOCOL_2; // If minting... upgrade to Protocol 2
      end;
      FOperationBlock.block := bank.BlocksCount;
      FOperationBlock.reward := TPascalCoinProtocol.GetRewardForNewLine(bank.BlocksCount);
      FOperationBlock.compact_target := bank.Safebox.GetActualCompactTargetHash(FOperationBlock.protocol_version=CT_PROTOCOL_2);
      FOperationBlock.initial_safe_box_hash := bank.SafeBox.SafeBoxHash;
      If Bank.LastOperationBlock.timestamp>FOperationBlock.timestamp then
        FOperationBlock.timestamp := Bank.LastOperationBlock.timestamp;
    end else begin
      FOperationBlock.block := 0;
      FOperationBlock.reward := TPascalCoinProtocol.GetRewardForNewLine(0);
      FOperationBlock.compact_target := CT_MinCompactTarget;
      FOperationBlock.initial_safe_box_hash := TCrypto.DoSha256(CT_Genesis_Magic_String_For_Old_Block_Hash); // Nothing for first line
      FOperationBlock.protocol_version := CT_PROTOCOL_1;
    end;
    FOperationBlock.operations_hash := FOperationsHashTree.HashTree;
    FOperationBlock.fee := 0;
    FOperationBlock.nonce := 0;
    FOperationBlock.proof_of_work := '';
    FOperationBlock.protocol_available := CT_BlockChain_Protocol_Available;
    FIsOnlyOperationBlock := false;
  Finally
    try
      CalcProofOfWork(true,FOperationBlock.proof_of_work);
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
    FOperationBlock.compact_target := Bank.Safebox.GetActualCompactTargetHash(FOperationBlock.protocol_version=CT_PROTOCOL_2);
    FIsOnlyOperationBlock := Operations.FIsOnlyOperationBlock;
    FOperationsHashTree.CopyFromHashTree(Operations.FOperationsHashTree);
    FOperationBlock.operations_hash := FOperationsHashTree.HashTree;
    if Assigned(FSafeBoxTransaction) And Assigned(Operations.FSafeBoxTransaction) then begin
      FSafeBoxTransaction.CopyFrom(Operations.FSafeBoxTransaction);
    end;
    // Recalc all
    CalcProofOfWork(true,FOperationBlock.proof_of_work);
  finally
    Unlock;
  end;
end;

function TPCOperationsComp.Count: Integer;
begin
  Result := FOperationsHashTree.OperationsCount;
end;

constructor TPCOperationsComp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOperationsLock := TPCCriticalSection.Create('TPCOperationsComp_OPERATIONSLOCK');
  FDisableds := 0;
  FStreamPoW := TMemoryStream.Create;
  FStreamPoW.Position := 0;
  FOperationsHashTree := TOperationsHashTree.Create;
  FOperationsHashTree.OnChanged:= OnOperationsHashTreeChanged;
  FBank := Nil;
  FOperationBlock := GetFirstBlock;
  FSafeBoxTransaction := Nil;
  if Assigned(AOwner) And (AOwner is TPCBank) then begin
    Bank := TPCBank(AOwner);
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
           And (OperationBlock1.block_payload=OperationBlock2.block_payload)
           And (OperationBlock1.initial_safe_box_hash=OperationBlock2.initial_safe_box_hash)
           And (OperationBlock1.operations_hash=OperationBlock2.operations_hash)
           And (OperationBlock1.proof_of_work=OperationBlock2.proof_of_work);
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

function TPCOperationsComp.LoadBlockFromStorage(Stream: TStream; var errors: AnsiString): Boolean;
begin
  Result := LoadBlockFromStreamExt(Stream,true,errors);
end;

function TPCOperationsComp.LoadBlockFromStream(Stream: TStream; var errors: AnsiString): Boolean;
begin
  Result := LoadBlockFromStreamExt(Stream,false,errors);
end;

function TPCOperationsComp.LoadBlockFromStreamExt(Stream: TStream; LoadingFromStorage: Boolean; var errors: AnsiString): Boolean;
Var i: Cardinal;
  lastfee : UInt64;
  soob : Byte;
  m: AnsiString;
  load_protocol_v2 : Boolean;
begin
  Lock;
  Try
    Clear(true);
    Result := False;
    //
    errors := 'Invalid protocol structure. Check application version!';
    if (Stream.Size - Stream.Position < 5) then exit;
    Stream.Read(soob,1);
    // About soob var:
    // In build prior to 1.0.4 soob only can have 2 values: 0 or 1
    // In build 1.0.4 soob can has 2 more values: 2 or 3
    // In build 2.0 soob can has 1 more value: 4
    // In future, old values 0 and 1 will no longer be used!
    // - Value 0 and 2 means that contains also operations
    // - Value 1 and 3 means that only contains operationblock info
    // - Value 2 and 3 means that contains protocol info prior to block number
    // - Value 4 means that is loading from storage using protocol v2 (so, includes always operations)
    load_protocol_v2 := false;
    if (soob in [0,2]) then FIsOnlyOperationBlock:=false
    else if (soob in [1,3]) then FIsOnlyOperationBlock:=true
    else if (soob in [4]) then begin
      FIsOnlyOperationBlock:=false;
      load_protocol_v2 := true;
    end else begin
      errors := 'Invalid value in protocol header! Found:'+inttostr(soob)+' - Check if your application version is Ok';
      exit;
    end;

    if (soob in [2,3,4]) then begin
      Stream.Read(FOperationBlock.protocol_version, Sizeof(FOperationBlock.protocol_version));
      Stream.Read(FOperationBlock.protocol_available, Sizeof(FOperationBlock.protocol_available));
    end else begin
      // We assume that protocol_version is 1 and protocol_available is 0
      FOperationBlock.protocol_version := 1;
      FOperationBlock.protocol_available := 0;
    end;

    if Stream.Read(FOperationBlock.block, Sizeof(FOperationBlock.block))<0 then exit;

    if TStreamOp.ReadAnsiString(Stream, m) < 0 then exit;
    FOperationBlock.account_key := TAccountComp.RawString2Accountkey(m);
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
    Result := FOperationsHashTree.LoadOperationsHashTreeFromStream(Stream,LoadingFromStorage,load_protocol_v2,errors);
    if not Result then begin
      exit;
    end;
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

procedure TPCOperationsComp.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then begin
    if AComponent = FBank then begin
      FBank := Nil;
      FreeAndNil(FSafeBoxTransaction);
    end;
  end;
end;

class function TPCOperationsComp.OperationBlockToText(OperationBlock: TOperationBlock): AnsiString;
begin
  Result := Format('Block:%d Timestamp:%d Reward:%d Fee:%d Target:%d PoW:%s',[operationBlock.block,
    operationblock.timestamp,operationblock.reward,operationblock.fee, OperationBlock.compact_target, TCrypto.ToHexaString(operationblock.proof_of_work)]);
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
Var i,n,lastn : Integer;
  op : TPCOperation;
  errors : AnsiString;
  aux,aux2 : TOperationsHashTree;
begin
  Lock;
  Try
    FOperationBlock.timestamp := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
    if Assigned(FBank) then begin
      FOperationBlock.protocol_version := bank.SafeBox.CurrentProtocol;
      If (FOperationBlock.protocol_version=CT_PROTOCOL_1) And (FBank.SafeBox.CanUpgradeToProtocol2) then begin
        TLog.NewLog(ltinfo,ClassName,'New miner protocol version to 2 at sanitize');
        FOperationBlock.protocol_version := CT_PROTOCOL_2;
      end;
      FOperationBlock.block := bank.BlocksCount;
      FOperationBlock.reward := TPascalCoinProtocol.GetRewardForNewLine(bank.BlocksCount);
      FOperationBlock.compact_target := bank.SafeBox.GetActualCompactTargetHash(FOperationBlock.protocol_version=CT_PROTOCOL_2);
      FOperationBlock.initial_safe_box_hash := bank.SafeBox.SafeBoxHash;
      If Bank.LastOperationBlock.timestamp>FOperationBlock.timestamp then
        FOperationBlock.timestamp := Bank.LastOperationBlock.timestamp;
    end else begin
      FOperationBlock.block := 0;
      FOperationBlock.reward := TPascalCoinProtocol.GetRewardForNewLine(0);
      FOperationBlock.compact_target := CT_MinCompactTarget;
      FOperationBlock.initial_safe_box_hash := TCrypto.DoSha256(CT_Genesis_Magic_String_For_Old_Block_Hash);
      FOperationBlock.protocol_version := CT_PROTOCOL_1;
    end;
    FOperationBlock.proof_of_work := '';
    FOperationBlock.protocol_available := CT_BlockChain_Protocol_Available;
    n := 0;
    FOperationBlock.fee := 0;
    //
    SafeBoxTransaction.CleanTransaction;
    //
    aux := TOperationsHashTree.Create;
    Try
      lastn := FOperationsHashTree.OperationsCount;
      for i:=0 to lastn-1 do begin
        op := FOperationsHashTree.GetOperation(i);
        if (op.DoOperation(SafeBoxTransaction,errors)) then begin
          inc(n);
          aux.AddOperationToHashTree(op);
          inc(FOperationBlock.fee,op.OperationFee);
          TLog.NewLog(ltdebug,Classname,'Sanitizing (pos:'+inttostr(i+1)+'/'+inttostr(lastn)+'): '+op.ToString);
        end;
      end;
    Finally
      aux2 := FOperationsHashTree;
      FOperationsHashTree := aux;
      aux2.Free;
      FOperationBlock.operations_hash := FOperationsHashTree.HashTree;
    End;
  Finally
    CalcProofOfWork(true,FOperationBlock.proof_of_work);
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
        // Introduced on protocol v2: soob = 4 when saving to storage
        soob := 4;
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
    value.FreeNotification(Self);
    FSafeBoxTransaction := TPCSafeBoxTransaction.Create(FBank.SafeBox);
  end;
  Clear(true);
end;

procedure TPCOperationsComp.SetBlockPayload(const Value: TRawBytes);
Var i : Integer;
begin
  Lock;
  Try
    if Value=FOperationBlock.block_payload then exit;
    if Length(Value)>CT_MaxPayloadSize then Exit;
    // Checking Miner Payload valid chars
    for i := 1 to length(Value) do begin
      if Not (Value[i] in [#32..#254]) then begin
        exit;
      end;
    end;
    FOperationBlock.block_payload := Value;
    CalcProofOfWork(true,FOperationBlock.proof_of_work);
  finally
    Unlock;
  end;
end;

procedure TPCOperationsComp.OnOperationsHashTreeChanged(Sender: TObject);
begin
  FOperationBlock.operations_hash:=FOperationsHashTree.HashTree;
  Calc_Digest_Part3;
end;

procedure TPCOperationsComp.SetnOnce(const value: Cardinal);
begin
  Lock;
  Try
    FOperationBlock.nonce := value;
    CalcProofOfWork(false,FOperationBlock.proof_of_work);
  finally
    Unlock;
  end;
end;

procedure TPCOperationsComp.Settimestamp(const value: Cardinal);
begin
  Lock;
  Try
    if FOperationBlock.timestamp=Value then exit; // No change, nothing to do
    FOperationBlock.timestamp := value;
    CalcProofOfWork(false,FOperationBlock.proof_of_work);
  finally
    Unlock;
  end;
end;

procedure TPCOperationsComp.UpdateTimestamp;
Var ts : Cardinal;
begin
  Lock;
  Try
    ts := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
    if Assigned(bank) then begin
      If Bank.FLastOperationBlock.timestamp>ts then ts := Bank.FLastOperationBlock.timestamp;
    end;
    timestamp := ts;
  finally
    Unlock;
  end;
end;

function TPCOperationsComp.ValidateOperationBlock(var errors : AnsiString): Boolean;
Var lastpow : AnsiString;
  i : Integer;
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
    for i := 0 to Count - 1 do begin
      If Not Operation[i].DoOperation(SafeBoxTransaction,errors) then begin
        errors := 'Error executing operation '+inttostr(i+1)+'/'+inttostr(Count)+': '+errors;
        exit;
      end;
    end;
    // Check OperationsHash value is valid
    if FOperationsHashTree.HashTree<>OperationBlock.operations_hash then begin
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

procedure TOperationsHashTree.AddOperationToHashTree(op: TPCOperation);
Var l : TList;
begin
  l := FHashTreeOperations.LockList;
  try
    InternalAddOperationToHashTree(l,op);
  finally
    FHashTreeOperations.UnlockList;
  end;
end;

procedure TOperationsHashTree.ClearHastThree;
var l : TList;
  i : Integer;
  P : POperationHashTreeReg;
begin
  l := FHashTreeOperations.LockList;
  try
    FTotalAmount := 0;
    FTotalFee := 0;
    Try
      for i := 0 to l.Count - 1 do begin
        P := l[i];
        P^.Op.Free;
        Dispose(P);
      end;
    Finally
      l.Clear;
      FHashTree := TCrypto.DoSha256('');
    End;
    If Assigned(FOnChanged) then FOnChanged(Self);
  finally
    FHashTreeOperations.UnlockList;
  end;
end;

procedure TOperationsHashTree.CopyFromHashTree(Sender: TOperationsHashTree);
Var i : Integer;
  lme, lsender : TList;
  PSender : POperationHashTreeReg;
  lastNE : TNotifyEvent;
begin
  if (Sender = Self) then begin
    exit;
  end;
  ClearHastThree;
  lme := FHashTreeOperations.LockList;
  try
    lastNE := FOnChanged;
    FOnChanged := Nil;
    try
      lsender := Sender.FHashTreeOperations.LockList;
      try
        for i := 0 to lsender.Count - 1 do begin
          PSender := lsender[i];
          InternalAddOperationToHashTree(lme,PSender^.Op);
        end;
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
  FTotalAmount := 0;
  FTotalFee := 0;
  FHashTree := TCrypto.DoSha256('');
  FHashTreeOperations := TPCThreadList.Create('TOperationsHashTree_HashTreeOperations');
end;

procedure TOperationsHashTree.Delete(index: Integer);
Var l : TList;
  P : POperationHashTreeReg;
  i : Integer;
begin
  l := FHashTreeOperations.LockList;
  try
    P := l[index];
    l.Delete(index);
    P^.Op.Free;
    Dispose(P);
    // Recalc operations hash
    FTotalAmount := 0;
    FTotalFee := 0;
    FHashTree := '';
    for i := 0 to l.Count - 1 do begin
      P := l[i];
      // Include to hash tree
      FHashTree := TCrypto.DoSha256(FHashTree+P^.Op.Sha256);
      P^.Op.tag := i;
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
  SetLength(FHashTree,0);
  inherited;
end;

function TOperationsHashTree.GetOperation(index: Integer): TPCOperation;
Var l : TList;
begin
  l := FHashTreeOperations.LockList;
  try
    Result := POperationHashTreeReg(l[index])^.Op;
  finally
    FHashTreeOperations.UnlockList;
  end;
end;

function TOperationsHashTree.GetOperationsAffectingAccount(account_number: Cardinal; List: TList): Integer;
  // This function retrieves operations from HashTree that affeccts to an account_number
Var l,intl : TList;
  i,j : Integer;
begin
  List.Clear;
  l := FHashTreeOperations.LockList;
  try
    intl := TList.Create;
    try
      for i := 0 to l.Count - 1 do begin
        intl.Clear;
        POperationHashTreeReg(l[i])^.Op.AffectedAccounts(intl);
        if intl.IndexOf(TObject(account_number))>=0 then List.Add(TObject(i));
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
Var
  l : TList;
  OpSha256 : TRawBytes;
begin
  OpSha256 := op.Sha256;
  l := FHashTreeOperations.LockList;
  Try
    for Result := 0 to l.Count - 1 do begin
      if POperationHashTreeReg(l[Result])^.Op.Sha256=OpSha256 then exit;
    end;
    Result := -1;
  Finally
    FHashTreeOperations.UnlockList;
  End;
end;

function TOperationsHashTree.CountOperationsBySameSignerWithoutFee(account_number: Cardinal): Integer;
Var l : TList;
  i : Integer;
begin
  Result := 0;
  l := FHashTreeOperations.LockList;
  Try
    for i := 0 to l.Count - 1 do begin
      if (POperationHashTreeReg(l[i])^.Op.SignerAccount=account_number) And (POperationHashTreeReg(l[i])^.Op.OperationFee=0) then inc(Result);
    end;
  Finally
    FHashTreeOperations.UnlockList;
  End;
end;

procedure TOperationsHashTree.InternalAddOperationToHashTree(list: TList; op: TPCOperation);
Var msCopy : TMemoryStream;
  h : TRawBytes;
  P : POperationHashTreeReg;
begin
  msCopy := TMemoryStream.Create;
  try
    New(P);
    P^.Op := TPCOperation( op.NewInstance );
    P^.Op.InitializeData;
    op.SaveOpToStream(msCopy,true);
    msCopy.Position := 0;
    P^.Op.LoadOpFromStream(msCopy, true);
    P^.Op.FPrevious_Signer_updated_block := op.Previous_Signer_updated_block;
    P^.Op.FPrevious_Destination_updated_block := op.FPrevious_Destination_updated_block;
    P^.Op.FPrevious_Seller_updated_block := op.FPrevious_Seller_updated_block;
    h := op.Sha256;
    P^.Op.tag := list.Count;
    // Include to hash tree
    FHashTree := TCrypto.DoSha256(FHashTree+h);
    list.Add(P);
  finally
    msCopy.Free;
  end;
  inc(FTotalAmount,op.OperationAmount);
  inc(FTotalFee,op.OperationFee);
  If Assigned(FOnChanged) then FOnChanged(Self);
end;

function TOperationsHashTree.LoadOperationsHashTreeFromStream(Stream: TStream; LoadingFromStorage, LoadProtocolV2: Boolean; var errors: AnsiString): Boolean;
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
      errors := 'Invalid operation structure ' + inttostr(i) + '/' + inttostr(c);
      if Stream.Size - Stream.Position < 4 then exit;
      Stream.Read(OpType, 4);
      j := TPCOperationsComp.IndexOfOperationClassByOpType(OpType);
      if j >= 0 then
        OpClass := _OperationsClass[j]
      else
        OpClass := Nil;
      if Not Assigned(OpClass) then begin
        errors := errors + ' optype not valid:' + InttoHex(OpType, 4);
        exit;
      end;
      errors := 'Invalid operation load from stream ' + inttostr(i) + '/' + inttostr(c)+' Class:'+OpClass.ClassName;
      bcop := OpClass.Create;
      Try
        if LoadingFromStorage then begin
          If not bcop.LoadFromStorage(Stream,LoadProtocolV2) then exit;
        end else if not bcop.LoadFromNettransfer(Stream) then begin
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
  Result := true;
end;

function TOperationsHashTree.OperationsCount: Integer;
Var l : TList;
begin
  l := FHashTreeOperations.LockList;
  try
    Result := l.Count;
  finally
    FHashTreeOperations.UnlockList;
  end;
end;

function TOperationsHashTree.SaveOperationsHashTreeToStream(Stream: TStream; SaveToStorage: Boolean): Boolean;
Var c, i, OpType: Cardinal;
  bcop: TPCOperation;
  l : TList;
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

{ TStorage }

procedure TStorage.CopyConfiguration(const CopyFrom: TStorage);
begin
  Orphan := CopyFrom.Orphan;
end;

constructor TStorage.Create(AOwner: TComponent);
begin
  inherited;
  FOrphan := '';
  FReadOnly := false;
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

function TStorage.RestoreBank(max_block: Int64): Boolean;
begin
  Result := DoRestoreBank(max_block);
end;

function TStorage.SaveBank: Boolean;
begin
  Result := true;
  if Not TPCSafeBox.MustSafeBoxBeSaved(Bank.BlocksCount) then exit; // No save
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
  FBufferedSha256:='';
  InitializeData;
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
      setlength(Result,ms.Size);
      ms.ReadBuffer(Result[1],ms.Size);
    finally
      ms.Free;
    end;
  end else Raise Exception.Create('ERROR DEV 20170426-1'); // This should never happen, if good coded
end;

class function TPCOperation.DecodeOperationHash(const operationHash: TRawBytes;
  var block, account, n_operation: Cardinal): Boolean;
  { Decodes a previously generated OperationHash }
var ms : TMemoryStream;
begin
  Result := false;
  account:=0;
  n_operation:=0;
  if length(operationHash)<>32 then exit;
  ms := TMemoryStream.Create;
  try
    ms.Write(operationHash[1],length(operationHash));
    ms.position := 0;
    ms.Read(block,4);
    ms.Read(account,4);
    ms.Read(n_operation,4);
    Result := true;
  finally
    ms.free;
  end;
end;

class function TPCOperation.FinalOperationHashAsHexa(const operationHash: TRawBytes): AnsiString;
begin
  Result := TCrypto.ToHexaString(Copy(operationHash,5,28));
end;

procedure TPCOperation.InitializeData;
begin
  FTag := 0;
  FPrevious_Signer_updated_block := 0;
  FPrevious_Destination_updated_block := 0;
  FPrevious_Seller_updated_block := 0;
  FHasValidSignature := false;
  FBufferedSha256:='';
end;

function TPCOperation.LoadFromNettransfer(Stream: TStream): Boolean;
begin
  Result := LoadOpFromStream(Stream, False);
end;

function TPCOperation.LoadFromStorage(Stream: TStream; LoadProtocolV2:Boolean): Boolean;
begin
  Result := false;
  If LoadOpFromStream(Stream, LoadProtocolV2) then begin
    if Stream.Size - Stream.Position<8 then exit;
    Stream.Read(FPrevious_Signer_updated_block,Sizeof(FPrevious_Signer_updated_block));
    Stream.Read(FPrevious_Destination_updated_block,Sizeof(FPrevious_Destination_updated_block));
    if (LoadProtocolV2) then begin
      Stream.Read(FPrevious_Seller_updated_block,Sizeof(FPrevious_Seller_updated_block));
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
    ms.WriteBuffer(TCrypto.DoRipeMD160_HEXASTRING(op.GetBufferForOpHash(False))[1],20);
    SetLength(Result,ms.size);
    ms.Position:=0;
    ms.Read(Result[1],ms.size);
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
    ms.WriteBuffer(TCrypto.DoRipeMD160AsRaw(op.GetBufferForOpHash(True))[1],20); // Calling GetBufferForOpHash(TRUE) is the same than data used for Sha256
    SetLength(Result,ms.size);
    ms.Position:=0;
    ms.Read(Result[1],ms.size);
  finally
    ms.Free;
  end;
end;

class function TPCOperation.OperationToOperationResume(Block : Cardinal; Operation: TPCOperation;
  Affected_account_number: Cardinal;
  var OperationResume: TOperationResume): Boolean;
Var spayload : AnsiString;
  s : AnsiString;
begin
  OperationResume := CT_TOperationResume_NUL;
  OperationResume.Block:=Block;
  If Operation.SignerAccount=Affected_account_number then begin
    OperationResume.Fee := (-1)*Int64(Operation.OperationFee);
  end;
  OperationResume.AffectedAccount := Affected_account_number;
  OperationResume.OpType:=Operation.OpType;
  OperationResume.SignerAccount := Operation.SignerAccount;
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
    end
  else Exit;
  end;
  OperationResume.OriginalPayload := Operation.OperationPayload;
  If TCrypto.IsHumanReadable(OperationResume.OriginalPayload) then OperationResume.PrintablePayload := OperationResume.OriginalPayload
  else OperationResume.PrintablePayload := TCrypto.ToHexaString(OperationResume.OriginalPayload);
  OperationResume.OperationHash:=TPCOperation.OperationHashValid(Operation,Block);
  if (Block<CT_Protocol_Upgrade_v2_MinBlock) then begin
    OperationResume.OperationHash_OLD:=TPCOperation.OperationHash_OLD(Operation,Block);
  end;
  OperationResume.valid := true;
end;

function TPCOperation.DestinationAccount: Int64;
begin
  Result := -1;
end;

function TPCOperation.SellerAccount: Int64;
begin
  Result := -1;
end;

function TPCOperation.SaveToNettransfer(Stream: TStream): Boolean;
begin
  Result := SaveOpToStream(Stream,False);
end;

function TPCOperation.SaveToStorage(Stream: TStream): Boolean;
begin
  Result := SaveOpToStream(Stream,True);
  Stream.Write(FPrevious_Signer_updated_block,Sizeof(FPrevious_Signer_updated_block));
  Stream.Write(FPrevious_Destination_updated_block,SizeOf(FPrevious_Destination_updated_block));
  Stream.Write(FPrevious_Seller_updated_block,SizeOf(FPrevious_Seller_updated_block));
  Result := true;
end;

function TPCOperation.Sha256: TRawBytes;
begin
  If FBufferedSha256='' then begin
    FBufferedSha256 := TCrypto.DoSha256(GetBufferForOpHash(true));
  end;
  Result := FBufferedSha256;
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
  l : TList;
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
Var l : TList;
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
  FList := TPCThreadList.Create('TOperationsResumeList_List');
end;

procedure TOperationsResumeList.Delete(index: Integer);
Var P : POperationResume;
  l : TList;
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
Var l : TList;
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
finalization

end.
