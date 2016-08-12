﻿unit UBlockChain;

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
  Classes, UCrypto, UAccounts, Windows, ULog;


Type
  TOperationBlock = Record
    block: Cardinal;
    account_key: TAccountKey;
    reward: UInt64;
    fee: UInt64;
    protocol_version: Word;     // Protocol version
    protocol_available: Word;   // Used to upgrade protocol
    timestamp: Cardinal;        // Timestamp creation
    compact_target: Cardinal;   // Target in compact form
    nonce: Cardinal;            // Random value to generate a new P-o-W
    block_payload : TRawBytes;  // RAW Payload that a miner can include to a blockchain
    initial_safe_box_hash: TRawBytes; // RAW Safe Box Hash value (32 bytes, it's a Sha256)
    operations_hash: TRawBytes; // RAW sha256 (32 bytes) of Operations
    proof_of_work: TRawBytes;   // RAW Double Sha256
  end;

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

  TPCOperation = Class
  Private
    Ftag: integer;
    FAuxBalance: Int64;
  public
    function GetOperationBufferToHash: TRawBytes; virtual; abstract;
    function DoOperation(AccountTransaction : TPCSafeBoxTransaction; var errors: AnsiString): Boolean; virtual; abstract;
    function SaveToStream(Stream: TStream): Boolean; virtual; abstract;
    function LoadFromStream(Stream: TStream): Boolean; virtual; abstract;
    procedure AffectedAccounts(list : TList); virtual; abstract;
    class function OpType: Byte; virtual; abstract;
    function OperationAmount : Int64; virtual; abstract;
    function OperationFee: UInt64; virtual; abstract;
    function OperationPayload : TRawBytes; virtual; abstract;
    function SenderAccount : Cardinal; virtual; abstract;
    Property tag : integer read Ftag Write Ftag;
    Property AuxBalance : Int64 read FAuxBalance Write FAuxBalance;
    Class Function IsReadablePayload(const Payload : TRawBytes) : Boolean;
  End;

  TOperationsHashTree = Class
  private
    FHashTreeOperations : TThreadList;
    FHashTree: TRawBytes;
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
  End;

  TPCOperationsComp = Class(TComponent)
  private
    FBank: TPCBank;
    FSafeBoxTransaction : TPCSafeBoxTransaction;
    FOperationBlock: TOperationBlock;
    FOperationsHashTree : TOperationsHashTree;
    FDigest_Basic : TRawBytes;
    FDigest_Operations : TRawBytes;
    FIsOnlyOperationBlock: Boolean;
    function GetOperation(index: Integer): TPCOperation;
    procedure SetBank(const value: TPCBank);
    procedure SetnOnce(const value: Cardinal);
    procedure Settimestamp(const value: Cardinal);
    function GetnOnce: Cardinal;
    function Gettimestamp: Cardinal;
    procedure SetAccountKey(const value: TAccountKey);
    function GetAccountKey: TAccountKey;
    Procedure Calc_Digest_Basic;
    Procedure Calc_Digest_Operations;
    Function CalcProofOfWork(fullcalculation : Boolean): TRawBytes;
    function GetBlockPayload: TRawBytes;
    procedure SetBlockPayload(const Value: TRawBytes);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); Override;
  public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Procedure CopyFromExceptAddressKey(Operations : TPCOperationsComp);
    Function CopyFromAndValidate(Operations : TPCOperationsComp; var errors : AnsiString) : Boolean;
    Function AddOperation(Execute : Boolean; op: TPCOperation; var errors: AnsiString): Boolean;
    Function AddOperations(operations: TOperationsHashTree; var errors: AnsiString): Integer;
    Property Operation[index: Integer]: TPCOperation read GetOperation;
    Property bank: TPCBank read FBank write SetBank;
    Procedure Clear(DeleteOperations : Boolean);
    Function Count: Integer;
    Property OperationBlock: TOperationBlock read FOperationBlock;
    Class Function OperationBlockToText(OperationBlock: TOperationBlock) : AnsiString;
    Property AccountKey: TAccountKey read GetAccountKey write SetAccountKey;
    Property nonce: Cardinal read GetnOnce write SetnOnce;
    Property timestamp: Cardinal read Gettimestamp write Settimestamp;
    Property BlockPayload : TRawBytes read GetBlockPayload write SetBlockPayload;
    Function IncrementNOnce: Boolean;
    procedure UpdateTimestamp;
    function SaveToStream(save_header, save_only_OperationBlock : Boolean; Stream: TStream): Boolean;
    function LoadFromStream(ExecuteOperations : Boolean; load_header : Boolean; Stream: TStream; var errors: AnsiString): Boolean;
    //
    Function ValidateOperationBlock(var errors : AnsiString) : Boolean;
    Property IsOnlyOperationBlock : Boolean read FIsOnlyOperationBlock;
    //
    Procedure SanitizeOperations;

    Class Function RegisterOperationClass(OpClass: TPCOperationClass): Boolean;
    Class Function IndexOfOperationClass(OpClass: TPCOperationClass): Integer;
    Class Function IndexOfOperationClassByOpType(OpType: Cardinal): Integer;
    Class Function GetOperationClassByOpType(OpType: Cardinal): TPCOperationClass;
    Class Function GetFirstBlock : TOperationBlock;
    //
    Property SafeBoxTransaction : TPCSafeBoxTransaction read FSafeBoxTransaction;
    Property OperationsHashTree : TOperationsHashTree read FOperationsHashTree;
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

  TStorage = Class(TComponent)
  private
    FOrphan: TOrphan;
    FBank : TPCBank;
    procedure SetBank(const Value: TPCBank);
  protected
    procedure SetOrphan(const Value: TOrphan); virtual;
    Function DoLoadBlockChain(Operations : TPCOperationsComp; Block : Cardinal) : Boolean; virtual; abstract;
    Function DoSaveBlockChain(Operations : TPCOperationsComp) : Boolean; virtual; abstract;
    Function DoMoveBlockChain(StartBlock : Cardinal; Const DestOrphan : TOrphan) : Boolean; virtual; abstract;
    Function DoSaveBank : Boolean; virtual; abstract;
    Function DoRestoreBank(max_block : Int64) : Boolean; virtual; abstract;
    Procedure DoDeleteBlockChainBlocks(StartingDeleteBlock : Cardinal; Orphan : TOrphan); virtual; abstract;
    Function BlockExists(Block : Cardinal) : Boolean; virtual; abstract;
  public
    Function LoadBlockChainBlock(Operations : TPCOperationsComp; Block : Cardinal) : Boolean;
    Function SaveBlockChainBlock(Operations : TPCOperationsComp) : Boolean;
    Function MoveBlockChainBlocks(StartBlock : Cardinal; Const DestOrphan : TOrphan) : Boolean;
    Procedure DeleteBlockChainBlocks(StartingDeleteBlock : Cardinal; Orphan : TOrphan);
    Function SaveBank : Boolean;
    Function RestoreBank(max_block : Int64) : Boolean;
    Constructor Create(AOwner : TComponent); Override;
    Property Orphan : TOrphan read FOrphan write SetOrphan;
    Property Bank : TPCBank read FBank write SetBank;
    Procedure CopyConfiguration(Const CopyFrom : TStorage); virtual;
  End;

  TStorageClass = Class of TStorage;

  TPCBank = Class(TComponent)
  private
    FStorage : TStorage;
    FSafeBox: TPCSafeBox;
    FLastOperationBlock: TOperationBlock;
    FInitialSafeBoxHash: TRawBytes;
    FActualTargetHash: TRawBytes;
    FIsRestoringFromFile: Boolean;
    FOnLog: TPCBankLog;
    FBankLock: TRTLCriticalSection;
    FNotifyList : TList;
    FStorageClass: TStorageClass;
    function GetStorage: TStorage;
    procedure SetStorageClass(const Value: TStorageClass);
  protected
  public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Function BlocksCount: Cardinal;
    Function AccountsCount : Cardinal;
    procedure AssignTo(Dest: TPersistent); Override;
    Class Function GetRewardForNewLine(line_index: Cardinal): UInt64;
    Class Function TargetToCompact(target: TRawBytes): Cardinal;
    Class Function TargetFromCompact(encoded: Cardinal): TRawBytes;
    Class Function GetNewTarget(vteorical, vreal: Cardinal; Const actualTarget: TRawBytes): TRawBytes;
    Function GetActualCompactTargetHash: Cardinal;
    function GetActualTargetHash: AnsiString;
    function GetActualTargetSecondsAverage(BackBlocks : Cardinal): Real;
    function LoadFromStream(Stream : TStream; var errors : AnsiString) : Boolean;
    Procedure SaveToStream(Stream : TStream);
    Procedure Clear;
    Function LoadOperations(Operations : TPCOperationsComp; Block : Cardinal) : Boolean;
    Property SafeBox : TPCSafeBox read FSafeBox;
    Function AddNewBlockChainBlock(Operations: TPCOperationsComp; var newBlock: TBlockAccount; var errors: AnsiString): Boolean;
    Procedure DiskRestoreFromOperations(max_block : Int64);
    Procedure NewLog(Operations: TPCOperationsComp; Logtype: TLogType; Logtxt: AnsiString);
    Property OnLog: TPCBankLog read FOnLog write FOnLog;
    Property LastOperationBlock : TOperationBlock read FLastOperationBlock;
    Property Storage : TStorage read GetStorage;
    Property StorageClass : TStorageClass read FStorageClass write SetStorageClass;
    Function IsReady(Var CurrentProcess : AnsiString) : Boolean;
  End;

Const
  CT_OperationBlock_NUL : TOperationBlock = (block:0;account_key:(EC_OpenSSL_NID:0;x:'';y:'');reward:0;fee:0;protocol_version:0;
    protocol_available:0;timestamp:0;compact_target:0;nonce:0;block_payload:'';initial_safe_box_hash:'';operations_hash:'';proof_of_work:'');

implementation

uses
  Messages, SysUtils, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  UTime, UConst, UThread;

{ TPCBank }

function TPCBank.AccountsCount: Cardinal;
begin
  Result := FSafeBox.AccountsCount;
end;

function TPCBank.AddNewBlockChainBlock(Operations: TPCOperationsComp; var newBlock: TBlockAccount; var errors: AnsiString): Boolean;
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
      // Check valid data
      if (BlocksCount <> Operations.OperationBlock.block) then begin
        errors := 'block ('+inttostr(Operations.OperationBlock.block)+') is not new position ('+inttostr(BlocksCount)+')';
        exit;
      end;
      if (SafeBox.TotalBalance<>(Operations.FSafeBoxTransaction.TotalBalance+Operations.FSafeBoxTransaction.TotalFee)) then begin
        errors := Format('Invalid integrity balance at SafeBox. Actual Balance:%d  New Balance:(%d + fee %d = %d)',
          [SafeBox.TotalBalance,
            Operations.FSafeBoxTransaction.TotalBalance,
            Operations.FSafeBoxTransaction.TotalFee,
            Operations.FSafeBoxTransaction.TotalBalance+Operations.FSafeBoxTransaction.TotalFee]);
        exit;
      end;
      if (Operations.OperationBlock.block > 0) then begin
        if ((Operations.OperationBlock.timestamp) < (FLastOperationBlock.timestamp)) then begin
          errors := 'Invalid timestamp (New timestamp:'+inttostr(Operations.OperationBlock.timestamp)+' last timestamp ('+Inttostr(SafeBox.BlocksCount-1)+'):'+Inttostr(FLastOperationBlock.timestamp)+')';
          exit;
        end;
        if (Operations.OperationBlock.timestamp > (UnivDateTimeToUnix(DateTime2UnivDateTime(now))+CT_MaxSecondsDifferenceOfNetworkNodes)) then begin
          errors := 'Invalid timestamp (Future time '+Inttostr(Operations.OperationBlock.timestamp)+'-'+inttostr(UnivDateTimeToUnix(DateTime2UnivDateTime(now)))+'='+
             inttostr(Operations.OperationBlock.timestamp-UnivDateTimeToUnix(DateTime2UnivDateTime(now)))+' > '+inttostr(CT_MaxSecondsDifferenceOfNetworkNodes)+')';
          exit;
        end;
      end else begin
        // Check if valid Zero block
        if Not (AnsiSameText(TCrypto.ToHexaString(Operations.OperationBlock.proof_of_work),CT_Zero_Block_Proof_of_work_in_Hexa)) then begin
          errors := 'Zero block not valid, Proof of Work invalid: '+TCrypto.ToHexaString(Operations.OperationBlock.proof_of_work)+'<>'+CT_Zero_Block_Proof_of_work_in_Hexa;
          exit;
        end;
      end;
      if (Operations.OperationBlock.compact_target <> GetActualCompactTargetHash) then begin
        errors := 'Invalid target found:'+IntToHex(Operations.OperationBlock.compact_target,8)+' actual:'+IntToHex(GetActualCompactTargetHash,8);
        exit;
      end;
      if (Operations.OperationBlock.proof_of_work > GetActualTargetHash) then begin
        errors := 'Proof of work is higher than target';
        exit;
      end;
      if (Operations.OperationBlock.initial_safe_box_hash <> SafeBox.CalcSafeBoxHash) then begin
        errors := 'BlockChain Safe box hash invalid: '+TCrypto.ToHexaString(Operations.OperationBlock.initial_safe_box_hash)+' var: '+
          TCrypto.ToHexaString(FInitialSafeBoxHash)+
          ' Calculated:'+TCrypto.ToHexaString(SafeBox.CalcSafeBoxHash);
        exit;
      end;

      // Ok, include!
      // WINNER !!!
      // Congrats!

      if Not Operations.SafeBoxTransaction.Commit(Operations.OperationBlock.account_key,
        Operations.OperationBlock.reward,
        Operations.OperationBlock.timestamp,Operations.OperationBlock.compact_target,
        Operations.OperationBlock.proof_of_work,errors) then begin
        exit;
      end;
      newBlock := SafeBox.Block(SafeBox.BlocksCount-1);

      // Initialize values
      FActualTargetHash := GetActualTargetHash;
      FInitialSafeBoxHash := SafeBox.CalcSafeBoxHash;
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
            TCrypto.ToHexaString(FInitialSafeBoxHash)]));
      // Save Operations to disk
      if Not FIsRestoringFromFile then begin
        Storage.SaveBlockChainBlock(Operations);
      end;
      Operations.Clear(true);
      Result := true;
    Finally
      if Not Result then
        NewLog(Operations, lterror, 'Invalid new block '+inttostr(Operations.OperationBlock.block)+': ' + errors);
    End;
  Finally
    LeaveCriticalSection(FBankLock);
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
  d.FInitialSafeBoxHash := FInitialSafeBoxHash;
  d.FLastOperationBlock := FLastOperationBlock;
  d.FActualTargetHash := FActualTargetHash;
  d.FIsRestoringFromFile := FIsRestoringFromFile;
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
  FInitialSafeBoxHash := TCrypto.DoSha256(CT_Genesis_Magic_String_For_Old_Block_Hash); // Genesis hash
  FActualTargetHash := TargetFromCompact(CT_MinCompactTarget);
  NewLog(Nil, ltupdate, 'Clear Bank');
end;

constructor TPCBank.Create(AOwner: TComponent);
begin
  inherited;
  FStorage := Nil;
  FStorageClass := Nil;
  InitializeCriticalSection(FBankLock);
  FIsRestoringFromFile := False;
  FOnLog := Nil;
  FSafeBox := TPCSafeBox.Create;
  FNotifyList := TList.Create;
  Clear;
end;

destructor TPCBank.Destroy;
var step : String;
begin
  Try
    step := 'Deleting critical section';
    DeleteCriticalSection(FBankLock);
    step := 'Clear';
    Clear;
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
begin
  if FIsRestoringFromFile then begin
    TLog.NewLog(lterror,Classname,'Is Restoring!!!');
    raise Exception.Create('Is restoring!');
  end;
  TPCThread.ProtectEnterCriticalSection(Self,FBankLock);
  try
    FIsRestoringFromFile := true;
    try
      Clear;
      Storage.RestoreBank(max_block);
      NewLog(Nil, ltinfo,'Start restoring from disk operations (Max '+inttostr(max_block)+') Orphan: ' +Storage.Orphan);
      Operations := TPCOperationsComp.Create(Self);
      try
        while ((BlocksCount<=max_block)) do begin
          if Storage.BlockExists(BlocksCount) then begin
            if Storage.LoadBlockChainBlock(Operations,BlocksCount) then begin
                  if Not AddNewBlockChainBlock(Operations,newBlock,errors) then begin
                    NewLog(Operations, lterror,'Error restoring block: ' + Inttostr(BlocksCount)+ ' Errors: ' + errors);
                    Storage.DeleteBlockChainBlocks(BlocksCount,Storage.Orphan);
                    break;
                  end else Storage.SaveBank;
            end else break;
          end else break;
        end;
      finally
        Operations.Free;
      end;
      NewLog(Nil, ltinfo,'End restoring from disk operations (Max '+inttostr(max_block)+') Orphan: ' + Storage.Orphan);
    finally
      FIsRestoringFromFile := False;
    end;
  finally
    LeaveCriticalSection(FBankLock);
  end;
end;

function TPCBank.GetActualCompactTargetHash: Cardinal;
begin
  Result := TargetToCompact(GetActualTargetHash);
end;

function TPCBank.GetActualTargetHash: AnsiString;
  { Target is calculated in each block with avg obtained in previous
    CT_CalcNewDifficulty blocks.
    If Block is lower than CT_CalcNewDifficulty then is calculated
    with all previous blocks.
  }
Var ts1, ts2, tsTeorical, tsReal: Int64;
  CalcBack : Integer;
begin
  if (BlocksCount <= 1) then begin
    // Important: CT_MinCompactTarget is applied for blocks 0 until ((CT_CalcNewDifficulty*2)-1)
    FActualTargetHash := TargetFromCompact(CT_MinCompactTarget);
  end else begin
    if BlocksCount > CT_CalcNewTargetBlocksAverage then CalcBack := CT_CalcNewTargetBlocksAverage
    else CalcBack := BlocksCount-1;
    // Calc new target!
    ts1 := SafeBox.Block(BlocksCount-1).timestamp;
    ts2 := SafeBox.Block(BlocksCount-CalcBack-1).timestamp;
    tsTeorical := (CalcBack * CT_NewLineSecondsAvg);
    tsReal := (ts1 - ts2);
    FActualTargetHash := GetNewTarget(tsTeorical, tsReal,TargetFromCompact(FLastOperationBlock.compact_target));
  end;
  Result := FActualTargetHash;

  exit;

  if (BlocksCount <= CT_CalcNewTargetBlocksAverage) then begin
    // Important: CT_MinCompactTarget is applied for blocks 0 until ((CT_CalcNewDifficulty*2)-1)
    FActualTargetHash := TargetFromCompact(CT_MinCompactTarget);
  end else begin
    if (BlocksCount MOD CT_CalcNewTargetBlocksAverage) = 0 then begin
      // Calc new target!
      ts1 := SafeBox.Block(BlocksCount-1).timestamp;
      ts2 := SafeBox.Block(BlocksCount-CT_CalcNewTargetBlocksAverage-1).timestamp;
      tsTeorical := (CT_CalcNewTargetBlocksAverage * CT_NewLineSecondsAvg);
      tsReal := (ts1 - ts2);
      FActualTargetHash := GetNewTarget(tsTeorical, tsReal,TargetFromCompact(FLastOperationBlock.compact_target));
    end;
  end;
  Result := FActualTargetHash;
end;

function TPCBank.GetActualTargetSecondsAverage(BackBlocks: Cardinal): Real;
Var ts1, ts2: Int64;
begin
  if BlocksCount>BackBlocks then begin
    ts1 := SafeBox.Block(BlocksCount-1).timestamp;
    ts2 := SafeBox.Block(BlocksCount-BackBlocks-1).timestamp;
  end else if (BlocksCount>1) then begin
    ts1 := SafeBox.Block(BlocksCount-1).timestamp;
    ts2 := SafeBox.Block(0).timestamp;
    BackBlocks := BlocksCount-1;
  end else begin
    Result := 0;
    exit;
  end;
  Result := (ts1 - ts2) / BackBlocks;
end;

class function TPCBank.GetNewTarget(vteorical, vreal: Cardinal; Const actualTarget: TRawBytes): TRawBytes;
Var
  bnact, bnaux, bnmindiff, bnremainder, bn: TBigNum;
  ts1, ts2: Cardinal;
  tsTeorical, tsReal, factor1000, factor1000Min, factor1000Max: Int64;
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
  factor1000 := (((tsTeorical - tsReal) * 1000) DIV (tsTeorical)) * (-1);

  { Important: Note that a -500 is the same that divide by 2 (-100%), and
    1000 is the same that multiply by 2 (+100%), so we limit increase
    in a limit [-500..+1000] for a complete (CT_CalcNewTargetBlocksAverage DIV 2) round }
  if CT_CalcNewTargetBlocksAverage>1 then begin
    factor1000Min := (-500) DIV (CT_CalcNewTargetBlocksAverage DIV 2);
    factor1000Max := (1000) DIV (CT_CalcNewTargetBlocksAverage DIV 2);
  end else begin
    factor1000Min := (-500);
    factor1000Max := (1000);
  end;

  if factor1000 < factor1000Min then factor1000 := factor1000Min
  else if factor1000 > factor1000Max then factor1000 := factor1000Max
  else if factor1000=0 then begin
    Result := actualTarget;
    exit;
  end;

  // Calc new target by increasing factor (-500 <= x <= 1000)
  bn := TBigNum.Create(factor1000);
  bnact := TBigNum.Create(0);
  try
    bnact.RawValue := actualTarget;
    bnaux := bnact.Copy;
    try
      bnact.Multiply(factor1000).Divide(1000).Add(bnaux);
    finally
      bnaux.Free;
    end;
    // Adjust to TargetCompact limitations:
    Result := TargetFromCompact(TargetToCompact(bnact.RawValue));
  finally
    bn.Free;
    bnact.Free;
  end;
end;

class function TPCBank.GetRewardForNewLine(line_index: Cardinal): UInt64;
Var n, i : Cardinal;
begin
  n := (line_index + 1) DIV CT_NewLineRewardDecrease;
  Result := CT_FirstReward;
  for i := 1 to n do begin
    Result := Result DIV 2;
  end;
  if (Result < CT_MinReward) then
    Result := CT_MinReward;
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
  if FIsRestoringFromFile then CurrentProcess := 'Is restoring from file...'
  else Result := true;
end;

function TPCBank.LoadFromStream(Stream: TStream; var errors : AnsiString) : Boolean;
Var LastReadBlock : TBlockAccount;
  op : TPCOperationsComp;
  i : Integer;
begin
  Clear;
  Result := SafeBox.LoadFromStream(Stream,LastReadBlock,errors);
  if Result then begin
    TPCThread.ProtectEnterCriticalSection(Self,FBankLock);
    try
      op := TPCOperationsComp.Create(Self);
      try
        if Not LoadOperations(op,BlocksCount-1) then begin
          errors := 'Cannot read operations of block '+inttostr(BlocksCount-1);
          Result := false;
          Clear;
          exit;
        end;
        FLastOperationBlock := op.OperationBlock;
      finally
        op.Free;
      end;
      FInitialSafeBoxHash := SafeBox.CalcSafeBoxHash;
      if (BlocksCount>0) then FActualTargetHash := TargetFromCompact( FLastOperationBlock.compact_target );
      // Initialize new target hash:
      FActualTargetHash := GetActualTargetHash;
    finally
      LeaveCriticalSection(FBankLock);
    end;
    for i := 0 to FNotifyList.Count - 1 do begin
      TPCBankNotify(FNotifyList.Items[i]).NotifyNewBlock;
    end;
  end;
end;


function TPCBank.LoadOperations(Operations: TPCOperationsComp; Block: Cardinal): Boolean;
begin
  TPCThread.ProtectEnterCriticalSection(Self,FBankLock);
  try
    Result := Storage.LoadBlockChainBlock(Operations,Block);
  finally
    LeaveCriticalSection(FBankLock);
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

procedure TPCBank.SaveToStream(Stream: TStream);
begin
  SafeBox.SaveToStream(Stream);
end;

procedure TPCBank.SetStorageClass(const Value: TStorageClass);
begin
  if FStorageClass=Value then exit;
  FStorageClass := Value;
  if Assigned(FStorage) then FreeAndNil(FStorage);
end;

class function TPCBank.TargetFromCompact(encoded: Cardinal): TRawBytes;
Var
  nbits, high, offset, i: Cardinal;
  bn: TBigNum;
  s1,s2 : String;
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
  i := CT_MinCompactTarget shr 24;
  if nbits < i then
    nbits := i; // min nbits
  if nbits > 231 then
    nbits := 231; // max nbits

  offset := (encoded shl 8) shr 8;
  // Make a XOR at offset and put a "1" on the left
  offset := ((offset XOR $00FFFFFF) OR ($01000000));

  bn := TBigNum.Create(offset);
  bn.LShift(256 - nbits - 25);
  Result := bn.RawValue;
  Result := StringOfChar(#0, 32 - Length(Result)) + Result;
  if length(Result)<>32 then begin
    raise Exception.Create('TargetFromCompact result length<>32 '+inttostr(Length(Result)));
  end;
  bn.Free;
end;

class function TPCBank.TargetToCompact(target: TRawBytes): Cardinal;
Var
  bn, bn2: TBigNum;
  i: Int64;
  nbits: Cardinal;
  c: AnsiChar;
begin
  { See instructions in explanation of TargetFromCompact }
  Result := 0;
  if length(target)>32 then begin
    raise Exception.Create('Invalid target to compact: '+TCrypto.ToHexaString(target)+' ('+inttostr(length(target))+')');
  end;
  target := StringOfChar(#0, 32 - Length(target)) + target;
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
    i := CT_MinCompactTarget shr 24;
    if (nbits < i) then
    begin
      Result := CT_MinCompactTarget;
      exit;
    end;
    bn.RShift((256 - 25) - nbits);
    Result := (nbits shl 24) + ((bn.value AND $00FFFFFF) XOR $00FFFFFF);
  finally
    bn.Free;
    bn2.Free;
  end;
end;

{ TPCOperationsComp }

var
  _OperationsClass: Array of TPCOperationClass;

Function TPCOperationsComp.AddOperation(Execute : Boolean; op: TPCOperation; var errors: AnsiString): Boolean;
Begin
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
    Calc_Digest_Operations;
  end;
End;


function TPCOperationsComp.AddOperations(operations: TOperationsHashTree; var errors: AnsiString): Integer;
Var i : Integer;
  e : AnsiString;
begin
  Result := 0;
  errors := '';
  if operations=FOperationsHashTree then exit;
  for i := 0 to operations.OperationsCount - 1 do begin
    if not AddOperation(true,operations.GetOperation(i),e) then begin
      if (errors<>'') then errors := errors+' ';
      errors := errors + 'Op'+inttostr(i+1)+'/'+inttostr(operations.OperationsCount)+':'+e;
    end else inc(Result);
  end;
end;

function TPCOperationsComp.CalcProofOfWork(fullcalculation : Boolean): TRawBytes;
Var ms : TMemoryStream;
begin
  if fullcalculation then begin
    Calc_Digest_Basic;
    Calc_Digest_Operations;
  end;
  ms := TMemoryStream.Create;
  try
    ms.WriteBuffer(FDigest_Basic[1],length(FDigest_Basic));
    ms.WriteBuffer(FDigest_Operations[1],length(FDigest_Operations));
    ms.Write(FOperationBlock.timestamp,4);
    ms.Write(FOperationBlock.nonce,4);
    Result := TCrypto.DoDoubleSha256(ms.Memory,ms.Size);
  finally
    ms.Free;
  end;
end;

procedure TPCOperationsComp.Calc_Digest_Basic;
var ms : TMemoryStream;
  s : AnsiString;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(FOperationBlock.block,Sizeof(FOperationBlock.block)); // Little endian
    s := TAccountComp.AccountKey2RawString(FOperationBlock.account_key);
    ms.WriteBuffer(s[1],length(s));
    ms.Write(FOperationBlock.reward,Sizeof(FOperationBlock.reward)); // Little endian
    ms.Write(FOperationBlock.protocol_version,Sizeof(FOperationBlock.protocol_version)); // Little endian
    ms.Write(FOperationBlock.protocol_available,Sizeof(FOperationBlock.protocol_available)); // Little endian
    ms.Write(FOperationBlock.compact_target,Sizeof(FOperationBlock.compact_target)); // Little endian
    ms.WriteBuffer(FOperationBlock.block_payload[1],length(FOperationBlock.block_payload));
    ms.WriteBuffer(FOperationBlock.initial_safe_box_hash[1],length(FOperationBlock.initial_safe_box_hash));
    SetLength(FDigest_Basic,ms.Size);
    ms.Position :=0;
    ms.ReadBuffer(FDigest_Basic[1],ms.Size);
  finally
    ms.Free;
  end;
end;

procedure TPCOperationsComp.Calc_Digest_Operations;
var ms : TMemoryStream;
  i: Cardinal;
  buff: AnsiString;
begin
  ms := TMemoryStream.Create;
  try
    ms.WriteBuffer(FOperationsHashTree.HashTree[1],length(FOperationsHashTree.HashTree));
    ms.Write(FOperationBlock.fee,4);
    SetLength(FDigest_Operations,ms.Size);
    ms.Position := 0;
    ms.ReadBuffer(FDigest_Operations[1],ms.Size);
  finally
    ms.Free;
  end;
end;

procedure TPCOperationsComp.Clear(DeleteOperations : Boolean);
begin
  Try
    if DeleteOperations then begin
      FOperationsHashTree.ClearHastThree;
      if Assigned(FSafeBoxTransaction) then
        FSafeBoxTransaction.CleanTransaction;
    end;
    FOperationBlock.timestamp := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
    if Assigned(FBank) then begin
      FOperationBlock.block := bank.BlocksCount;
      FOperationBlock.reward := TPCBank.GetRewardForNewLine(bank.BlocksCount);
      FOperationBlock.compact_target := bank.GetActualCompactTargetHash;
      FOperationBlock.initial_safe_box_hash := bank.FInitialSafeBoxHash;
      If Bank.LastOperationBlock.timestamp>FOperationBlock.timestamp then
        FOperationBlock.timestamp := Bank.LastOperationBlock.timestamp;
    end else begin
      FOperationBlock.block := 0;
      FOperationBlock.reward := TPCBank.GetRewardForNewLine(0);
      FOperationBlock.compact_target := CT_MinCompactTarget;
      FOperationBlock.initial_safe_box_hash := TCrypto.DoSha256(CT_Genesis_Magic_String_For_Old_Block_Hash); // Nothing for first line
    end;
    FOperationBlock.operations_hash := FOperationsHashTree.HashTree;
    FOperationBlock.fee := 0;
    FOperationBlock.nonce := 0;

    FOperationBlock.proof_of_work := '';
    FOperationBlock.protocol_version := CT_Protocol_Version;
    FOperationBlock.protocol_available := CT_Protocol_Available;
    FIsOnlyOperationBlock := false;
  Finally
    FOperationBlock.proof_of_work := CalcProofOfWork(true);
  End;
end;

function TPCOperationsComp.CopyFromAndValidate(Operations: TPCOperationsComp; var errors: AnsiString): Boolean;
Var i : Integer;
  e : AnsiString;
  op : TPCOperation;
begin
  errors := '';
  if Self=Operations then begin
    result := true;
    exit;
  end else Result := false;
  Clear(true);
  if Operations.IsOnlyOperationBlock then begin
    errors := 'Operations is only an operation block';
    exit;
  end;
  FOperationBlock := Operations.OperationBlock;
  for i := 0 to Operations.OperationsHashTree.OperationsCount-1 do begin
    op := Operations.OperationsHashTree.GetOperation(i);
    if Not op.DoOperation(SafeBoxTransaction,e) then begin
      errors := 'Error executing operation '+inttostr(i+1)+'/'+Inttostr(Operations.OperationsHashTree.OperationsCount)+':'+e;
      exit;
    end;
    FOperationsHashTree.AddOperationToHashTree(op);
  end;
  Result := ValidateOperationBlock(errors);
end;

procedure TPCOperationsComp.CopyFromExceptAddressKey(Operations: TPCOperationsComp);
var lastopb : TOperationBlock;
begin
  if Self=Operations then exit;
  lastopb := FOperationBlock;
  FOperationBlock := Operations.FOperationBlock;
  FOperationBlock.account_key := lastopb.account_key; // Except AddressKey
  FOperationBlock.compact_target := Bank.GetActualCompactTargetHash;
  FIsOnlyOperationBlock := Operations.FIsOnlyOperationBlock;
  FOperationsHashTree.CopyFromHashTree(Operations.FOperationsHashTree);
  FOperationBlock.operations_hash := FOperationsHashTree.HashTree;
  FSafeBoxTransaction.CopyFrom(Operations.FSafeBoxTransaction);
  // Recalc all
  FOperationBlock.proof_of_work := CalcProofOfWork(true);
end;

function TPCOperationsComp.Count: Integer;
begin
  Result := FOperationsHashTree.OperationsCount;
end;

constructor TPCOperationsComp.Create(AOwner: TComponent);
begin
  inherited;
  FOperationsHashTree := TOperationsHashTree.Create;
  FBank := Nil;
  FOperationBlock := GetFirstBlock;
  FSafeBoxTransaction := Nil;
  if Assigned(AOwner) And (AOwner is TPCBank) then begin
    Bank := TPCBank(AOwner);
  end else Clear(true);
end;

destructor TPCOperationsComp.Destroy;
begin
  Clear(true);
  FOperationsHashTree.Free;
  FSafeBoxTransaction.Free;
  inherited;
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

function TPCOperationsComp.IncrementNOnce: Boolean;
begin
  nonce := nonce + 1;
  Result := FOperationBlock.proof_of_work < FBank.FActualTargetHash;
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

function TPCOperationsComp.LoadFromStream(ExecuteOperations : Boolean; load_header : Boolean; Stream: TStream; var errors: AnsiString): Boolean;
Var
  c, i, lastfee: Cardinal;
  soob : Byte;
  OpType: Cardinal;
  bcop: TPCOperation;
  pow,
  m: AnsiString;
  j: Integer;
  OpClass: TPCOperationClass;
  errors2 : AnsiString;
begin
  Clear(true);
  Result := False;
  if (load_header) then begin
    // Header: Magic string + protocol info
    if TStreamOp.ReadAnsiString(Stream, m) < 0 then begin
      errors := 'Invalid header';
      exit;
    end;
    if (Not AnsiSameStr(CT_MagicIdentificator, m)) then begin
      errors := 'Invalid header name';
      exit;
    end;
    errors := 'Invalid header structure';
    if (Stream.Size - Stream.Position < 4) then exit;
    Stream.Read(FOperationBlock.protocol_version, Sizeof(FOperationBlock.protocol_version));
    Stream.Read(FOperationBlock.protocol_available, Sizeof(FOperationBlock.protocol_available));
    if (FOperationBlock.protocol_version<>CT_Protocol_Version) then begin
      errors := 'Invalid protocol block: '+inttostr(FOperationBlock.protocol_version);
      exit;
    end;
  end;
  //
  errors := 'Invalid structure';
  if (Stream.Size - Stream.Position < 5) then exit;
  Stream.Read(soob,1);
  if soob=0 then FIsOnlyOperationBlock:=false
  else if soob=1 then FIsOnlyOperationBlock:=true
  else exit; // Invalid value

  Stream.Read(FOperationBlock.block, Sizeof(FOperationBlock.block));

  if TStreamOp.ReadAnsiString(Stream, m) < 0 then exit;
  FOperationBlock.account_key := TAccountComp.RawString2Accountkey(m); // String2Addresskey(m);
  if Stream.Read(FOperationBlock.reward, Sizeof(FOperationBlock.reward)) < 0 then exit;
  if Stream.Read(FOperationBlock.fee, Sizeof(FOperationBlock.fee)) < 0 then exit;
  if Stream.Read(FOperationBlock.timestamp, Sizeof(FOperationBlock.timestamp)) < 0 then exit;
  if Stream.Read(FOperationBlock.compact_target, Sizeof(FOperationBlock.compact_target)) < 0 then exit;
  if Stream.Read(FOperationBlock.nonce, Sizeof(FOperationBlock.nonce)) < 0 then exit;
  if TStreamOp.ReadAnsiString(Stream, FOperationBlock.block_payload) < 0 then exit;
  if TStreamOp.ReadAnsiString(Stream, FOperationBlock.initial_safe_box_hash) < 0 then exit;
  if TStreamOp.ReadAnsiString(Stream, FOperationBlock.operations_hash) < 0 then exit;
  if TStreamOp.ReadAnsiString(Stream, FOperationBlock.proof_of_work) < 0 then exit;
  pow := OperationBlock.proof_of_work;
  If FIsOnlyOperationBlock then begin
    Result := true;
    exit;
  end;
  // Fee will be calculated for each operation. Set it to 0 and check later for integrity
  lastfee := OperationBlock.fee;
  FOperationBlock.fee := 0;
  Stream.Read(c, 4);
  // c = operations count
  for i := 1 to c do begin
    bcop := Nil;
    try
      errors := 'Invalid operation structure ' + inttostr(i) + '/' + inttostr(c);
      if Stream.Size - Stream.Position < 4 then exit;
      Stream.Read(OpType, 4);
      j := IndexOfOperationClassByOpType(OpType);
      if j >= 0 then
        OpClass := _OperationsClass[j]
      else
        OpClass := Nil;
      if Not Assigned(OpClass) then begin
        errors := errors + ' optype not valid:' + InttoHex(OpType, 4);
        exit;
      end;
      errors := errors + ' Operation:'+OpClass.ClassName;
      bcop := OpClass.Create;
      if not bcop.LoadFromStream(Stream) then begin
        bcop.Free;
        exit;
      end;
      if Not AddOperation(ExecuteOperations,bcop, errors2) then begin
        errors := errors + ' '+errors2+' '+bcop.ToString;
        bcop.Free;
        exit;
      end;
    Except
      FreeAndNil(bcop);
      raise ;
    end;
  end;
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
end;

procedure TPCOperationsComp.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if AComponent = FBank then begin
      FBank := Nil;
      FSafeBoxTransaction.Free;
      FSafeBoxTransaction := Nil;
    end;
  end;
end;

class function TPCOperationsComp.OperationBlockToText(OperationBlock: TOperationBlock): AnsiString;
begin
  Result := Format('Block:%d Timestamp:%d Reward:%d Fee:%d PoW:%s',[operationBlock.block,
    operationblock.timestamp,operationblock.reward,operationblock.fee, TCrypto.ToHexaString(operationblock.proof_of_work)]);
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
  aux : TOperationsHashTree;
begin
  Try
    FOperationBlock.timestamp := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
    if Assigned(FBank) then begin
      FOperationBlock.block := bank.BlocksCount;
      FOperationBlock.reward := TPCBank.GetRewardForNewLine(bank.BlocksCount);
      FOperationBlock.compact_target := bank.GetActualCompactTargetHash;
      FOperationBlock.initial_safe_box_hash := bank.FInitialSafeBoxHash;
      If Bank.LastOperationBlock.timestamp>FOperationBlock.timestamp then
        FOperationBlock.timestamp := Bank.LastOperationBlock.timestamp;
    end else begin
      FOperationBlock.block := 0;
      FOperationBlock.reward := TPCBank.GetRewardForNewLine(0);
      FOperationBlock.compact_target := CT_MinCompactTarget;
      FOperationBlock.initial_safe_box_hash := TCrypto.DoSha256(CT_Genesis_Magic_String_For_Old_Block_Hash);
    end;
    FOperationBlock.proof_of_work := '';
    FOperationBlock.protocol_version := CT_Protocol_Version;
    FOperationBlock.protocol_available := CT_Protocol_Available;
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
      FOperationBlock.operations_hash := FOperationsHashTree.HashTree;
    Finally
      FOperationsHashTree.Free;
      FOperationsHashTree := aux;
    End;
  Finally
    FOperationBlock.proof_of_work := CalcProofOfWork(true);
  End;
  if (n>0) then begin
    TLog.NewLog(ltdebug,Classname,Format('Sanitize operations (before %d - after %d)',[lastn,n]));
  end;
end;

function TPCOperationsComp.SaveToStream(save_header, save_only_OperationBlock : Boolean; Stream: TStream): Boolean;
Var
  c, opl, i, OpType: Cardinal;
  soob : Byte;
  bcop: TPCOperation;
  bcops, errors: AnsiString;
begin
  if save_header then begin
    // Header: Magic string + protocol info
    TStreamOp.WriteAnsiString(Stream, CT_MagicIdentificator);
    Stream.Write(CT_Protocol_Version, Sizeof(FOperationBlock.protocol_version));
    Stream.Write(CT_Protocol_Available, Sizeof(FOperationBlock.protocol_available));
  end;
  //
  if save_only_OperationBlock then soob := 1
  else soob := 0;
  Stream.Write(soob,1);
  //
  Stream.Write(FOperationBlock.block, Sizeof(FOperationBlock.block));
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
  if (Not save_only_OperationBlock) then begin
    c := Count;
    Stream.Write(c, 4);
    // c = operations count
    for i := 1 to c do
    begin
      bcop := Operation[i - 1];
      OpType := bcop.OpType;
      Stream.write(OpType, 4);
      bcop.SaveToStream(Stream);
    end;
  end;
  Result := true;
end;

procedure TPCOperationsComp.SetAccountKey(const value: TAccountKey);
begin
  if TAccountComp.AccountKey2RawString(value)=TAccountComp.AccountKey2RawString(FOperationBlock.account_key) then exit;
  FOperationBlock.account_key := value;
  Calc_Digest_Basic;
end;

procedure TPCOperationsComp.SetBank(const value: TPCBank);
begin
  if FBank = value then exit;
  if Assigned(FBank) then begin
     FSafeBoxTransaction.Free;
     FSafeBoxTransaction := Nil;
  end;
  FBank := value;
  if Assigned(value) then begin
    value.FreeNotification(Self);
    FSafeBoxTransaction := TPCSafeBoxTransaction.Create(FBank.SafeBox);
  end;
  Clear(true);
end;

procedure TPCOperationsComp.SetBlockPayload(const Value: TRawBytes);
begin
  if Value=FOperationBlock.block_payload then exit;
  if Length(Value)>CT_MaxPayloadSize then Exit;
  FOperationBlock.block_payload := Value;
  FOperationBlock.proof_of_work := CalcProofOfWork(true);
end;

procedure TPCOperationsComp.SetnOnce(const value: Cardinal);
begin
  FOperationBlock.nonce := value;
  FOperationBlock.proof_of_work := CalcProofOfWork(false);
end;

procedure TPCOperationsComp.Settimestamp(const value: Cardinal);
begin
  if FOperationBlock.timestamp=Value then exit; // No change, nothing to do
  FOperationBlock.timestamp := value;
  FOperationBlock.proof_of_work := CalcProofOfWork(false);
end;

procedure TPCOperationsComp.UpdateTimestamp;
Var ts : Cardinal;
begin
  ts := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
  if Assigned(bank) then begin
    If Bank.FLastOperationBlock.timestamp>ts then ts := Bank.FLastOperationBlock.timestamp;
  end;
  timestamp := ts;
end;

function TPCOperationsComp.ValidateOperationBlock(var errors : AnsiString): Boolean;
Var lastpow : AnsiString;
  i : Integer;
begin
  Result := false;
  lastpow := OperationBlock.proof_of_work;
  // Execute SafeBoxTransaction operations:
  SafeBoxTransaction.Rollback;
  for i := 0 to Count - 1 do begin
    If Not Operation[i].DoOperation(SafeBoxTransaction,errors) then begin
      errors := 'Error executing operation '+inttostr(i+1)+'/'+inttostr(Count)+': '+errors;
      exit;
    end;
  end;
  FOperationBlock.proof_of_work := CalcProofOfWork(true);
  if Not AnsiSameStr(OperationBlock.proof_of_work,lastpow) then begin
    errors := 'Invalid Proof of work calculation';
    exit;
  end;
  if FOperationsHashTree.HashTree<>OperationBlock.operations_hash then begin
    errors := 'Invalid Operations Hash '+TCrypto.ToHexaString(OperationBlock.operations_hash)+'<>'+TCrypto.ToHexaString(FOperationsHashTree.HashTree);
    exit;
  end;

  if Not TAccountComp.IsValidAccountKey(OperationBlock.account_key,errors) then begin
    exit;
  end;
  if (OperationBlock.reward<>TPCBank.GetRewardForNewLine(OperationBlock.block)) then begin
    errors := 'Invalid reward';
    exit;
  end;
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
var op : TPCOperation;
  l : TList;
  i : Integer;
begin
  l := FHashTreeOperations.LockList;
  try
    Try
      for i := 0 to l.Count - 1 do begin
        op := l[i];
        op.Free;
      end;
    Finally
      l.Clear;
      FHashTree := TCrypto.DoSha256('');
    End;
  finally
    FHashTreeOperations.UnlockList;
  end;
end;

procedure TOperationsHashTree.CopyFromHashTree(Sender: TOperationsHashTree);
Var i : Integer;
  lme, lsender : TList;
  opsender,op : TPCOperation;
  ms : TMemoryStream;
begin
  if (Sender = Self) then begin
    exit;
  end;

  ClearHastThree;
  lme := FHashTreeOperations.LockList;
  lsender := Sender.FHashTreeOperations.LockList;
  try
    ms := TMemoryStream.Create;
    Try
      for i := 0 to lsender.Count - 1 do begin
        opsender := lsender[i];
        op := TPCOperation(opsender.NewInstance);
        ms.Size:=0;
        opsender.SaveToStream(ms);
        ms.Position:=0;
        op.LoadFromStream(ms);
        InternalAddOperationToHashTree(lme,op);
      end;
    Finally
      ms.Free;
    End;
  finally
    FHashTreeOperations.UnlockList;
    Sender.FHashTreeOperations.UnlockList;
  end;
end;

constructor TOperationsHashTree.Create;
begin
  FHashTree := TCrypto.DoSha256('');
  FHashTreeOperations := TThreadList.Create;
end;

destructor TOperationsHashTree.Destroy;
begin
  ClearHastThree;
  FHashTreeOperations.Free;
  inherited;
end;

function TOperationsHashTree.GetOperation(index: Integer): TPCOperation;
Var l : TList;
begin
  l := FHashTreeOperations.LockList;
  try
    Result := TPCOperation(l[index]);
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
        TPCOperation(l[i]).AffectedAccounts(intl);
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

procedure TOperationsHashTree.InternalAddOperationToHashTree(list: TList; op: TPCOperation);
Var ms : TMemoryStream;
  h : TRawBytes;
  newOp : TPCOperation;
begin
  ms := TMemoryStream.Create;
  try
      newOp := TPCOperation( op.NewInstance );
      op.SaveToStream(ms);
      ms.Position := 0;
      newOp.LoadFromStream(ms);
      h := TCrypto.DoSha256(ms.Memory,ms.Size);
      list.Add(newOp);
  finally
      ms.Free;
  end;
  // Include to hash tree
  FHashTree := TCrypto.DoSha256(FHashTree+h);
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

{ TStorage }

procedure TStorage.CopyConfiguration(const CopyFrom: TStorage);
begin
  Orphan := CopyFrom.Orphan;
end;

constructor TStorage.Create(AOwner: TComponent);
begin
  inherited;
  FOrphan := '';
end;

procedure TStorage.DeleteBlockChainBlocks(StartingDeleteBlock: Cardinal; Orphan: TOrphan);
begin
  DoDeleteBlockChainBlocks(StartingDeleteBlock,Orphan);
end;

function TStorage.LoadBlockChainBlock(Operations: TPCOperationsComp; Block: Cardinal): Boolean;
begin
   Result := DoLoadBlockChain(Operations,Block);
end;

function TStorage.MoveBlockChainBlocks(StartBlock: Cardinal; const DestOrphan: TOrphan): Boolean;
begin
  Result := DoMoveBlockChain(StartBlock,DestOrphan);
end;

function TStorage.RestoreBank(max_block: Int64): Boolean;
begin
  Result := DoRestoreBank(max_block);
end;

function TStorage.SaveBank: Boolean;
begin
  Result := true;
  if (Bank.BlocksCount MOD CT_BankToDiskEveryNBlocks)<>0 then exit; // No bank!
  Try
    Result := DoSaveBank;
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,Classname,'Error saving Bank: '+E.Message);
      Raise;
    end;
  End;
end;

function TStorage.SaveBlockChainblock(Operations: TPCOperationsComp): Boolean;
begin
  Try
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

{ TPCOperation }

class function TPCOperation.IsReadablePayload(const Payload: TRawBytes): Boolean;
Var i : Integer;
begin
  Result := true;
    for i := 1 to length(Payload) do begin
      if (ord(Payload[i])<32) Or (ord(Payload[i])>=127) then begin
        Result := false;
        Exit;
      end;
    end;
end;

initialization
  SetLength(_OperationsClass, 0);
finalization

end.
