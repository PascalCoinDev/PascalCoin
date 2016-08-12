unit UAccounts;

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
  Classes, UConst, Windows, UCrypto;

Type
  TAccountKey = TECDSA_Public;
  PAccountKey = ^TAccountKey;

  TAccountComp = Class
  private
  public
    Class Function IsValidAccountKey(account: TAccountKey; var errors : AnsiString): Boolean;
    Class Function GetECInfoTxt(Const EC_OpenSSL_NID: Word) : AnsiString;
    Class Procedure ValidsEC_OpenSSL_NID(list : TList);
    Class Function AccountKey2RawString(account: TAccountKey): AnsiString;
    Class Function RawString2Accountkey(rawaccstr: AnsiString): TAccountKey;
    Class Function PrivateToAccountkey(key: TECPrivateKey): TAccountKey;
    Class Function IsAccountBlockedByProtocol(account_number, blocks_count : Cardinal) : Boolean;
    Class Function Equal(account1,account2 : TAccountKey) : Boolean;
    Class Function AccountNumberToAccountTxtNumber(account_number : Cardinal) : AnsiString;
    Class Function AccountTxtNumberToAccountNumber(Const account_txt_number : AnsiString; var account_number : Cardinal) : Boolean;
    Class Function FormatMoney(Money : Int64) : AnsiString;
    Class Function TxtToMoney(Const moneytxt : AnsiString; var money : Int64) : Boolean;
    Class Function AccountKeyToExport(Const account : TAccountKey) : AnsiString;
    Class Function AccountKeyFromImport(Const HumanReadable : AnsiString; var account : TAccountKey; var errors : AnsiString) : Boolean;
    Class Function AccountPublicKeyExport(Const account : TAccountKey) : AnsiString;
    Class Function AccountPublicKeyImport(Const HumanReadable : AnsiString; var account : TAccountKey; var errors : AnsiString) : Boolean;
  End;

  TAccount = Record
    account: Cardinal;        // FIXED value. Account number
    accountkey: TAccountKey;  // Public EC
    balance: UInt64;          // Balance, allways >= 0
    updated_block: Cardinal;  // Number of block where was updated
    n_operation: Cardinal;    // count number of owner operations (when receive, this is not updated)
  End;
  PAccount = ^TAccount;

  TBlockAccount = Record
    blockaccount : Cardinal;  // FIXED. Number in the BlockChain
    accounts : Array[0..CT_AccountsPerBlock-1] of TAccount;
    timestamp: Cardinal;      // FIXED: Same value that stored in BlockChain. Included here because I need it to calculate new target value
    block_hash: AnsiString;   // Calculated on every block change (on create and on accounts updated)
  end;
  PBlockAccount = ^TBlockAccount;

  TPCSafeBox = Class;

  // This is a class to quickly find accountkeys and their respective account number/s
  TOrderedAccountKeysList = Class
  Private
    FAutoAddAll : Boolean;
    FAccountList : TPCSafeBox;
    FOrderedAccountKeysList : TList; // An ordered list of pointers to quickly find account keys in account list
    Function Find(Const AccountKey: TAccountKey; var Index: Integer): Boolean;
    function GetAccountKeyList(index: Integer): TList;
    function GetAccountKey(index: Integer): TAccountKey;
  protected
    Procedure Clear(RemoveAccountList : Boolean);
  public
    Constructor Create(AccountList : TPCSafeBox; AutoAddAll : Boolean);
    Destructor Destroy; override;
    Procedure AddAccountKey(Const AccountKey : TAccountKey);
    Procedure RemoveAccountKey(Const AccountKey : TAccountKey);
    Procedure AddAccounts(Const AccountKey : TAccountKey; accounts : Array of Cardinal);
    Procedure RemoveAccounts(Const AccountKey : TAccountKey; accounts : Array of Cardinal);
    Function IndexOfAccountKey(Const AccountKey : TAccountKey) : Integer;
    Property AccountKeyList[index : Integer] : TList read GetAccountKeyList;
    Property AccountKey[index : Integer] : TAccountKey read GetAccountKey;
    Function Count : Integer;
  End;


  // SafeBox is a box that only can be updated using SafeBoxTransaction, and this
  // happens only when a new BlockChain is included. After this, a new "SafeBoxHash"
  // is created, so each SafeBox has a unique SafeBoxHash

  TPCSafeBox = Class
  private
    FBlockAccountsList : TList;
    FListOfOrderedAccountKeysList : TList;
    FBufferBlocksHash: AnsiString;
    FTotalBalance: Int64;
    FTotalFee: Int64;
    FSafeBoxHash : TRawBytes;
    FLock: TRTLCriticalSection; // Thread safe
    FIsLocked : Boolean;
    Procedure SetAccount(account_number : Cardinal; newAccountkey: TAccountKey; newBalance: UInt64; newN_operation: Cardinal);
    Procedure AccountKeyListAddAccounts(Const AccountKey : TAccountKey; accounts : Array of Cardinal);
    Procedure AccountKeyListRemoveAccount(Const AccountKey : TAccountKey; accounts : Array of Cardinal);
  protected
    Function AddNew(Const accountkey: TAccountKey; reward: UInt64; timestamp: Cardinal; compact_target: Cardinal; Const proof_of_work: AnsiString) : TBlockAccount;
  public
    Constructor Create;
    Destructor Destroy; override;
    function AccountsCount: Integer;
    Function BlocksCount : Integer;
    Procedure CopyFrom(accounts : TPCSafeBox);
    Class Function CalcBlockHash(const block : TBlockAccount):AnsiString;
    Class Function BlockAccountToText(Const block : TBlockAccount):AnsiString;
    Function LoadFromStream(Stream : TStream; var LastReadBlock : TBlockAccount; var errors : AnsiString) : Boolean;
    Procedure SaveToStream(Stream : TStream);
    Procedure Clear;
    Function Account(account_number : Cardinal) : TAccount;
    Function Block(block_number : Cardinal) : TBlockAccount;
    Function CalcSafeBoxHash : TRawBytes;
    Property TotalBalance : Int64 read FTotalBalance;
    Procedure StartThreadSafe;
    Procedure EndThreadSave;
    Property IsLocked : Boolean read FIsLocked;
  End;


  TOrderedAccountList = Class
  private
    FList : TList;
    Function Find(const account_number: Cardinal; var Index: Integer): Boolean;
  public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Clear;
    Function Add(Const account : TAccount) : Integer;
    Function Count : Integer;
    Function Get(index : Integer) : TAccount;
  End;


  TPCSafeBoxTransaction = Class
  private
    FOrderedList : TOrderedAccountList;
    FFreezedAccounts : TPCSafeBox;
    FTotalBalance: Int64;
    FTotalFee: Int64;
    FOldSafeBoxHash : TRawBytes;
    Function GetInternalAccount(account_number : Cardinal) : PAccount;
  protected
  public
    Constructor Create(SafeBox : TPCSafeBox);
    Destructor Destroy; override;
    Function TransferAmount(sender,target : Cardinal; n_operation : Cardinal; amount, fee : UInt64; var errors : AnsiString) : Boolean;
    Function UpdateAccountkey(account_number, n_operation: Cardinal; accountkey: TAccountKey; fee: UInt64; var errors : AnsiString) : Boolean;
    Function Commit(accountkey: TAccountKey; reward: UInt64; timestamp: Cardinal; compact_target: Cardinal; proof_of_work: AnsiString; var errors : AnsiString) : Boolean;
    Function Account(account_number : Cardinal) : TAccount;
    Procedure Rollback;
    Function CheckIntegrity : Boolean;
    Property FreezedSafeBox : TPCSafeBox read FFreezedAccounts;
    Property TotalFee : Int64 read FTotalFee;
    Property TotalBalance : Int64 read FTotalBalance;
    Procedure CopyFrom(transaction : TPCSafeBoxTransaction);
    Procedure CleanTransaction;
    Function ModifiedCount : Integer;
    Function Modified(index : Integer) : TAccount;
  End;

  TStreamOp = Class
  public
    class Function WriteAnsiString(Stream: TStream; value: AnsiString): Integer;
    class Function ReadAnsiString(Stream: TStream; var value: AnsiString): Integer;
  End;

Const
  CT_Account_NUL : TAccount = (account:0;accountkey:(EC_OpenSSL_NID:0;x:'';y:'');balance:0;updated_block:0;n_operation:0);
  CT_BlockAccount_NUL : TBlockAccount = (
    blockaccount:0;
    accounts:(
    (account:0;accountkey:(EC_OpenSSL_NID:0;x:'';y:'');balance:0;updated_block:0;n_operation:0),
    (account:0;accountkey:(EC_OpenSSL_NID:0;x:'';y:'');balance:0;updated_block:0;n_operation:0),
    (account:0;accountkey:(EC_OpenSSL_NID:0;x:'';y:'');balance:0;updated_block:0;n_operation:0),
    (account:0;accountkey:(EC_OpenSSL_NID:0;x:'';y:'');balance:0;updated_block:0;n_operation:0),
    (account:0;accountkey:(EC_OpenSSL_NID:0;x:'';y:'');balance:0;updated_block:0;n_operation:0)
    );
    timestamp:0;
    block_hash:'');

implementation

uses
  SysUtils, ULog, ssl_const, ssl_err, UThread;

{ TStreamOp }

class function TStreamOp.ReadAnsiString(Stream: TStream; var value: AnsiString): Integer;
Var
  l: Word;
begin
  value := '';
  Result := -1;
  if Stream.Size - Stream.Position < 2 then
    exit;
  Stream.Read(l, 2);
  if Stream.Size - Stream.Position < l then begin
    Stream.Position := Stream.Position - 2; // Go back!
    exit;
  end;
  SetLength(value, l);
  Stream.ReadBuffer(value[1], l);
  Result := l;
end;

class function TStreamOp.WriteAnsiString(Stream: TStream; value: AnsiString): Integer;
Var
  l: Word;
begin
  if (Length(value)>(256*256)) then begin
    TLog.NewLog(lterror,Classname,'Invalid stream size! '+Inttostr(Length(value)));
    raise Exception.Create('Invalid stream size! '+Inttostr(Length(value)));
  end;

  l := Length(value);
  Stream.Write(l, 2);
  if (l > 0) then
    Stream.WriteBuffer(value[1], Length(value));
  Result := l;
end;

{ TAccountComp }
Const CT_Base58 : AnsiString = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz';

class function TAccountComp.AccountKeyToExport(const account: TAccountKey): AnsiString;
Var raw : TRawBytes;
  BN, BNMod, BNDiv : TBigNum;
  i : Integer;
begin
  Result := '';
  raw := AccountKey2RawString(account);
  BN := TBigNum.Create;
  BNMod := TBigNum.Create;
  BNDiv := TBigNum.Create(Length(CT_Base58));
  try
    BN.HexaValue := '01'+TCrypto.ToHexaString( raw )+TCrypto.ToHexaString(Copy(TCrypto.DoSha256(raw),1,4));
    while (Not BN.IsZero) do begin
      BN.Divide(BNDiv,BNMod);
      If (BNMod.Value>=0) And (BNMod.Value<length(CT_Base58)) then Result := CT_Base58[Byte(BNMod.Value)+1] + Result
      else raise Exception.Create('Error converting to Base 58');
    end;
  finally
    BN.Free;
    BNMod.Free;
    BNDiv.Free;
  end;
end;


class function TAccountComp.AccountKey2RawString(account: TAccountKey): AnsiString;
Var s : TMemoryStream;
begin
  s := TMemoryStream.Create;
  try
    s.Write(account.EC_OpenSSL_NID, SizeOf(account.EC_OpenSSL_NID));
    TStreamOp.WriteAnsiString(s,account.x);
    TStreamOp.WriteAnsiString(s,account.y);
    SetLength(Result,s.Size);
    s.Position := 0;
    s.Read(Result[1],s.Size);
  finally
    s.Free;
  end;
end;

class function TAccountComp.AccountKeyFromImport(const HumanReadable: AnsiString; var account: TAccountKey; var errors : AnsiString): Boolean;
Var raw : TRawBytes;
  BN, BNAux, BNBase : TBigNum;
  i,j : Integer;
  s1,s2 : AnsiString;
  i64 : Int64;
  b : Byte;
begin
  result := false;
  errors := 'Invalid length';
  account := CT_Account_NUL.accountkey;
  if length(HumanReadable)<20 then exit;
  BN := TBigNum.Create(0);
  BNAux := TBigNum.Create;
  BNBase := TBigNum.Create(1);
  try
    for i := length(HumanReadable) downto 1 do begin
      j := pos(HumanReadable[i],CT_Base58);
      if j=0 then begin
        errors := 'Invalid char "'+HumanReadable[i]+'" at pos '+inttostr(i)+'/'+inttostr(length(HumanReadable));
        exit;
      end;
      BNAux.Value := j-1;
      BNAux.Multiply(BNBase);
      BN.Add(BNAux);
      BNBase.Multiply(length(CT_Base58));
    end;
    // Last 8 hexa chars are the checksum of others
    s1 := Copy(BN.HexaValue,3,length(BN.HexaValue));
    s2 := copy(s1,length(s1)-7,8);
    s1 := copy(s1,1,length(s1)-8);
    raw := TCrypto.HexaToRaw(s1);
    s1 := TCrypto.ToHexaString( TCrypto.DoSha256(raw) );
    if copy(s1,1,8)<>s2 then begin
      // Invalid checksum
      errors := 'Invalid checksum';
      exit;
    end;
    try
      account := TAccountComp.RawString2Accountkey(raw);
      Result := true;
      errors := '';
    except
      // Nothing to do... invalid
      errors := 'Error on conversion from Raw to Account key';
    end;
  Finally
    BN.Free;
    BNBase.Free;
    BNAux.Free;
  end;
end;

class function TAccountComp.AccountNumberToAccountTxtNumber(account_number: Cardinal): AnsiString;
Var an : int64;
begin
  an := account_number;
  an := ((((((an * 3) MOD 97) * 7) MOD 101) * 5) MOD 89)+10;
  Result := IntToStr(account_number)+'-'+Inttostr(an);
end;

class function TAccountComp.AccountPublicKeyExport(const account: TAccountKey): AnsiString;
Var raw : TRawBytes;
  BN, BNMod, BNDiv : TBigNum;
  i : Integer;
begin
  Result := '';
  raw := AccountKey2RawString(account);
  BN := TBigNum.Create;
  BNMod := TBigNum.Create;
  BNDiv := TBigNum.Create(Length(CT_Base58));
  try
    BN.HexaValue := '01'+TCrypto.ToHexaString( raw )+TCrypto.ToHexaString(Copy(TCrypto.DoSha256(raw),1,4));
    while (Not BN.IsZero) do begin
      BN.Divide(BNDiv,BNMod);
      If (BNMod.Value>=0) And (BNMod.Value<length(CT_Base58)) then Result := CT_Base58[Byte(BNMod.Value)+1] + Result
      else raise Exception.Create('Error converting to Base 58');
    end;
  finally
    BN.Free;
    BNMod.Free;
    BNDiv.Free;
  end;
end;

class function TAccountComp.AccountPublicKeyImport(
  const HumanReadable: AnsiString; var account: TAccountKey;
  var errors: AnsiString): Boolean;
Var raw : TRawBytes;
  BN, BNAux, BNBase : TBigNum;
  i,j : Integer;
  s1,s2 : AnsiString;
  i64 : Int64;
  b : Byte;
begin
  result := false;
  errors := 'Invalid length';
  account := CT_Account_NUL.accountkey;
  if length(HumanReadable)<20 then exit;
  BN := TBigNum.Create(0);
  BNAux := TBigNum.Create;
  BNBase := TBigNum.Create(1);
  try
    for i := length(HumanReadable) downto 1 do begin
      j := pos(HumanReadable[i],CT_Base58);
      if j=0 then begin
        errors := 'Invalid char "'+HumanReadable[i]+'" at pos '+inttostr(i)+'/'+inttostr(length(HumanReadable));
        exit;
      end;
      BNAux.Value := j-1;
      BNAux.Multiply(BNBase);
      BN.Add(BNAux);
      BNBase.Multiply(length(CT_Base58));
    end;
    // Last 8 hexa chars are the checksum of others
    s1 := Copy(BN.HexaValue,3,length(BN.HexaValue));
    s2 := copy(s1,length(s1)-7,8);
    s1 := copy(s1,1,length(s1)-8);
    raw := TCrypto.HexaToRaw(s1);
    s1 := TCrypto.ToHexaString( TCrypto.DoSha256(raw) );
    if copy(s1,1,8)<>s2 then begin
      // Invalid checksum
      errors := 'Invalid checksum';
      exit;
    end;
    try
      account := TAccountComp.RawString2Accountkey(raw);
      Result := true;
      errors := '';
    except
      // Nothing to do... invalid
      errors := 'Error on conversion from Raw to Account key';
    end;
  Finally
    BN.Free;
    BNBase.Free;
    BNAux.Free;
  end;
end;

class function TAccountComp.AccountTxtNumberToAccountNumber(const account_txt_number: AnsiString; var account_number: Cardinal): Boolean;
Var i : Integer;
  char1 : AnsiChar;
  char2 : AnsiChar;
  n : Int64;
begin
  Result := false;
  if length(trim(account_txt_number))=0 then exit;
  n := 0;
  for i := 1 to length(account_txt_number) do begin
    if account_txt_number[i] in ['0'..'9'] then begin
      n := (n * 10) + ord( account_txt_number[i] ) - ord('0');
    end else break;
  end;
  account_number := n;
  if i>length(account_txt_number) then begin
    result := true;
    exit;
  end;
  if (account_txt_number[i] in ['-','.',' ']) then inc(i);
  if length(account_txt_number)-1<>i then exit;
  n := StrToIntDef(copy(account_txt_number,i,length(account_txt_number)),0);
  Result := n = (((((((account_number * 3) MOD 97) * 7) MOD 101) * 5) MOD 89)+10);
end;

class function TAccountComp.Equal(account1, account2: TAccountKey): Boolean;
begin
  Result := (account1.EC_OpenSSL_NID=account2.EC_OpenSSL_NID) And
    (account1.x=account2.x) And (account1.y=account2.y);
end;

class function TAccountComp.FormatMoney(Money: Int64): AnsiString;
begin
  Result := FormatFloat('0.0000',(Money/10000));
end;

class function TAccountComp.GetECInfoTxt(const EC_OpenSSL_NID: Word): AnsiString;
begin
  case EC_OpenSSL_NID of
    NID_secp256k1 : begin
      Result := 'secp256k1';
    end;
    NID_secp384r1 : begin
      Result := 'secp384r1';
    end;
    NID_sect283k1 : Begin
      Result := 'secp283k1';
    End;
    NID_secp521r1 : begin
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

class function TAccountComp.IsValidAccountKey(account: TAccountKey; var errors : AnsiString): Boolean;
Var _a, _b: AnsiString;
begin
  errors := '';
  case account.EC_OpenSSL_NID of
    NID_secp256k1,NID_secp384r1,NID_sect283k1,NID_secp521r1 : begin
      Result := TECPrivateKey.IsValidPublicKey(account);
      if Not Result then begin
        errors := Format('Invalid AccountKey type:%d - Length x:%d y:%d Error:%s',[account.EC_OpenSSL_NID,length(account.x),length(account.y),  ERR_error_string(ERR_get_error(),nil)]);
      end;
    end;
  else
    errors := Format('Invalid AccountKey type:%d (Unknown type) - Length x:%d y:%d',[account.EC_OpenSSL_NID,length(account.x),length(account.y)]);
    Result := False;
  end;
  if (errors='') And (Not Result) then errors := ERR_error_string(ERR_get_error(),nil);
end;

class function TAccountComp.PrivateToAccountkey(key: TECPrivateKey): TAccountKey;
begin
  Result := key.PublicKey;
end;

class function TAccountComp.RawString2Accountkey(rawaccstr: AnsiString): TAccountKey;
Var s : TMemoryStream;
begin
  Result := CT_TECDSA_Public_Nul;
  s := TMemoryStream.Create;
  try
    s.WriteBuffer(rawaccstr[1],length(rawaccstr));
    s.Position := 0;
    s.Read(Result.EC_OpenSSL_NID,SizeOf(Result.EC_OpenSSL_NID));
    TStreamOp.ReadAnsiString(s,Result.x);
    TStreamOp.ReadAnsiString(s,Result.y);
  finally
    s.Free;
  end;
end;

class function TAccountComp.TxtToMoney(Const moneytxt : AnsiString; var money : Int64) : Boolean;
Var s : AnsiString;
begin
  money := 0;
  if Trim(moneytxt)='' then begin
    Result := true;
    exit;
  end;
  try
    s := StringReplace(moneytxt,ThousandSeparator,DecimalSeparator,[rfReplaceAll]);
    money := Round( StrToFloat(s)*10000 );
    Result := true;
  Except
    result := false;
  end;
end;

class procedure TAccountComp.ValidsEC_OpenSSL_NID(list: TList);
begin
  list.Clear;
  list.Add(TObject(NID_secp256k1)); // = 714
  list.Add(TObject(NID_secp384r1)); // = 715
  list.Add(TObject(NID_sect283k1)); // = 729
  list.Add(TObject(NID_secp521r1)); // = 716
end;

{ TPCSafeBox }

function TPCSafeBox.Account(account_number: Cardinal): TAccount;
var b : Cardinal;
begin
  b := account_number DIV CT_AccountsPerBlock;
  if (b<0) Or (b>=FBlockAccountsList.Count) then raise Exception.Create('Invalid account: '+IntToStr(account_number));
  Result := PBlockAccount(FBlockAccountsList.Items[b])^.accounts[account_number MOD CT_AccountsPerBlock];
end;

function TPCSafeBox.AddNew(Const accountkey: TAccountKey; reward: UInt64; timestamp: Cardinal; compact_target: Cardinal; Const proof_of_work: AnsiString) : TBlockAccount;
var i, base_addr : Integer;
  P : PBlockAccount;
  accs : Array of cardinal;
begin
  base_addr := BlocksCount * CT_AccountsPerBlock;
  Result := CT_BlockAccount_NUL;
  Result.blockaccount := BlocksCount;
  setlength(accs,length(Result.accounts));
  for i := Low(Result.accounts) to High(Result.accounts) do begin
    Result.accounts[i] := CT_Account_NUL;
    Result.accounts[i].account := base_addr + i;
    Result.accounts[i].accountkey := accountkey;
    Result.accounts[i].updated_block := BlocksCount;
    Result.accounts[i].n_operation := 0;
    if i=0 then begin
      // Only first account wins the reward + fee
      Result.accounts[i].balance := reward + FTotalFee;
    end else begin
    end;
    accs[i] := base_addr + i;
  end;
  Result.timestamp := timestamp;
  Result.block_hash := CalcBlockHash(Result);
  New(P);
  P^ := Result;
  FBlockAccountsList.Add(P);
  FBufferBlocksHash := FBufferBlocksHash+Result.block_hash;
  Inc(FTotalBalance,reward + FTotalFee);
  Dec(FTotalFee,FTotalFee);
  AccountKeyListAddAccounts(accountkey,accs);
  // Calculating new value of safebox
  FSafeBoxHash := CalcSafeBoxHash;
end;

procedure TPCSafeBox.AccountKeyListAddAccounts(const AccountKey: TAccountKey; accounts: array of Cardinal);
Var i : Integer;
begin
  for i := 0 to FListOfOrderedAccountKeysList.count-1 do begin
    TOrderedAccountKeysList( FListOfOrderedAccountKeysList[i] ).AddAccounts(AccountKey,accounts);
  end;
end;

procedure TPCSafeBox.AccountKeyListRemoveAccount(const AccountKey: TAccountKey; accounts: array of Cardinal);
Var i : Integer;
begin
  for i := 0 to FListOfOrderedAccountKeysList.count-1 do begin
    TOrderedAccountKeysList( FListOfOrderedAccountKeysList[i] ).RemoveAccounts(AccountKey,accounts);
  end;
end;

function TPCSafeBox.AccountsCount: Integer;
begin
  Result := BlocksCount * CT_AccountsPerBlock;
end;

function TPCSafeBox.Block(block_number: Cardinal): TBlockAccount;
begin
  if (block_number<0) Or (block_number>=FBlockAccountsList.Count) then raise Exception.Create('Invalid block number: '+inttostr(block_number));
  Result := PBlockAccount(FBlockAccountsList.Items[block_number])^;
end;

class function TPCSafeBox.BlockAccountToText(const block: TBlockAccount): AnsiString;
begin
  Result := Format('Block:%d Timestamp:%d BlockHash:%s',
    [block.blockaccount, block.timestamp,
     TCrypto.ToHexaString(block.block_hash)]);
end;

function TPCSafeBox.BlocksCount: Integer;
begin
  Result := FBlockAccountsList.Count;
end;

class function TPCSafeBox.CalcBlockHash(const block : TBlockAccount): AnsiString;
Var s: AnsiString;
  ms : TMemoryStream;
  i : Integer;
begin
  ms := TMemoryStream.Create;
  try
    ms.Write(block.blockaccount,4); // Little endian
    for i := Low(block.accounts) to High(block.accounts) do begin
      ms.Write(block.accounts[i].account,4);  // Little endian
      s := TAccountComp.AccountKey2RawString(block.accounts[i].accountkey);
      ms.WriteBuffer(s[1],length(s)); // Raw bytes
      ms.Write(block.accounts[i].balance,SizeOf(Uint64));  // Little endian
      ms.Write(block.accounts[i].updated_block,4);  // Little endian
      ms.Write(block.accounts[i].n_operation,4); // Little endian
    end;
    ms.Write(block.timestamp,4); // Little endian
    Result := TCrypto.DoSha256(ms.Memory,ms.Size);
  finally
    ms.Free;
  end;
end;

function TPCSafeBox.CalcSafeBoxHash: TRawBytes;
begin
  // If No buffer to hash is because it's firts block... so use Genesis: CT_Genesis_Magic_String_For_Old_Block_Hash
  if (FBufferBlocksHash='') then Result := TCrypto.DoSha256(CT_Genesis_Magic_String_For_Old_Block_Hash)
  else Result := TCrypto.DoSha256(FBufferBlocksHash);
end;

procedure TPCSafeBox.Clear;
Var i : Integer;
  P : PBlockAccount;
begin
  for i := 0 to FBlockAccountsList.Count - 1 do begin
    P := FBlockAccountsList.Items[i];
    Dispose(P);
  end;
  FBlockAccountsList.Clear;
  For i:=0 to FListOfOrderedAccountKeysList.count-1 do begin
    TOrderedAccountKeysList( FListOfOrderedAccountKeysList[i] ).Clear(False);
  end;
  FBufferBlocksHash := '';
  FTotalBalance := 0;
  FTotalFee := 0;
  FSafeBoxHash := CalcSafeBoxHash;
end;

procedure TPCSafeBox.CopyFrom(accounts: TPCSafeBox);
Var i,j : Cardinal;
  P : PBlockAccount;
  BA : TBlockAccount;
begin
  if accounts=Self then exit;
  Clear;
  if accounts.BlocksCount>0 then begin
    for i := 0 to accounts.BlocksCount - 1 do begin
      BA := accounts.Block(i);
      New(P);
      P^ := BA;
      FBlockAccountsList.Add(P);
      for j := Low(BA.accounts) to High(BA.accounts) do begin
        AccountKeyListAddAccounts(BA.accounts[j].accountkey,[BA.accounts[j].account]);
      end;
    end;
  end;

  FTotalBalance := accounts.TotalBalance;
  FTotalFee := accounts.FTotalFee;
  FBufferBlocksHash := accounts.FBufferBlocksHash;
  FSafeBoxHash := accounts.FSafeBoxHash;
end;

constructor TPCSafeBox.Create;
begin
  InitializeCriticalSection(FLock);
  FIsLocked := false;
  FBlockAccountsList := TList.Create;
  FListOfOrderedAccountKeysList := TList.Create;
  Clear;
end;

destructor TPCSafeBox.Destroy;
Var i : Integer;
begin
  Clear;
  for i := 0 to FListOfOrderedAccountKeysList.Count - 1 do begin
    TOrderedAccountKeysList( FListOfOrderedAccountKeysList[i] ).FAccountList := Nil;
  end;
  FBlockAccountsList.Free;
  FListOfOrderedAccountKeysList.Free;
  DeleteCriticalSection(Flock);
  inherited;
end;

procedure TPCSafeBox.EndThreadSave;
begin
  if Not FIsLocked then raise Exception.Create('Is not locked');
  FIsLocked := False;
  LeaveCriticalSection(FLock);
end;

function TPCSafeBox.LoadFromStream(Stream : TStream; var LastReadBlock : TBlockAccount; var errors : AnsiString) : Boolean;
Var w : Word;
  blockscount,iblock,iacc : Cardinal;
  s : AnsiString;
  block : TBlockAccount;
  P : PBlockAccount;
  j : Integer;
begin
  Clear;
  Result := false;
  Try
    errors := 'Invalid stream';
    TStreamOp.ReadAnsiString(Stream,s);
    if (s<>CT_MagicIdentificator) then exit;
    errors := 'Invalid version or corrupted stream';
    if Stream.Size<8 then exit;
    Stream.Read(w,2);
    if w<>CT_Protocol_Version then exit;
    Stream.Read(w,2); // protocol version available, nothing to do with it
    Stream.Read(blockscount,4);
    if blockscount>(CT_NewLineSecondsAvg*2000000) then exit; // Protection for corrupted data...
    errors := 'Corrupted stream';
    for iblock := 0 to blockscount-1 do begin
      errors := 'Corrupted stream reading block '+inttostr(iblock+1)+'/'+inttostr(blockscount);
      block := CT_BlockAccount_NUL;
      if Stream.Read(block.blockaccount,4)<4 then exit;
      if (block.blockaccount<>iblock) then exit; // Invalid value
      for iacc := Low(block.accounts) to High(block.accounts) do begin
        errors := 'Corrupted stream reading account '+inttostr(iacc+1)+'/'+inttostr(length(block.accounts))+' of block '+inttostr(iblock+1)+'/'+inttostr(blockscount);
        if Stream.Read(block.accounts[iacc].account,4)<4 then exit;
        if TStreamOp.ReadAnsiString(Stream,s)<0 then exit;
        block.accounts[iacc].accountkey := TAccountComp.RawString2Accountkey(s);
        if Stream.Read(block.accounts[iacc].balance,SizeOf(UInt64))<SizeOf(UInt64) then exit;
        if Stream.Read(block.accounts[iacc].updated_block,4)<4 then exit;
        if Stream.Read(block.accounts[iacc].n_operation,4)<4 then exit;
        // check valid
        if not TAccountComp.IsValidAccountKey(block.accounts[iacc].accountkey,s) then begin
          errors := errors + ' > '+s;
          exit;
        end;
        inc(FTotalBalance,block.accounts[iacc].balance);
      end;
      errors := 'Corrupted stream reading block hash '+inttostr(iblock+1)+'/'+inttostr(blockscount);
      if Stream.Read(block.timestamp,4)<4 then exit;
      if TStreamOp.ReadAnsiString(Stream,s)<0 then exit;
      block.block_hash := s;
      // Check is valid:
      if CalcBlockHash(block)<>block.block_hash then exit;
      // Add
      New(P);
      P^ := block;
      FBlockAccountsList.Add(P);
      for j := low(block.accounts) to High(block.accounts) do begin
        AccountKeyListAddAccounts(block.accounts[j].accountkey,[block.accounts[j].account]);
      end;
      FBufferBlocksHash := FBufferBlocksHash+block.block_hash;
      LastReadBlock := block;
    end;
    Result := true;
  Finally
    if Not Result then Clear;
  End;
end;

procedure TPCSafeBox.SaveToStream(Stream: TStream);
Var
  c,iblock,iacc : Cardinal;
  b : TBlockAccount;
begin
  TStreamOp.WriteAnsiString(Stream,CT_MagicIdentificator);
  Stream.Write(CT_Protocol_Version,SizeOf(CT_Protocol_Version));
  Stream.Write(CT_Protocol_Available,SizeOf(CT_Protocol_Available));
  c := BlocksCount;
  Stream.Write(c,Sizeof(c));
  for iblock := 0 to c-1 do begin
    b := Block(iblock);
    Stream.Write(b.blockaccount,SizeOf(b.blockaccount)); // Little endian
    for iacc := Low(b.accounts) to High(b.accounts) do begin
      Stream.Write(b.accounts[iacc].account,Sizeof(b.accounts[iacc].account));
      TStreamOp.WriteAnsiString(Stream,TAccountComp.AccountKey2RawString(b.accounts[iacc].accountkey));
      Stream.Write(b.accounts[iacc].balance,Sizeof(b.accounts[iacc].balance));
      Stream.Write(b.accounts[iacc].updated_block,Sizeof(b.accounts[iacc].updated_block));
      Stream.Write(b.accounts[iacc].n_operation,Sizeof(b.accounts[iacc].n_operation));
    end;
    Stream.Write(b.timestamp,Sizeof(b.timestamp));
    TStreamOp.WriteAnsiString(Stream,b.block_hash);
  end;
end;

procedure TPCSafeBox.SetAccount(account_number : Cardinal; newAccountkey: TAccountKey; newBalance: UInt64; newN_operation: Cardinal);
Var iBlock : Cardinal;
  i,j,iAccount : Integer;
  lastbalance : UInt64;
  P : PBlockAccount;
begin
  iBlock := account_number DIV CT_AccountsPerBlock;
  iAccount := account_number MOD CT_AccountsPerBlock;
  P := FBlockAccountsList.Items[iBlock];
  if (NOT TAccountComp.Equal(P^.accounts[iAccount].accountkey,newAccountkey)) then begin
    AccountKeyListRemoveAccount(P^.accounts[iAccount].accountkey,[account_number]);
    AccountKeyListAddAccounts(newAccountkey,[account_number]);
  end;

  P^.accounts[iAccount].accountkey := newAccountkey;
  lastbalance := P^.accounts[iAccount].balance;
  P^.accounts[iAccount].balance := newBalance;
  P^.accounts[iAccount].updated_block := BlocksCount;
  P^.accounts[iAccount].n_operation := newN_operation;
  P^.block_hash := CalcBlockHash(P^);
  j := (length(P^.block_hash)*(iBlock));
  for i := 1 to length(P^.block_hash) do begin
    FBufferBlocksHash[i+j] := P^.block_hash[i];
  end;

  FTotalBalance := FTotalBalance - (Int64(lastbalance)-Int64(newBalance));
  FTotalFee := FTotalFee + (Int64(lastbalance)-Int64(newBalance));
end;

procedure TPCSafeBox.StartThreadSafe;
begin
  if FIsLocked then Begin
    TLog.NewLog(lterror,Classname,'IS LOCKED !!!');
    raise Exception.Create('IS LOCKED !!!');
  end;
  TPCThread.ProtectEnterCriticalSection(Self,FLock);
  FIsLocked := true;
end;

{ TPCSafeBoxTransaction }

function TPCSafeBoxTransaction.Account(account_number: Cardinal): TAccount;
Var i :Integer;
begin
  if FOrderedList.Find(account_number,i) then Result := PAccount(FOrderedList.FList[i])^
  else begin
    Result := FreezedSafeBox.Account(account_number);
  end;
end;

function TPCSafeBoxTransaction.CheckIntegrity: Boolean;
begin
  Result := FOldSafeBoxHash = FFreezedAccounts.FSafeBoxHash;
end;

procedure TPCSafeBoxTransaction.CleanTransaction;
begin
  FOrderedList.Clear;
  FOldSafeBoxHash := FFreezedAccounts.FSafeBoxHash;
  FTotalBalance := FFreezedAccounts.FTotalBalance;
  FTotalFee := 0;
end;

function TPCSafeBoxTransaction.Commit(accountkey: TAccountKey; reward: UInt64; timestamp: Cardinal; compact_target: Cardinal; proof_of_work: AnsiString; var errors : AnsiString) : Boolean;
Var i,j : Integer;
  B : TBlockAccount;
  Pa : PAccount;
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
      Pa := PAccount(FOrderedList.FList[i]);
      FFreezedAccounts.SetAccount(Pa^.account,
            Pa^.accountkey,
            Pa^.balance,
            Pa^.n_operation);
    end;
    //
    if (FFreezedAccounts.TotalBalance<>FTotalBalance) then begin
      TLog.NewLog(lterror,ClassName,Format('Invalid integrity balance! StrongBox:%d Transaction:%d',[FFreezedAccounts.TotalBalance,FTotalBalance]));
    end;
    if (FFreezedAccounts.FTotalFee<>FTotalFee) then begin
      TLog.NewLog(lterror,ClassName,Format('Invalid integrity fee! StrongBox:%d Transaction:%d',[FFreezedAccounts.FTotalFee,FTotalFee]));
    end;
    B := FFreezedAccounts.AddNew(accountkey,reward,timestamp,compact_target,proof_of_work);
    if (B.accounts[0].balance<>(reward + FTotalFee)) then begin
      TLog.NewLog(lterror,ClassName,Format('Invalid integrity reward! Account:%d Balance:%d  Reward:%d Fee:%d (Reward+Fee:%d)',
        [B.accounts[0].account,B.accounts[0].balance,reward,FTotalFee,reward+FTotalFee]));
    end;
    CleanTransaction;
    Result := true;
  finally
    FFreezedAccounts.EndThreadSave;
  end;
end;

procedure TPCSafeBoxTransaction.CopyFrom(transaction : TPCSafeBoxTransaction);
Var i : Integer;
  P : PAccount;
begin
  if transaction=Self then exit;
  if transaction.FFreezedAccounts<>FFreezedAccounts then raise Exception.Create('Invalid Freezed accounts to copy');
  CleanTransaction;
  for i := 0 to transaction.FOrderedList.FList.Count - 1 do begin
    P := PAccount(transaction.FOrderedList.FList[i]);
    FOrderedList.Add(P^);
  end;
  FOldSafeBoxHash := transaction.FOldSafeBoxHash;
  FTotalBalance := transaction.FTotalBalance;
  FTotalFee := transaction.FTotalFee;
end;

constructor TPCSafeBoxTransaction.Create(SafeBox : TPCSafeBox);
begin
  FOrderedList := TOrderedAccountList.Create;
  FFreezedAccounts := SafeBox;
  FOldSafeBoxHash := SafeBox.FSafeBoxHash;
  FTotalBalance := FFreezedAccounts.FTotalBalance;
  FTotalFee := 0;
end;

destructor TPCSafeBoxTransaction.Destroy;
begin
  CleanTransaction;
  FOrderedList.Free;
  inherited;
end;

function TPCSafeBoxTransaction.GetInternalAccount(account_number: Cardinal): PAccount;
Var i :Integer;
  P : PAccount;
begin
  if FOrderedList.Find(account_number,i) then Result := PAccount(FOrderedList.FList[i])
  else begin
    i := FOrderedList.Add( FreezedSafeBox.Account(account_number) );
    Result := PAccount(FOrderedList.FList[i]);
  end;
end;

function TPCSafeBoxTransaction.Modified(index: Integer): TAccount;
begin
  Result := FOrderedList.Get(index);
end;

function TPCSafeBoxTransaction.ModifiedCount: Integer;
begin
  Result := FOrderedList.Count;
end;

procedure TPCSafeBoxTransaction.Rollback;
begin
  CleanTransaction;
end;

function TPCSafeBoxTransaction.TransferAmount(sender, target, n_operation : Cardinal; amount, fee: UInt64; var errors: AnsiString): Boolean;
Var
  intSender, intTarget : Integer;
  PaccSender, PaccTarget : PAccount;
begin
  Result := false;
  errors := '';
  if not CheckIntegrity then begin
    errors := 'Invalid integrity in accounts transaction';
    exit;
  end;
  if (sender<0) Or (sender>=(FFreezedAccounts.BlocksCount*CT_AccountsPerBlock)) Or
     (target<0) Or (target>=(FFreezedAccounts.BlocksCount*CT_AccountsPerBlock)) then begin
     errors := 'Invalid sender or target on transfer';
     exit;
  end;
  if TAccountComp.IsAccountBlockedByProtocol(sender,FFreezedAccounts.BlocksCount) then begin
    errors := 'Sender account is blocked for protocol';
    Exit;
  end;
  if TAccountComp.IsAccountBlockedByProtocol(target,FFreezedAccounts.BlocksCount) then begin
    errors := 'Target account is blocked for protocol';
    Exit;
  end;
  PaccSender := GetInternalAccount(sender);
  PaccTarget := GetInternalAccount(target);
  if (PaccSender^.n_operation+1<>n_operation) then begin
    errors := 'Incorrect n_operation';
    Exit;
  end;
  if (PaccSender^.balance < (amount+fee)) then begin
    errors := 'Insuficient founds';
    Exit;
  end;
  if ((PaccTarget^.balance + amount)>CT_MaxWalletAmount) then begin
    errors := 'Max account balance';
    Exit;
  end;
  if (fee>CT_MaxTransactionFee) then begin
    errors := 'Max fee';
    Exit;
  end;
  PaccSender^.updated_block := FFreezedAccounts.BlocksCount;
  PaccTarget^.updated_block := FFreezedAccounts.BlocksCount;
  PaccSender^.n_operation := n_operation;
  PaccSender^.balance := PaccSender^.balance - (amount + fee);
  PaccTarget^.balance := PaccTarget^.balance + (amount);

  Dec(FTotalBalance,fee);
  inc(FTotalFee,fee);
  Result := true;
end;

function TPCSafeBoxTransaction.UpdateAccountkey(account_number, n_operation: Cardinal; accountkey: TAccountKey; fee: UInt64; var errors: AnsiString): Boolean;
Var intAccount : Integer;
  P : PAccount;
begin
  Result := false;
  errors := '';
  if (account_number<0) Or (account_number>=(FFreezedAccounts.BlocksCount*CT_AccountsPerBlock)) Then begin
     errors := 'Invalid account';
     exit;
  end;
  if (TAccountComp.IsAccountBlockedByProtocol(account_number,FFreezedAccounts.BlocksCount)) then begin
    errors := 'account is blocked for protocol';
    Exit;
  end;
  P := GetInternalAccount(account_number);
  if (P^.n_operation+1<>n_operation) then begin
    errors := 'Incorrect n_operation';
    Exit;
  end;
  if (P^.balance < fee) then begin
    errors := 'Insuficient founds';
    Exit;
  end;
  P^.updated_block := FFreezedAccounts.BlocksCount;
  P^.n_operation := n_operation;
  P^.accountkey := accountkey;
  Dec(P^.balance,fee);
  Dec(FTotalBalance,fee);
  Inc(FTotalFee,fee);
  Result := true;
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
  FList := TList.Create;
end;

destructor TOrderedAccountList.Destroy;
begin
  Clear;
  FList.Free;
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

{ TOrderedAccountKeysList }
Type
  TOrderedAccountKeyList = Record
    rawaccountkey : TRawBytes;
    accounts : TList;
  end;
  POrderedAccountKeyList = ^TOrderedAccountKeyList;

function SortOrdered(Item1, Item2: Pointer): Integer;
begin
   Result := Integer(Item1) - Integer(Item2);
end;

procedure TOrderedAccountKeysList.AddAccountKey(const AccountKey: TAccountKey);
Var P : POrderedAccountKeyList;
  i,j : Integer;
begin
  if Not Find(AccountKey,i) then begin
    New(P);
    P^.rawaccountkey := TAccountComp.AccountKey2RawString(AccountKey);
    P^.accounts := TList.Create;
    FOrderedAccountKeysList.Insert(i,P);
    // Search this key in the AccountsList and add all...
    j := 0;
    if Assigned(FAccountList) then begin
      For i:=0 to FAccountList.AccountsCount-1 do begin
        If TAccountComp.Equal(FAccountList.Account(i).accountkey,AccountKey) then begin
          // Note: P^.accounts will be ascending ordered due to "for i:=0 to ..."
          P^.accounts.Add(TObject(i));
        end;
      end;
      TLog.NewLog(ltdebug,Classname,Format('Adding account key (%d of %d) %s',[j,FAccountList.AccountsCount,TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(AccountKey))]));
    end else begin
      TLog.NewLog(ltdebug,Classname,Format('Adding account key (no Account List) %s',[TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(AccountKey))]));
    end;
  end;
end;

procedure TOrderedAccountKeysList.AddAccounts(const AccountKey: TAccountKey; accounts: array of Cardinal);
Var P : POrderedAccountKeyList;
  i : Integer;
begin
  if Find(AccountKey,i) then begin
    P :=  POrderedAccountKeyList(FOrderedAccountKeysList[i]);
  end else if (FAutoAddAll) then begin
    New(P);
    P^.rawaccountkey := TAccountComp.AccountKey2RawString(AccountKey);
    P^.accounts := TList.Create;
    FOrderedAccountKeysList.Insert(i,P);
  end else exit;
  for i := Low(accounts) to High(accounts) do begin
    If P^.accounts.IndexOf(TObject(accounts[i]))<0 then begin
      // Add ordered
      P^.accounts.Add(TObject(accounts[i]));
    end;
    P^.accounts.SortList(SortOrdered);
  end;
end;

procedure TOrderedAccountKeysList.Clear(RemoveAccountList : Boolean);
Var P : POrderedAccountKeyList;
  i : Integer;
begin
  for i := 0 to FOrderedAccountKeysList.Count - 1 do begin
    P := FOrderedAccountKeysList[i];
    if RemoveAccountList then begin
      P^.accounts.Free;
      Dispose(P);
    end else begin
      P^.accounts.Clear;
    end;
  end;
  if RemoveAccountList then begin
    FOrderedAccountKeysList.Clear;
  end;
end;

function TOrderedAccountKeysList.Count: Integer;
begin
  Result := FOrderedAccountKeysList.Count;
end;

constructor TOrderedAccountKeysList.Create(AccountList : TPCSafeBox; AutoAddAll : Boolean);
Var i : Integer;
begin
  TLog.NewLog(ltdebug,Classname,'Creating an Ordered Account Keys List adding all:'+CT_TRUE_FALSE[AutoAddAll]);
  FAutoAddAll := AutoAddAll;
  FAccountList := AccountList;
  FOrderedAccountKeysList := TList.Create;
  if Assigned(AccountList) then begin
    AccountList.FListOfOrderedAccountKeysList.Add(Self);
    if AutoAddAll then begin
      for i := 0 to AccountList.AccountsCount - 1 do begin
        AddAccountKey(AccountList.Account(i).accountkey);
      end;
    end;
  end;
end;

destructor TOrderedAccountKeysList.Destroy;
begin
  TLog.NewLog(ltdebug,Classname,'Destroying an Ordered Account Keys List adding all:'+CT_TRUE_FALSE[FAutoAddAll]);
  if Assigned(FAccountList) then begin
    FAccountList.FListOfOrderedAccountKeysList.Remove(Self);
  end;
  Clear(true);
  FOrderedAccountKeysList.Free;
  inherited;
end;

function TOrderedAccountKeysList.Find(const AccountKey: TAccountKey; var Index: Integer): Boolean;
var L, H, I, C: Integer;
  rak : TRawBytes;
begin
  Result := False;
  rak := TAccountComp.AccountKey2RawString(AccountKey);
  L := 0;
  H := FOrderedAccountKeysList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStr( POrderedAccountKeyList(FOrderedAccountKeysList[I]).rawaccountkey, rak );
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

function TOrderedAccountKeysList.GetAccountKey(index: Integer): TAccountKey;
Var raw : TRawBytes;
begin
  raw := POrderedAccountKeyList(FOrderedAccountKeysList[index]).rawaccountkey;
  Result := TAccountComp.RawString2Accountkey(raw);
end;

function TOrderedAccountKeysList.GetAccountKeyList(index: Integer): TList;
begin
  Result := POrderedAccountKeyList(FOrderedAccountKeysList[index]).accounts;
end;

function TOrderedAccountKeysList.IndexOfAccountKey(const AccountKey: TAccountKey): Integer;
begin
  If Not Find(AccountKey,Result) then Result := -1;
end;

procedure TOrderedAccountKeysList.RemoveAccounts(const AccountKey: TAccountKey; accounts: array of Cardinal);
Var P : POrderedAccountKeyList;
  i,j : Integer;
begin
  if Not Find(AccountKey,i) then exit; // Nothing to do
  P :=  POrderedAccountKeyList(FOrderedAccountKeysList[i]);
  for j := Low(accounts) to High(accounts) do begin
    P^.accounts.Remove(TObject(accounts[j]));
  end;
  if (P^.accounts.Count=0) And (FAutoAddAll) then begin
    // Remove from list
    FOrderedAccountKeysList.Delete(i);
    // Free it
    P^.accounts.free;
    Dispose(P);
  end;
end;

procedure TOrderedAccountKeysList.RemoveAccountKey(const AccountKey: TAccountKey);
Var P : POrderedAccountKeyList;
  i,j : Integer;
begin
  if Not Find(AccountKey,i) then exit; // Nothing to do
  P :=  POrderedAccountKeyList(FOrderedAccountKeysList[i]);
  // Remove from list
  FOrderedAccountKeysList.Delete(i);
  // Free it
  P^.accounts.free;
  Dispose(P);
end;

end.
