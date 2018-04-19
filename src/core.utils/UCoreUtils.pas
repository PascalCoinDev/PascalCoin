unit UCoreUtils;

{ Copyright (c) 2018 by PascalCoin Project

  Contains common types for Core module.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
    Herman Schoenfeld <herman@sphere10.com>: unit creator
}

{$mode delphi}

interface

uses
  Classes, SysUtils, UCrypto, UAccounts, UBlockChain, UNode, UCommon, UNetProtocol,
  Generics.Collections, Generics.Defaults, UCoreObjects;

type

{ TAccountComparer }

TAccountComparer = class (TComparer<TAccount>)
  function Compare(constref ALeft, ARight: T): Integer; override;
  class function DoCompare(constref ALeft, ARight : TAccount) : Integer; inline;
end;

{ TAccountEqualityComparer }

TAccountEqualityComparer = class(TEqualityComparer<TAccount>)
  public
    function Equals(constref ALeft, ARight: TAccount): Boolean; override;
    function GetHashCode(constref AValue: TAccount): UInt32; override;
    class function AreEqual(constref ALeft, ARight: TAccount): Boolean;
    class function CalcHashCode(constref AValue: TAccount): UInt32;
end;

{ TAccountKeyComparer }

TAccountKeyComparer = class (TComparer<TAccountKey>)
  function Compare(constref ALeft, ARight: T): Integer; override;
  class function DoCompare(constref ALeft, ARight : TAccountKey) : Integer; inline;
end;

{ TAccountKeyEqualityComparer }

TAccountKeyEqualityComparer = class(TEqualityComparer<TAccountKey>)
  public
    function Equals(constref ALeft, ARight: TAccountKey): Boolean; override;
    function GetHashCode(constref AValue: TAccountKey): UInt32; override;
    class function AreEqual(constref ALeft, ARight: TAccountKey): Boolean;
    class function CalcHashCode(constref AValue: TAccountKey): UInt32;
end;

{ TCoreTool }

TCoreTool = class
public
  class function GetSignerCandidates(ANumOps : Integer; const ACandidates : array of TAccount) : TArray<TAccount>; static;
end;

{ TSafeBoxHelper }

TSafeBoxHelper = class helper for TPCSafeBox
  private
    function GetKeysSummaryInternal(UseFilter: boolean; const AFilterKeys : array of TAccountKey; FetchAccounts : boolean = false) : TUserSummary;
  public
    function GetModifiedAccounts(const AAccounts : array of TAccount) : TArray<TAccount>;
    function GetKeySummary(const AKey : TAccountKey; FetchAccounts : boolean = false) : TKeySummary;
    function GetUserSummary(const AKeys : array of TAccountKey; FetchAccounts : boolean = false) : TUserSummary;
    function GetSummaryAllKeys : TUserSummary;
end;

{ TNodeHelper }

TNodeHelper = class helper for TNode
  function HasBestKnownBlockchainTip : boolean;
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
  UMemory, UConst;

{ TCoreTool }

class function TCoreTool.GetSignerCandidates(ANumOps : Integer; const ACandidates : array of TAccount) : TArray<TAccount>;
var
  i: Integer;
  Fee: Int64;
  acc: TAccount;
begin
  //make deep copy of accounts!!! Very Important
  Result := TArrayTool<TAccount>.Copy(ACandidates);
  Fee := ANumOps * CT_MOLINA;
  for i := High(Result) downto Low(Result) do
    begin
      acc := Result[i];
      if not (acc.Balance >= Fee) then
      begin
        TArrayTool<TAccount>.RemoveAt(Result, i);
      end;
    end;
end;

{ TNodeHelper }

function TNodeHelper.HasBestKnownBlockchainTip : boolean;
var
  LReady : boolean;
  LMsg : AnsiString;
begin
  LReady := Self.Bank.IsReady(LMsg);
  if LReady AND TNetData.NetData.IsGettingNewBlockChainFromClient then
    Result := Self.Bank.BlocksCount = TNetData.NetData.MaxRemoteOperationBlock.block;
end;

{ TSafeBoxHelper }

function TSafeBoxHelper.GetModifiedAccounts(const AAccounts : array of TAccount) : TArray<TAccount>;
var
  i : integer;
  LChanged : TList<TAccount>;
  LAcc : TAccount;
  GC : TDisposables;
begin
  LChanged := GC.AddObject( TList<TAccount>.Create ) as TList<TAccount>;
  for i := Low(AAccounts) to High(AAccounts) do begin
    LAcc := Self.Account(AAccounts[i].account);
    if (LAcc.n_Operation <> AAccounts[i].n_operation) OR (LAcc.Balance <> AAccounts[i].balance) then
      LChanged.Add(LAcc);
  end;
  Result := LChanged.ToArray;
end;

function TSafeBoxHelper.GetKeySummary(const AKey : TAccountKey; FetchAccounts : boolean = false) : TKeySummary;
var
  AKeysResult : TUserSummary;
begin
  AKeysResult := GetUserSummary([AKey], FetchAccounts);
  if Length(AKeysResult.Items) = 0 then begin
    Result := CT_KeySummary_Nil;
    exit;
  end;
  Result := AKeysResult.Items[0];
end;

function TSafeBoxHelper.GetUserSummary(const AKeys : array of TAccountKey; FetchAccounts : boolean = false) : TUserSummary;
begin
  Result := GetKeysSummaryInternal(True, AKeys, FetchAccounts);
end;

function TSafeBoxHelper.GetSummaryAllKeys : TUserSummary;
begin
  Result := GetKeysSummaryInternal(False, [], False);
end;

function TSafeBoxHelper.GetKeysSummaryInternal(UseFilter: boolean; const AFilterKeys : array of TAccountKey; FetchAccounts : boolean = false) : TUserSummary;
type
  __TList_TAccount = TList<TAccount>;
  __TPair_TAccountKey_TList_TAccount = TPair<TAccountKey, __TList_TAccount>;
  __TObjectDictionary_TAccountKey_TList_TAccount = TObjectDictionary<TAccountKey, __TList_TAccount>;
var
  i,j : integer;
  LAcc : TAccount;
  LAccs : TSortedHashSet<TAccount>;
  LKey : TAccountKey;
  LValue : __TList_TAccount;
  safeBox : TPCSafeBox;
  GC : TDisposables;
  LKeys : __TObjectDictionary_TAccountKey_TList_TAccount;
  LPair : __TPair_TAccountKey_TList_TAccount;
begin
  // Setup local dictionary key -> account[]
  LAccs := GC.AddObject( TSortedHashSet<TAccount>.Create( TAccountComparer.Create, TAccountEqualityComparer.Create ) ) as TSortedHashSet<TAccount>;
  LKeys := GC.AddObject( __TObjectDictionary_TAccountKey_TList_TAccount.Create([doOwnsValues], TAccountKeyEqualityComparer.Create )) as __TObjectDictionary_TAccountKey_TList_TAccount;

  if UseFilter then begin
    for i := Low(AFilterKeys) to High(AFilterKeys) do
      LKeys.Add(AFilterKeys[i], __TList_TAccount.Create);

    for i := 0 to Self.AccountsCount - 1 do begin
      LAcc := Self.Account(i);
      if LKeys.TryGetValue(LAcc.accountInfo.accountKey, LValue) then
        LValue.Add(LAcc);
    end
  end
  else
  for i := 0 to Self.AccountsCount - 1 do begin
    LAcc := Self.Account(i);
    if NOT LKeys.TryGetValue(LAcc.accountInfo.accountKey, LValue) then begin
      LValue := __TList_TAccount.Create;
      LKeys.Add(LAcc.accountInfo.accountKey, LValue);
     end;
     LValue.Add(LAcc);
  end;

  // Build the results
  SetLength(Result.Items, LKeys.Count);
  i := 0;
  for LPair in LKeys do begin
    Result.Items[i].Key := CT_AccountInfo_NUL.accountKey;
    Result.Items[i].TotalPASA:=0;
    Result.Items[i].TotalPASC:=0;
    SetLength(Result.Items[i].Accounts, 0);
    for j := 0 to LPair.Value.Count - 1 do begin
      LAcc := LPair.Value[j];
      Inc(Result.Items[i].TotalPASA);
      Inc(Result.Items[i].TotalPASC, LAcc.balance);
    end;
    if FetchAccounts then begin
      Result.Items[i].Accounts := LPair.Value.ToArray;
      LAccs.AddRange(Result.Items[i].Accounts);
    end;
    Inc(i);
  end;
  Result := CT_UserSummary_Nil;
  Result.Keys := TArrayTool<TAccountKey>.Copy(AFilterKeys);
  if FetchAccounts then
    Result.Accounts := LAccs.ToArray;
  Result.TotalPASA := LAccs.Count;
  for LAcc in LAccs do
    Inc(Result.TotalPASC, LAcc.Balance);
end;

{ TAccountComparer }

function TAccountComparer.Compare(constref ALeft, ARight: TAccount): Integer;
begin
  Result := TAccountComparer.DoCompare(ALeft, ARight);
end;

class function TAccountComparer.DoCompare(constref ALeft, ARight : TAccount) : Integer;
begin
  Result := TCompare.UInt64(ALeft.account, ARight.account);
end;

{ TAccountEqualityComparer }

function TAccountEqualityComparer.Equals(constref ALeft, ARight: TAccount): Boolean;
begin
  Result := TAccountEqualityComparer.AreEqual(ALeft, ARight);
end;

function TAccountEqualityComparer.GetHashCode(constref AValue: TAccount): UInt32;
begin
  Result := TAccountEqualityComparer.CalcHashCode(AValue);
end;

class function TAccountEqualityComparer.AreEqual(constref ALeft, ARight: TAccount): Boolean;
begin
  Result :=
    (ALeft.account = ARight.account) AND
    (ALeft.balance = ARight.balance) AND
    (ALeft.updated_block = ARight.updated_block) AND
    (ALeft.n_operation = ARight.n_operation) AND
    TAccountKeyEqualityComparer.AreEqual(ALeft.accountInfo.accountKey, ARight.accountInfo.accountKey);
end;

class function TAccountEqualityComparer.CalcHashCode(constref AValue: TAccount): UInt32;
begin
  Result := AValue.account;
end;

{ TAccountKeyComparer }

function TAccountKeyComparer.Compare(constref ALeft, ARight: T): Integer;
begin
  Result := TAccountKeyComparer.DoCompare(ALeft, ARight);
end;

class function TAccountKeyComparer.DoCompare(constref ALeft, ARight : TAccountKey) : Integer;
begin
  Result := BinStrComp(ALeft.x, ARight.x);
  if Result = 0 then
    Result := BinStrComp(ALeft.y, ARight.y);
end;

{ TAccountKeyEqualityComparer }

function TAccountKeyEqualityComparer.Equals(constref ALeft, ARight: TAccountKey): Boolean;
begin
  Result := TAccountKeyEqualityComparer.AreEqual(ALeft, ARight);
end;

function TAccountKeyEqualityComparer.GetHashCode(constref AValue: TAccountKey): UInt32;
begin
  Result := TAccountKeyEqualityComparer.CalcHashCode(AValue);
end;

class function TAccountKeyEqualityComparer.AreEqual(constref ALeft, ARight: TAccountKey): Boolean;
begin
  Result := TAccountKeyComparer.DoCompare(ALeft, ARight) = 0;
end;

class function TAccountKeyEqualityComparer.CalcHashCode(constref AValue: TAccountKey): UInt32;
begin
  Result := TEqualityComparer<AnsiString>.Default.GetHashCode(IntToStr(AValue.EC_OpenSSL_NID) + AValue.x + AValue.y  );
end;

{ TAccountHelper }

function TAccountHelper.GetAccountString : AnsiString;
begin
  Result := TAccountComp.AccountNumberToAccountTxtNumber(Self.account);
end;

function TAccountHelper.GetDisplayString : AnsiString;
begin
  Result := GetAccountString;
  if Self.name <> '' then
    Result := Result + ': ' + Self.name;
end;

function TAccountHelper.GetInfoText(const ABank : TPCBank) : utf8string;
var
  builder : TStrings;
  GC : TDisposables;
begin
  builder := GC.AddObject(TStringList.Create) as TStrings;
  builder.Append(Format('Account: %s %s Type:%d',[TAccountComp.AccountNumberToAccountTxtNumber(self.account), IIF(Self.name<>'','Name: '+Self.name,'') ,Self.account_type]));
   builder.Append('');
   builder.Append(Format('Current balance: %s',[TAccountComp.FormatMoney(Self.balance)]));
   builder.Append('');
   builder.Append(Format('Updated on block: %d  (%d blocks ago)',[Self.updated_block, ABank.BlocksCount-Self.updated_block]));
   builder.Append(Format('Public key type: %s',[TAccountComp.GetECInfoTxt(Self.accountInfo.accountKey.EC_OpenSSL_NID)]));
   builder.Append(Format('Base58 Public key: %s',[TAccountComp.AccountPublicKeyExport(Self.accountInfo.accountKey)]));
  if TAccountComp.IsAccountForSale(Self.accountInfo) then begin
     builder.Append('');
     builder.Append('** Account is for sale: **');
     builder.Append(Format('Price: %s',[TAccountComp.FormatMoney(Self.accountInfo.price)]));
     builder.Append(Format('Seller account (where to pay): %s',[TAccountComp.AccountNumberToAccountTxtNumber(Self.accountInfo.account_to_pay)]));
    if TAccountComp.IsAccountForSaleAcceptingTransactions(Self.accountInfo) then begin
       builder.Append('');
       builder.Append('** Private sale **');
       builder.Append(Format('New Base58 Public key: %s',[TAccountComp.AccountPublicKeyExport(Self.accountInfo.new_publicKey)]));
       builder.Append('');
      if TAccountComp.IsAccountLocked(Self.accountInfo, ABank.BlocksCount) then begin
         builder.Append(Format('PURCHASE IS SECURE UNTIL BLOCK %d (current %d, remains %d)',
          [Self.accountInfo.locked_until_block, ABank.BlocksCount,Self.accountInfo.locked_until_block - ABank.BlocksCount]));
      end else begin
         builder.Append(Format('PURCHASE IS NOT SECURE (Expired on block %d, current %d)',
          [Self.accountInfo.locked_until_block, ABank.BlocksCount]));
      end;
    end;
  end;
  Result :=  builder.Text;
end;

{ TOperationResumeHelper }

function TOperationResumeHelper.GetPrintableOPHASH : AnsiString;
begin
  Result := TCrypto.ToHexaString(Self.OperationHash);
end;

function TOperationResumeHelper.GetInfoText(const ABank : TPCBank) : utf8string;
var
  builder : TStrings;
  GC : TDisposables;
begin
  If (not Self.valid) then exit;
  builder := GC.AddObject(TStringList.Create) as TStrings;
  If Self.Block < ABank.BlocksCount then
    if (Self.NOpInsideBlock>=0) then begin
      builder.Add(Format('Block: %d/%d',[Self.Block,Self.NOpInsideBlock]))
    end else begin
      builder.Add(Format('Block: %d',[Self.Block]))
    end
  else builder.Add('** Pending operation not included on blockchain **');
  builder.Add(Format('%s',[Self.OperationTxt]));
  builder.Add(Format('OpType:%d Subtype:%d',[Self.OpType,Self.OpSubtype]));
  builder.Add(Format('Operation Hash (ophash): %s',[TCrypto.ToHexaString(Self.OperationHash)]));
  If (Self.OperationHash_OLD<>'') then begin
    builder.Add(Format('Old Operation Hash (old_ophash): %s',[TCrypto.ToHexaString(Self.OperationHash_OLD)]));
  end;
  if (Self.OriginalPayload<>'') then begin
    builder.Add(Format('Payload length:%d',[length(Self.OriginalPayload)]));
    If Self.PrintablePayload<>'' then begin
      builder.Add(Format('Payload (human): %s',[Self.PrintablePayload]));
    end;
    builder.Add(Format('Payload (Hexadecimal): %s',[TCrypto.ToHexaString(Self.OriginalPayload)]));
  end;
  If Self.Balance>=0 then begin
    builder.Add(Format('Final balance: %s',[TAccountComp.FormatMoney(Self.Balance)]));
  end;
  Result := builder.Text;
end;

{ TTimeSpanHelper }

function TTimeSpanHelper.TotalBlockCount : Integer;
begin
  Result := Round( Self.TotalSeconds / CT_NewLineSecondsAvg );
end;

end.

